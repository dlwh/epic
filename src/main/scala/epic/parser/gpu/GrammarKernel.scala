package epic.parser
package gpu

import epic.parser.SimpleRefinedGrammar
import epic.trees._
import com.nativelibs4java.opencl.{CLEvent, CLContext, CLProgram, JavaCL}
import org.bridj.Pointer._
import org.bridj.{PointerIO, Pointer}
import java.{util, lang}
import com.nativelibs4java.util.IOUtils
import scala.Array
import breeze.config.{Configuration, CommandLineParser}
import java.io.File
import collection.mutable.ArrayBuffer
import breeze.collection.mutable.TriangularArray
import com.nativelibs4java.opencl.CLMem.Usage
import breeze.linalg.SparseVector
import epic.parser.ParseChart.LogProbability

class GrammarKernel[L, L2, W](context: CLContext,
                              grammar: SimpleRefinedGrammar[L, L2, W],
                              inside: CLProgram,
                              outside: CLProgram,
                              maxCells: Int = 95000) {


  val nsyms = grammar.refinedGrammar.labelIndex.size
  val root = grammar.refinedGrammar.labelIndex(grammar.refinedGrammar.root)
  def parse(sentences: IndexedSeq[IndexedSeq[W]]):IndexedSeq[BinarizedTree[L]] = {
    val trees = new ArrayBuffer[BinarizedTree[L]]()
    val buffer = new Array[Float](maxCells * nsyms)
    val lengths = new ArrayBuffer[Int]()
    val offsets = new ArrayBuffer[Int]()
    var offset = 0
    var i = 0
    while(i < sentences.length) {

      val words = sentences(i)
      val ncells = TriangularArray.arraySize(words.length+1)
      val anch = grammar.anchor(sentences(i))

      if (offset + ncells * nsyms * 2 > buffer.length) {
        trees ++= doParse(buffer, offsets.toArray, lengths.toArray)
        util.Arrays.fill(buffer, 0.0f)
        lengths.clear()
        offsets.clear()
        offset = 0
      }

      lengths += words.length
      if(ncells * nsyms * 2 > buffer.length) throw new Exception(":(")

      for(pos <- (0 until words.length);
          aa <- grammar.lexicon.tagsForWord(words(pos));
          a = grammar.labelIndex(aa);
          ref <- anch.validLabelRefinements(pos, pos+1, a)) {
        val globalizedRefinement = grammar.refinements.labels.globalize(a, ref)
        val score = anch.scoreSpan(pos, pos + 1, a, ref)
        buffer(offset + TriangularArray.index(pos, pos+1) * nsyms * 2 + nsyms + globalizedRefinement) = math.exp(score).toFloat
      }

      offsets += offset
      offset += ncells * nsyms * 2

      i += 1
    }
    if(offset != 0)
      trees ++= doParse(buffer, offsets.toArray, lengths.toArray)

    trees.toIndexedSeq
  }

  val queue = context.createDefaultProfilingQueue()
  val binaries = inside.createKernel("inside_inner")
  val unaries = inside.createKernel("inside_unary")
  val obinaries = outside.createKernel("outside_inner")
  val ounaries = outside.createKernel("outside_unary")

  def doParse(buffer: Array[Float], offsets: Array[Int], lengths: Array[Int]):IndexedSeq[BinarizedTree[L]] = {
    val bufPtr = pointerToFloats(buffer:_*)
    val offPtr = pointerToInts(offsets:_*)
    val lenPtr = pointerToInts(lengths:_*)
    val maxLength = lengths.max
    val bufDev = context.createBuffer(Usage.InputOutput, bufPtr)
    val offDev = context.createBuffer(Usage.Input, offPtr)
    val lenDev = context.createBuffer(Usage.Input, lenPtr)
    binaries.setArg(0, bufDev)
    binaries.setArg(1, offDev)
    binaries.setArg(2, lenDev)
    unaries.setArg(0, bufDev)
    unaries.setArg(1, offDev)
    unaries.setArg(2, lenDev)
    unaries.setArg(3, 1)
    var iuCount, ibCount, ouCount, obCount = 0l
    val iu, ib, ou, ob = new ArrayBuffer[CLEvent]()
    var lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength))
    iu += lastU
    for(len <- 2 to maxLength) {
      unaries.setArg(3, len)
      binaries.setArg(3, len)
      val b = binaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), lastU)
      ib += b
      lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), b)
      iu += lastU
    }
    // outside
    // fill outsideDev with zeros...
    val outside = new Array[Float](buffer.length)
    val outsidePtr = pointerToFloats(outside:_*)
    val outsideDev = context.createBuffer(Usage.InputOutput, outsidePtr)
    obinaries.setArg(0, outsideDev)
    obinaries.setArg(1, bufDev)
    obinaries.setArg(2, offDev)
    obinaries.setArg(3, lenDev)
    ounaries.setArg(0, outsideDev)
    ounaries.setArg(1, offDev)
    ounaries.setArg(2, lenDev)
    ounaries.setArg(3, maxLength)
    lastU = ounaries.enqueueNDRange(queue, Array(offsets.length, 1), lastU)
    ou += lastU
    for(len <- (maxLength-1) to 1 by -1) {
      obinaries.setArg(4, len)
      val b = obinaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), lastU)
      ob += b
      ounaries.setArg(3, len)
      lastU = ounaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), b)
      ou += lastU
    }
    queue.finish()
    iuCount = iu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    ibCount = ib.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    ouCount = ou.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    obCount = ob.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum

    println(iuCount + " " + ibCount + " " + ouCount + " " + obCount)

    val in = bufDev.read(queue, lastU).getFloats
    val out = outsideDev.read(queue, lastU).getFloats

    val marginals = for(i <- 0 until offsets.length) yield {
      val off = offsets(i)
      val len = lengths(i)
      Marginal(in, out, off, len)
    }

//    println(marginals.map(m => breeze.numerics.logSum((0 until grammar.refinedGrammar.labelIndex.size).map(i => m.topOutsideScore(0,1,i) + m.topInsideScore(0, 1, i)))))
//        println(marginals.mkString("\n...\n"))
//    println(marginals.map(_.rootScore).mkString("\n"))

    bufPtr.release()
    offPtr.release()
    lenPtr.release()
    bufDev.release()
    offDev.release()
    lenDev.release()
    outsideDev.release()
    outsidePtr.release()
    IndexedSeq.empty
  }

  case class Marginal(inside: Array[Float], outside: Array[Float], offset: Int, length: Int) {
    def topInsideScore(begin: Int, end: Int, label: Int) =  {
      val score = inside(topIndex(begin, end, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }


    def botInsideScore(begin: Int, end: Int, label: Int) =  {
      val score = inside(botIndex(begin, end, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
//      math.log(score)
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }

    def topOutsideScore(begin: Int, end: Int, label: Int) =  {
      val score = outside(topIndex(begin, end, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
      math.log(score) - 10 * (length-(end-begin)) * math.log(2)
    }


    def botOutsideScore(begin: Int, end: Int, label: Int) =  {
      val score = outside(botIndex(begin, end, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
//      math.log(score)
      math.log(score) - 10 * (length-(end-begin)) * math.log(2)
    }

    @inline
    private def topIndex(begin: Int, end: Int, label: Int): Int = {
      offset + TriangularArray.index(begin, end) * nsyms * 2 + label
    }

    @inline
    private def botIndex(begin: Int, end: Int, label: Int): Int = topIndex(begin, end, label) + nsyms


    override def toString() = {
      val pieces = for {
        i <- 0 until length
        end <- (i+1) to length
        l <- 0 until nsyms
        ts = topOutsideScore(i, end, l)
        bs = botOutsideScore(i, end, l)
        if !ts.isInfinite || !bs.isInfinite
      } yield {
        (i,end,l) + " " + ts + " " + bs
      }

      pieces.mkString("\n")
    }

    def rootScore = topInsideScore(0, length, root)
  }
}

object GrammarKernel {
  case class Params(xxx: String = "")

  def main(args: Array[String]) {
    import ParserParams.JointParams

    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = try {
      config.readIn[JointParams[Params]]("test")
    } catch {
      case e =>
        e.printStackTrace()
        println(breeze.config.GenerateHelp[JointParams[Params]](config))
        sys.exit(1)
    }

    if(params.help) {
      println(breeze.config.GenerateHelp[JointParams[Params]](config))
      System.exit(1)
    }
    println("Training Parser...")
    println(params)
    val grammar = GenerativeParser.extractGrammar(AnnotatedLabel.TOP, params.treebank.trainTrees)

    val kern = fromSimpleGrammar(grammar)
    println("Parsing...")
    val timeIn = System.currentTimeMillis()
    val train = params.treebank.trainTrees.slice(0,1000)
    kern.parse(train.map(_.words.toIndexedSeq))
    println("Done: " + (System.currentTimeMillis() - timeIn))
    val timeX = System.currentTimeMillis()
//    val marg = train.map(_.words).map(ChartMarginal(AugmentedGrammar.fromRefined(grammar), _, ParseChart.logProb))
//    def unroll(m: ChartMarginal[ParseChart.LogProbabilityParseChart, AnnotatedLabel, String]) = {
//      for(l <- 0 until grammar.labelIndex.size; ref <- grammar.refinements.labels.localRefinements(l)) yield{
//        m.outside.top(0,1,l, ref)
//      }
//      m.partition
//    }
//    println(marg.map(m => unroll(m)).mkString("\n"))
//    println("Done: " + (System.currentTimeMillis() - timeX))
  }

  def fromSimpleGrammar[L, L2, W](grammar: SimpleRefinedGrammar[L, L2, W]) = {
    import grammar.refinedGrammar._
    val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))
    val sortedBinary: IndexedSeq[Int] = binaryRules.sortBy{r1 => (leftChild(r1), rightChild(r1), parent(r1))}(Ordering.Tuple3)
    val sortedUnary = unaryRules.sortBy(r => parent(r) -> child(r))(Ordering.Tuple2)

    val unaryRuleScores = sortedUnary.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> grammar.ruleScore(r) }
    val binaryRuleScores = sortedBinary.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> grammar.ruleScore(r) }
    val insideBinaryText = insideTemplate(labelIndex.size, binaryRuleScores, unaryRuleScores)
    val outsideBinaryText = outsideTemplate(labelIndex.size, grammar.refinedGrammar.rootIndex, binaryRuleScores, unaryRuleScores)

    val context = JavaCL.createBestContext()
//    val cpuPlatform = JavaCL.listPlatforms().filter(_.listCPUDevices(true).nonEmpty).head
//    val context = cpuPlatform.createContext(new java.util.HashMap(), cpuPlatform.listCPUDevices(true):_*)
//    println(context)

    val program = context.createProgram(insideBinaryText)
    val outside = context.createProgram(outsideBinaryText)

    val kern = new GrammarKernel(context, grammar, program, outside)

    kern
  }

  def insideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Double)]): String = {
    // TODO fma
   val individual = for( (r,score) <- rules) yield
      """top[%d] += %ff * bot[%d];""".format(r.parent, math.exp(score.toFloat), r.child)
    individual.mkString("\n    ")
  }

  def outsideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Double)]): String = {
    // TODO fma
   val individual = for( (r,score) <- rules) yield
      """bot[%d] += %ff * top[%d];""".format(r.child, math.exp(score.toFloat), r.parent)
    individual.mkString("\n    ")
  }

  def insideRuleUpdates(rules: IndexedSeq[(BinaryRule[Int], Double)]): String = {
    // TODO fma
    var lastLeft = -1
    var lastRight = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentLeftScore;"
    for((r@BinaryRule(p, l, right), score) <- rules) {
      if(lastLeft != l) {
        if(lastLeft != -1)
          sb += "}"
        sb += "currentLeftScore = left[%d];" format l
        sb += "if(currentLeftScore != 0.0) {"
        lastLeft = l
      }
      sb += """out[%d] = mad(%ff, currentLeftScore * right[%d], out[%d]);""".format(r.parent, math.exp(score), r.right, r.parent)
    }
    sb += "}"

    sb.mkString("\n    ")
  }


  // otarget is the left child, completion on right.
  def outsideRightCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Double)]): String = {
    // resort by right child, parent, left chidl
    val newrules = rules.sortBy(r => (r._1.right, r._1.parent, r._1.left))(Ordering.Tuple3)
    var lastRight = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), score) <- newrules) {
      if(lastRight != right) {
        if(lastRight != -1)
          sb += "}"
        sb += "currentCompl = gright[%d];" format right
        sb += "if(currentCompl != 0.0) {"
        lastRight = right
      }
      sb += """otarget[%d] = mad(%ff, currentCompl * oparent[%d], otarget[%d]);""".format(r.left, math.exp(score), r.parent, r.left)
    }

    sb += "}"
    sb.mkString("\n    ")
  }

  // otarget is the right child, completion on left.
  def outsideLeftCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Double)]): String = {
    var lastLeft = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), score) <- rules) {
      if(lastLeft != l) {
        if(lastLeft != -1)
          sb += "}"
        sb += "currentCompl = gleft[%d];" format l
        sb += "if(currentCompl != 0.0) {"
        lastLeft = l
      }
      sb += """otarget[%d] = mad(%ff, currentCompl * oparent[%d], otarget[%d]);""".format(r.right, math.exp(score), r.parent, r.right)
    }
    sb += "}"

    sb.mkString("\n    ")
  }

  def insideTemplate(numSyms: Int,
                     rules: IndexedSeq[(BinaryRule[Int], Double)],
                     unaries: IndexedSeq[(UnaryRule[Int], Double)]): String =
    """
#define SCALE_FACTOR 10
#define NUM_SYMS %d
#define CELL_TOP(chart, begin, end) chart + ((end) * ((end)+1)/2 + begin) * NUM_SYMS * 2
#define CELL_BOT(chart, begin, end) CELL_TOP(chart, begin, end) + NUM_SYMS
__kernel void inside_inner(__global float * charts,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  float out[NUM_SYMS], right[NUM_SYMS];
  if (end <= length) {
    __global float* chart = charts + offsets[sentence];
     for(int i = 0; i < NUM_SYMS; ++i) {
       out[i] = 0.0f;
     }
    for(int split = begin + 1; split < end; ++split) {
       __global const float * left = CELL_TOP(chart, begin, split); // scale factor of (2 ^ SCALE_FACTOR)^((split - begin) - 1)
       __global const float * gright = CELL_TOP(chart, split, end); // scale factor of (2^ SCALE_FACTOR)((end-split) - 1)
       for(int i = 0; i < NUM_SYMS; ++i) {
         right[i] = gright[i];
       }
       %s
    }
    // out has a scale factor of (2^SCALE_FACTOR)^((end-split) + (split-begin) - 2) = (2^SCALE_FACTOR)^(end-begin-2)
    // multiply in a 2^SCALE_FACTOR to reachive balance.
    __global float* gout = CELL_BOT(chart, begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout[i] = ldexp(out[i], SCALE_FACTOR);
//      gout[i] = out[i];
    }
  }
}


__kernel void inside_unary(__global float * charts,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if (end <= length) {
    __global float* chart = charts + offsets[sentence];
    __global float* top = CELL_TOP(chart, begin, end);
    __global const float* bot = CELL_BOT(chart, begin, end);
    %s
  }
}

    """.stripMargin.format(numSyms, insideRuleUpdates(rules), insideUnaryUpdates(unaries))

def outsideTemplate(numSyms: Int, root: Int,
                   rules: IndexedSeq[(BinaryRule[Int], Double)],
                   unaries: IndexedSeq[(UnaryRule[Int], Double)]): String ="""
#define SCALE_FACTOR 10
#define NUM_SYMS %d
#define CELL_TOP(chart, begin, end) (chart + ((end) * ((end)+1)/2 + begin) * NUM_SYMS * 2)
#define CELL_BOT(chart, begin, end) (CELL_TOP(chart, begin, end) + NUM_SYMS)
  __kernel void outside_unary(__global float * charts,
                __global const int* offsets,
                __global const int* lengths,
                const int spanLength) {
    const int sentence = get_global_id(0);
    const int begin = get_global_id(1);
    const int end = begin + spanLength;
    const int length = lengths[sentence];

    if(spanLength == length) {
      __global float* outside = charts + offsets[sentence];
      (CELL_TOP(outside, 0, length))[%d] = 1.0f;
    }

    if (end <= length) {
      __global float* chart = charts + offsets[sentence];
      __global const float* top = CELL_TOP(chart, begin, end);
      __global float* bot = CELL_BOT(chart, begin, end);
      %s
    }
  }

  __kernel void outside_inner(__global float* outsides,
                __global const float * insides,
                __global const int* offsets,
                __global const int* lengths,
                const int spanLength) {
    const int sentence = get_global_id(0);
    const int begin = get_global_id(1);
    const int end = begin + spanLength;
    const int length = lengths[sentence];
    float oparent[NUM_SYMS], otarget[NUM_SYMS];
    if (end <= length) {
      __global const float* inside = insides + offsets[sentence];
      __global float* outside = outsides + offsets[sentence];
      for(int i = 0; i < NUM_SYMS; ++i) {
        otarget[i] = 0.0f;
      }
      // complete looking right
      for(int completion = end+1; completion <= length; ++completion) {
         __global const float * gparent = CELL_BOT(outside, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
         __global const float * gright = CELL_TOP(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
         // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
         for(int i = 0; i < NUM_SYMS; ++i) {
           oparent[i] = gparent[i];
         }
         %s
      }

     // complete looking left
      for(int completion = 0; completion < begin; ++completion) {
         __global float * gparent = CELL_BOT(outside, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
         __global const float * gleft = CELL_TOP(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
         // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
//         for(int i = 0; i < NUM_SYMS; ++i) {
//           icompl[i] = gleft[i];
//         }
         for(int i = 0; i < NUM_SYMS; ++i) {
           oparent[i] = gparent[i];
         }
         %s
      }

      // multiply in a 2^SCALE_FACTOR to re-achieve balance.
      __global float* gout = CELL_TOP(outside, begin, end);
      for(int i = 0; i < NUM_SYMS; ++i) {
        gout[i] = ldexp(otarget[i], SCALE_FACTOR);
//        gout[i] = otarget[i];
      }
    }
  }
                                                                                              """.format(numSyms, root, outsideUnaryUpdates(unaries), outsideRightCompletionUpdates(rules), outsideLeftCompletionUpdates(rules))
}
