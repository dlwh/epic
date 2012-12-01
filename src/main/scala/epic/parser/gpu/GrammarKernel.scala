package epic.parser
package gpu

import epic.parser.SimpleRefinedGrammar
import epic.trees._
import annotations.FilterAnnotations
import annotations.{TreeAnnotator, FilterAnnotations}
import com.nativelibs4java.opencl._
import java.{util, lang}
import scala.Array
import breeze.config.{Configuration, CommandLineParser}
import java.io.{FileWriter, File}
import collection.mutable.ArrayBuffer
import breeze.collection.mutable.TriangularArray
import com.nativelibs4java.opencl.CLMem.Usage
import breeze.linalg.DenseVector
import gpu.GrammarKernel.ZeroMemory

class GrammarKernel[L, L2, W](context: CLContext,
                              grammar: SimpleRefinedGrammar[L, L2, W],
                              inside: CLProgram,
                              outside: CLProgram,
                              ecounts: CLProgram,
                              maxCells: Int = 95000,
                              maxSentences: Int = 95000/ 10) {
  val nsyms = grammar.refinedGrammar.labelIndex.size
  val nrules = grammar.refinedGrammar.labelIndex.size
  val root = grammar.refinedGrammar.labelIndex(grammar.refinedGrammar.root)
  private val queue = context.createDefaultProfilingQueue()
  private val binaries = inside.createKernel("inside_inner")
  private val unaries = inside.createKernel("inside_unary")
  private val obinaries = outside.createKernel("outside_inner")
  private val ounaries = outside.createKernel("outside_unary")
  private val ebinaries = ecounts.createKernel("binary_ecounts")
  private val eunaries = ecounts.createKernel("unary_ecounts")
  private val sumVector = context.createProgram(GrammarKernel.sumECountVectors).createKernel("sum_vectors")
  private val memZero = new ZeroMemory(context)


  private val bufDev = context.createFloatBuffer(Usage.InputOutput, maxCells * nsyms * 2)
  private val bufPtr = bufDev.allocateCompatibleMemory(context.getDevices()(0))
  private val outsideDev = context.createFloatBuffer(Usage.InputOutput, maxCells * nsyms * 2 )
  private val outsidePtr = outsideDev.allocateCompatibleMemory(context.getDevices()(0))
  private val ecountsDev = context.createFloatBuffer(Usage.InputOutput, maxCells * nrules)
  private val offDev = context.createIntBuffer(Usage.Input, maxSentences)
  private val offPtr = offDev.allocateCompatibleMemory(context.getDevices()(0))
  private val lenDev = context.createIntBuffer(Usage.Input, maxSentences)
  private val lenPtr = lenDev.allocateCompatibleMemory(context.getDevices()(0))

  private val buffer = new Array[Float](maxCells * nsyms * 2)
  private val outsideBuffer = new Array[Float](buffer.length)

  def parse(sentences: IndexedSeq[IndexedSeq[W]]):IndexedSeq[BinarizedTree[L]] = synchronized {
    {for {
      partition <- getPartitions(sentences).iterator
      (lengths, offsets) = layoutIntoMemory(partition)
      t <- doParse(offsets, lengths)
    } yield {
      t
    }}.toIndexedSeq
  }

  def expectedRuleCounts(sentences: IndexedSeq[IndexedSeq[W]]): DenseVector[Double] = synchronized {
    val allCounts = for {
      partition <- getPartitions(sentences)
    } yield {
      val (lengths,offsets) = layoutIntoMemory(partition)
      new DenseVector[Double](doExpectedCounts(offsets, lengths).map(_.toDouble))
    }

    allCounts.reduceOption{_ += _}.getOrElse(DenseVector.zeros[Double](nrules))
  }


  private def layoutIntoMemory(sentences: IndexedSeq[IndexedSeq[W]]): (Array[Int], Array[Int]) = {
    val lengths = sentences.map(_.length)
    val offsets = new ArrayBuffer[Int]()
    util.Arrays.fill(buffer, 0.0f)

    var offset = 0

    for(s <- sentences) {
      offsets += offset
      val anch = grammar.anchor(s)
      for(pos <- (0 until s.length);
          aa <- grammar.lexicon.tagsForWord(s(pos));
          a = grammar.labelIndex(aa);
          ref <- anch.validLabelRefinements(pos, pos+1, a)) {
        val globalizedRefinement = grammar.refinements.labels.globalize(a, ref)
        val score = anch.scoreSpan(pos, pos + 1, a, ref)
        buffer(cellBottom(offset, pos, pos+1, globalizedRefinement)) = math.exp(score).toFloat
      }


      offset += TriangularArray.arraySize(s.length+1) * nsyms * 2
    }
    assert(lengths.length == offsets.length)

    lengths.toArray -> offsets.toArray
  }

  private def getPartitions(sentences: IndexedSeq[IndexedSeq[W]]): IndexedSeq[IndexedSeq[IndexedSeq[W]]] = {
    val result = ArrayBuffer[IndexedSeq[IndexedSeq[W]]]()
    var current = ArrayBuffer[IndexedSeq[W]]()
    var currentCellTotal = 0
    for(s <- sentences) {
      currentCellTotal += TriangularArray.arraySize(s.length+1)
      if(currentCellTotal > maxCells) {
        assert(current.nonEmpty)
        result += current
        currentCellTotal = TriangularArray.arraySize(s.length+1)
        current = ArrayBuffer[IndexedSeq[W]]()
      }
      current += s
    }

    if(current.nonEmpty) result += current
    result
  }


  private def cellBottom(offset: Int, begin: Int, end: Int, sym: Int) = {
    offset + TriangularArray.index(begin, end) * nsyms * 2 + nsyms + sym
  }

  private def doParse(offsets: Array[Int], lengths: Array[Int]):IndexedSeq[BinarizedTree[L]] = synchronized {
    val marginals = getMarginals(buffer, offsets, lengths)

//    println(marginals.mkString("\n...\n"))
//    println(marginals.map(_.rootScore).mkString("\n"))
//    println(marginals.map(m => breeze.numerics.logSum((0 until grammar.refinedGrammar.labelIndex.size).map(i => m.topOutsideScore(0,1,i) + m.topInsideScore(0, 1, i)))))


    IndexedSeq.empty
  }

  private def doExpectedCounts(offsets: Array[Int], lengths: Array[Int]) = synchronized {
    var lastEvent = insideOutside(offsets, lengths)

    val eu, eb, r = new ArrayBuffer[CLEvent]()
    val maxLength = lengths.max
    binaries.setArg(0, ecountsDev)
    binaries.setArg(1, bufDev)
    binaries.setArg(2, outsideDev)
    binaries.setArg(3, offDev)
    binaries.setArg(4, lenDev)

    unaries.setArg(0, ecountsDev)
    unaries.setArg(1, bufDev)
    unaries.setArg(2, outsideDev)
    unaries.setArg(3, offDev)
    unaries.setArg(4, lenDev)
    for (len <- 2 to maxLength) {
      unaries.setArg(5, len)
      binaries.setArg(5, len)
      eb += ebinaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), lastEvent)
      eu += eunaries.enqueueNDRange(queue, Array(offsets.length, maxLength+1-len), lastEvent)
    }
    queue.finish()

    // reduce to a single array
    var numCellsLeft = offsets.length / nsyms / 2
    sumVector.setArg(0, ecountsDev)
    while(numCellsLeft > 1) {
      val half = numCellsLeft / 2
      numCellsLeft -= half
      sumVector.setArg(1, half * nrules) // don't go past the first half, rounded down.
      sumVector.setArg(2, numCellsLeft * nrules) // pull from the corresponding second half.
      // the reason these are different are for odd splits.
      // if there are 5 cells remaining, we want to sum the last two into the first two, and then
      // the third into the first, and then the second into the first.
      lastEvent = sumVector.enqueueNDRange(queue, Array(half * nrules), lastEvent)
      r += lastEvent
    }
    val euCount = eu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val ebCount = eb.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val rCount = r.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum

    println("ecounts: " + euCount +" " + ebCount +" " + rCount)

    ecountsDev.read(queue, 0, nrules, lastEvent).toArray
  }


  private def getMarginals(buffer: Array[Float], offsets: Array[Int], lengths: Array[Int]) =  {
    val lastEvent = insideOutside(offsets, lengths)

    val in = bufDev.read(queue, lastEvent).getFloats
    val out = outsideDev.read(queue, lastEvent).getFloats

    for (i <- 0 until offsets.length) yield {
      val off = offsets(i)
      val len = lengths(i)
      Marginal(in, out, off, len)
    }
  }


  private def insideOutside(offsets: Array[Int], lengths: Array[Int]) = {
    bufPtr.setFloats(buffer)
    offPtr.setInts(offsets)
    lenPtr.setInts(lengths)

    val wB = bufDev.write(queue, 0, buffer.length, bufPtr, false)
    val wOB = memZero.zeroMemory(queue, outsideDev)
    val wO = offDev.write(queue, 0, offsets.length, offPtr, false)
    val wL = lenDev.write(queue, 0, lengths.length, lenPtr, false)

    val maxLength = lengths.max
    binaries.setArgs(bufDev, offDev, lenDev, Integer.valueOf(1))
    unaries.setArgs(bufDev, offDev, lenDev, Integer.valueOf(1))

    val iu, ib, ou, ob = new ArrayBuffer[CLEvent]()

    var lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength), wB, wO, wL)
    iu += lastU

    for (len <- 2 to maxLength) {
      binaries.setArg(3, len)
      val b = binaries.enqueueNDRange(queue, Array(offsets.length, maxLength + 1 - len), lastU)
      ib += b

      unaries.setArg(3, len)
      lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength + 1 - len), b)
      iu += lastU
    }

    // outside
    obinaries.setArgs(outsideDev, bufDev, offDev, lenDev, Integer.valueOf(maxLength))
    ounaries.setArgs(outsideDev, offDev, lenDev, Integer.valueOf(maxLength))

    lastU = ounaries.enqueueNDRange(queue, Array(offsets.length, 1), lastU, wOB)
    ou += lastU

    for (len <- (maxLength - 1) to 1 by -1) {
      obinaries.setArg(4, len)
      val b = obinaries.enqueueNDRange(queue, Array(offsets.length, maxLength + 1 - len), lastU)
      ob += b
      ounaries.setArg(3, len)
      lastU = ounaries.enqueueNDRange(queue, Array(offsets.length, maxLength + 1 - len), b)
      ou += lastU
    }

    queue.finish()


    val iuCount = iu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val ibCount = ib.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val ouCount = ou.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val obCount = ob.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum
    val writeCounts = IndexedSeq(wB, wOB, wO, wL).map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum

    println(iuCount + " " + ibCount + " " + ouCount + " " + obCount + " " + writeCounts)
    lastU
  }

  override protected def finalize() {
    bufPtr.release()
    offPtr.release()
    lenPtr.release()
    bufDev.release()
    offDev.release()
    lenDev.release()
    outsideDev.release()
    outsidePtr.release()
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
  case class Params(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                    useGPU: Boolean = true)

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
    val annotator = FilterAnnotations[String]()
    val transformed = params.treebank.trainTrees.par.map { ti => annotator(ti) }.seq.toIndexedSeq
    val grammar = GenerativeParser.extractGrammar(AnnotatedLabel.TOP, transformed)

    val kern = fromSimpleGrammar(grammar, params.trainer.useGPU)
    println("Parsing...")
    val train = transformed.slice(0,1000)
    val timeIn = System.currentTimeMillis()
    kern.parse(train.map(_.words.toIndexedSeq))
    println("Done: " + (System.currentTimeMillis() - timeIn))
    val time2 = System.currentTimeMillis()
    val counts = kern.expectedRuleCounts(train.map(_.words.toIndexedSeq))
    val time3 = System.currentTimeMillis()
    println(counts)
    println("Done ecounts: " + (time3 - time2))
    val timeX = System.currentTimeMillis()
    val marg = train.map(_.words).par.map{
      ChartMarginal(AugmentedGrammar.fromRefined(grammar), _, ParseChart.logProb).partition}
//    def unroll(m: ChartMarginal[ParseChart.LogProbabilityParseChart, AnnotatedLabel, String]) = {
//      for(l <- 0 until grammar.labelIndex.size; ref <- grammar.refinements.labels.localRefinements(l)) yield{
//        m.outside.top(0,1,l, ref)
//      }
//      m.partition
//    }
    println("Done: " + (System.currentTimeMillis() - timeX))
//    println(marg)
  }

  def fromSimpleGrammar[L, L2, W](grammar: SimpleRefinedGrammar[L, L2, W], useGPU: Boolean = true) = {
    import grammar.refinedGrammar._
    val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))
    val sortedBinary: IndexedSeq[Int] = binaryRules.sortBy{r1 => (leftChild(r1), parent(r1), rightChild(r1))}(Ordering.Tuple3)
    val sortedUnary = unaryRules.sortBy(r => parent(r) -> child(r))(Ordering.Tuple2)

    val unaryRuleScores = sortedUnary.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> grammar.ruleScore(r) }
    val binaryRuleScores = sortedBinary.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> grammar.ruleScore(r) }
    val insideBinaryText = insideTemplate(labelIndex.size, binaryRuleScores, unaryRuleScores)
    val outsideBinaryText = outsideTemplate(labelIndex.size, grammar.refinedGrammar.rootIndex, binaryRuleScores, unaryRuleScores)
    val ecountsText = ecountsTemplate(labelIndex.size, grammar.refinedGrammar.rootIndex, (0 until index.size).map(r => indexedRule(r) -> grammar.ruleScore(r)))

    if(true) {val o = new FileWriter("inside.cl"); o.write(insideBinaryText); o.close()}
    if(true) {val o = new FileWriter("outside.cl"); o.write(outsideBinaryText); o.close()}
    if(true) {val o = new FileWriter("ecounts.cl"); o.write(ecountsText); o.close()}

    val context = if(useGPU) {
      JavaCL.createBestContext()
    } else {
      val cpuPlatform = JavaCL.listPlatforms().filter(_.listCPUDevices(true).nonEmpty).head
      cpuPlatform.createContext(new java.util.HashMap(), cpuPlatform.listCPUDevices(true):_*)
    }
    println(context)

    println(grammar.refinedGrammar.labelIndex)
    val program = context.createProgram(insideBinaryText)
    program.setFastRelaxedMath()
    program.setUnsafeMathOptimizations()
    val outside = context.createProgram(outsideBinaryText)
    outside.setUnsafeMathOptimizations()
    outside.setFastRelaxedMath()
    val ecounts = context.createProgram(ecountsText)
    ecounts.setUnsafeMathOptimizations()
    ecounts.setFastRelaxedMath()
    ecounts.createKernel("binary_ecounts")

    val kern = new GrammarKernel(context, grammar, program, outside, ecounts)

    kern
  }

  def insideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Double)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float parent;"
    val rules2 = rules.sortBy(_._1.parent)
    var lastParent = -1
    for( (r, score) <- rules2) {
      if(r.parent != lastParent) {
        if(lastParent != -1) {
          sb += """top[%d] = parent;""".format(lastParent)
        }
        sb += """parent = %ff * bot[%d];""".format(math.exp(score.toFloat), r.child)
        lastParent = r.parent
      } else {
        sb += """parent = mad(%ff, bot[%d], parent);""".format(math.exp(score.toFloat), r.child)
      }
    }
    if(lastParent != -1) {
      sb += """top[%d] = parent;""".format(lastParent)
    }
    sb.mkString("\n    ")
  }

  def outsideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Double)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float child;"
    val rules2 = rules.sortBy(_._1.child)
    var lastChild = -1
    for( (r, score) <- rules2) {
      if(r.child != lastChild) {
        if(lastChild != -1) {
          sb += """bot[%d] = child;""".format(lastChild)
        }
        sb += """child = %ff * top[%d];""".format(math.exp(score.toFloat), r.parent)
        lastChild = r.child
      } else {
        sb += """child = mad(%ff, top[%d], child);""".format(math.exp(score.toFloat), r.parent)
      }
    }
    if(lastChild != -1) {
      sb += """bot[%d] = child;""".format(lastChild)
    }
    sb.mkString("\n    ")
  }

  def insideRuleUpdates(rules: IndexedSeq[(BinaryRule[Int], Double)]): String = {
    var lastLeft = -1
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
#define CELL_TOP(chart, begin, end) (chart + ((end) * ((end)+1)/2 + begin) * NUM_SYMS * 2)
#define CELL_BOT(chart, begin, end) (CELL_TOP(chart, begin, end) + NUM_SYMS)
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
  }""".format(numSyms, root, outsideUnaryUpdates(unaries), outsideRightCompletionUpdates(rules), outsideLeftCompletionUpdates(rules))



  def ecountsTemplate(numSyms: Int, root: Int, rules: IndexedSeq[(Rule[Int], Double)]) = {
    val (binary,unary) = rules.zipWithIndex.partition(_._1._1.isInstanceOf[BinaryRule[Int]])
    val byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Double, Int)]] = binary.map{ case ((r,s),i)=> (r.asInstanceOf[BinaryRule[Int]], s, i)}.groupBy(_._1.parent)
    val uByParent: Map[Int, IndexedSeq[(UnaryRule[Int], Double, Int)]] = unary.map{ case ((r,s),i)=> (r.asInstanceOf[UnaryRule[Int]], s, i)}.groupBy(_._1.parent)
    """
#define SCALE_FACTOR 10
#define NUM_SYMS %d
#define ROOT %d
#define NUM_RULES %d
#define MAX_NUM_RULES_PER_SYMBOL %d
#define CELL_TOP(chart, begin, end) (chart + ((end) * ((end)+1)/2 + begin) * NUM_SYMS * 2)
#define CELL_BOT(chart, begin, end) (CELL_TOP(chart, begin, end) + NUM_SYMS)

__kernel void binary_ecounts(__global float* ecounts,
   __global const float * insides,
   __global const float* outsides,
   __global const int* offsets,
   __global const int* lengths,
   const int span_length
   ) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + span_length;
  const int length = lengths[sentence];
  __global float* mybuf = ecounts + offsets[sentence] / NUM_SYMS / 2 * NUM_RULES; // todo needs to take into account num_rules
  __global const float* outside = outsides + offsets[sentence];
  __global const float* inside = insides + offsets[sentence];
  const float root_score = CELL_TOP(inside, 0, length)[ROOT]; // scale is 2^(SCALE_FACTOR)^(length-1)
//    const int split = begin + get_global_id(2) + 1;
  if(end <= length) {
    float oscore;
    __global const float* oparents = CELL_BOT(outside, begin, end);
    %s



  }
}


__kernel void unary_ecounts(
              __global float* ecounts,
              __global const float * insides,
              __global const float * outsides,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if (end <= length) {
    __global const float* outside = outsides + offsets[sentence];
    __global const float* inside = insides + offsets[sentence];
    const float root_score = CELL_TOP(inside, 0, length)[ROOT]; // scale is 2^(SCALE_FACTOR)^(length-1)
    __global float* mybuf = ecounts + offsets[sentence] / NUM_SYMS / 2 * NUM_RULES;
    __global const float* top = CELL_TOP(outside, begin, end);
    __global const float* bot = CELL_BOT(inside, begin, end);
    __global const float* oparents = CELL_BOT(outside, begin, end);
    %s
  }
}
    """.format(numSyms, root, rules.length, byParent.values.map(_.size).max, ecountBinaryRules(byParent), ecountUnaries(uByParent))
  }

  private def ecountBinaryRules(byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Double, Int)]]):String = {
    val buf = new ArrayBuffer[String]()
    for((par, rules) <- byParent) {
      // oparent has scale length + begin - end, root has scale length - 1
      // left * right has scale (end - begin-2)
      // left * right * oparent / root has scale -1
//      buf += "oscore = ldexp(oparents[%d]/root_score, SCALE_FACTOR);".format(par)
//      buf += "if (oscore != 0.0) {"
//      for((BinaryRule(_, l, r), score, globalR) <- rules) {
//        buf += "    mybuf[%d] = 0.0f;".format(globalR)
//      }
//      buf += "  for(int split = begin + 1; split < end; ++split) {"
//      for((BinaryRule(_, l, r), score, globalR) <- rules) {
//        buf += "    mybuf[%d] += %ff * CELL_TOP(inside, begin, split)[%d] * CELL_TOP(inside, split, end)[%d];".format(globalR, math.exp(score).toFloat, l, r)
//      }
//      buf += "  }"
//      buf += "}"
    }
    buf.mkString("\n    ")
  }

  private def ecountUnaries(byParent: Map[Int,IndexedSeq[(UnaryRule[Int], Double, Int)]]): String = {
    val buf = new ArrayBuffer[String]()
    buf += "    float oscore;"
    for( (par, rules) <- byParent) {
      // oparent has scale length + begin - end, root has scale length - 1
      // child has scale (end - begin-1)
      // child * oparent / root has scale 0 (yay!)
      buf += "oscore = oparents[%d]/root_score;".format(par)
      for( (r,score,index) <- rules) {
        buf += "mybuf[%d] = %ff * oscore * CELL_BOT(inside, begin, end)[%d];".format(index, math.exp(score), r.child)
      }
    }

    buf.mkString("\n    ")

  }

  private val sumECountVectors =
    """
__kernel void sum_vectors(__global float* vec, const size_t maxLen, size_t pivot) {
  int trg = get_global_id(0);
  if(trg < maxLen)
    vec[trg] += vec[trg + pivot];
}
    """

  class ZeroMemory(context: CLContext) {
    val kernel = context.createProgram{
"""
__kernel void mem_zero(__global float* data, size_t len) {
  int trg = get_global_id(0);
  if(trg < len)
    data[trg] = 0.0f;
}
""".stripMargin
    }.createKernel("mem_zero")

    def zeroMemory(queue: CLQueue, data: CLBuffer[java.lang.Float]): CLEvent = {
      kernel.setArgs(data, java.lang.Long.valueOf(data.getElementSize * data.getElementCount))
      kernel.enqueueTask(queue)
    }
  }

}
