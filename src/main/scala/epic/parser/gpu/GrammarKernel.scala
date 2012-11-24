package epic.parser
package gpu

import epic.parser.SimpleRefinedGrammar
import epic.trees._
import com.nativelibs4java.opencl.{CLContext, CLProgram, JavaCL}
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

class GrammarKernel[L, L2, W](context: CLContext,
                              grammar: SimpleRefinedGrammar[L, L2, W], program: CLProgram, maxCells: Int = 40000) {


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

  val binaries = program.createKernel("inside_inner")
  val unaries = program.createKernel("inside_unary")

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
    val queue = context.createDefaultQueue()
    var lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength))
    queue.finish()
    for(len <- 2 to maxLength) {
      unaries.setArg(3, len)
      binaries.setArg(3, len)
      val b = binaries.enqueueNDRange(queue, Array(offsets.length, maxLength), lastU)
      queue.finish()
      lastU = unaries.enqueueNDRange(queue, Array(offsets.length, maxLength), b)
      queue.finish()
    }
    queue.finish()
    val out = bufDev.read(queue, lastU).getFloats

    val rootScores = for(i <- 0 until offsets.length) yield {
      val off = offsets(i)
      val len = lengths(i)
      Marginal(out, off, len)
    }

    println(rootScores.map(_.rootScore).mkString("\n"))

    bufPtr.release()
    offPtr.release()
    lenPtr.release()
    bufDev.release()
    offDev.release()
    lenDev.release()
    IndexedSeq.empty
  }

  case class Marginal(inside: Array[Float], offset: Int, length: Int) {
    def topScore(begin: Int, end: Int, label: Int) =  {
      val score = inside(offset + TriangularArray.index(begin, end) * nsyms * 2 + label)
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }

    def botScore(begin: Int, end: Int, label: Int) =  {
      val score = inside(offset + TriangularArray.index(begin, end) * nsyms * 2 + nsyms + label)
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
//      math.log(score)
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }

    def rootScore = topScore(0, length, root)

    override def toString = TriangularArray.tabulate(length){ (beg, end) => (beg, end, {val s = SparseVector.tabulate(nsyms)(topScore(beg, end, _)); s.compact(); s}, {val x = SparseVector.tabulate(nsyms)(botScore(beg, end, _)); x.compact(); x})}.toString
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
    val train = params.treebank.trainTrees.slice(0,100)
    kern.parse(train.map(_.words.toIndexedSeq))
    println("Done: " + (System.currentTimeMillis() - timeIn))
    val timeX = System.currentTimeMillis()
    val marg = train.map(_.words).map(ChartMarginal(AugmentedGrammar.fromRefined(grammar), _, ParseChart.logProb).partition)
    println(marg.mkString("\n"))
    println("Done: " + (System.currentTimeMillis() - timeX))
  }

  def fromSimpleGrammar[L, L2, W](grammar: SimpleRefinedGrammar[L, L2, W]) = {
    import grammar.refinedGrammar._
    val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))
    val sortedBinary: IndexedSeq[Int] = binaryRules.sortBy{r1 => (leftChild(r1), rightChild(r1), parent(r1))}(Ordering.Tuple3)
    val sortedUnary = unaryRules.sortBy(r => parent(r) -> child(r))(Ordering.Tuple2)

    val unaryRuleScores = sortedUnary.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> grammar.ruleScore(r) }
    val binaryRuleScores = sortedBinary.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> grammar.ruleScore(r) }
    val insideBinaryText = insideTemplate(labelIndex.size, binaryRuleScores, unaryRuleScores)

    val context = JavaCL.createBestContext()

    val program = context.createProgram(insideBinaryText)

    val kern = new GrammarKernel(context, grammar, program)

    kern
  }

  def insideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Double)]): String = {
    // TODO fma
   val individual = for( (r,score) <- rules) yield
      """top[%d] += %ff * bot[%d]; """.format(r.parent, math.exp(score.toFloat), r.child)
    individual.mkString("\n    ")
  }

  def insideRuleUpdates(rules: IndexedSeq[(BinaryRule[Int], Double)]): String = {
    // TODO fma
   val individual = for( (r,score) <- rules) yield
      """out[%d] += %ff * left[%d] * right[%d];""".format(r.parent, math.exp(score.toFloat), r.left, r.right)
    individual.mkString("\n      ")
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
  float out[NUM_SYMS], left[NUM_SYMS], right[NUM_SYMS];
  if (end <= length) {
    __global float* chart = charts + offsets[sentence];
     for(int i = 0; i < NUM_SYMS; ++i) {
       out[i] = 0.0f;
     }
    for(int split = begin + 1; split < end; ++split) {
       __global const float * gleft = CELL_TOP(chart, begin, split); // scale factor of (2 ^ SCALE_FACTOR)^((split - begin) - 1)
       __global const float * gright = CELL_TOP(chart, split, end); // scale factor of (2^ SCALE_FACTOR)((end-split) - 1)
       for(int i = 0; i < NUM_SYMS; ++i) {
         left[i] = gleft[i];
       }
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
}
