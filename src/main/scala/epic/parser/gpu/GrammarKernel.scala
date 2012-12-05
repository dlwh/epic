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
import breeze.linalg.{Counter, DenseVector}
import gpu.GrammarKernel.ZeroMemory
import org.bridj.Pointer
import breeze.util.{Index, Encoder}
import collection.{immutable, mutable}
import java.nio.{FloatBuffer, ByteBuffer}
import projections.ProjectionIndexer

class GrammarKernel[L, W](context: CLContext,
                          grammar: BaseGrammar[L],
                          lexicon: Lexicon[L, W],
                          private var _ruleScores: Array[RuleScores],
                          tagScores: Array[(IndexedSeq[W], Int, Int)=>Double],
                          inside: CLProgram,
                          outside: CLProgram,
                          ecounts: CLProgram,
                          copyPosTags: CLProgram,
                          maxSentences: Int = 1000,
                          maxTotalLength: Int = 10000) {
  def ruleScores = _ruleScores

  val numGrammars = ruleScores.length
  val nsyms = grammar.labelIndex.size
  val nrules = grammar.index.size
  val nbinaries = ruleScores.head.binaries.length
  val nunaries = ruleScores.head.unaries.length
  val root = grammar.labelIndex(grammar.root)
  val totalRules: Int = nbinaries * numGrammars + nunaries * numGrammars
  val cellSize = nsyms * numGrammars
  val maxCells =  ((context.getMaxMemAllocSize / math.max(cellSize, totalRules)).toInt / 4) min (100000)

  private val queue = context.createDefaultProfilingQueue()
  private val binaries = inside.createKernel("inside_binaries")
  private val unaries = inside.createKernel("inside_unaries")
  private val obinaries = outside.createKernel("outside_binaries")
  private val ounaries = outside.createKernel("outside_unaries")
  private val ebinaries = ecounts.createKernel("ecount_binaries")
  private val eunaries = ecounts.createKernel("ecount_unaries")
  private val eterms = ecounts.createKernel("ecount_terminals")
  private val copyTags = copyPosTags.createKernel("copy_pos_to_charts")
  private val sumVector = context.createProgram(GrammarKernel.sumECountVectors).createKernel("sum_vectors")
  private val memZero = new ZeroMemory(context)

  private val insideTopDev, insideBotDev = context.createFloatBuffer(Usage.InputOutput, maxCells * cellSize)
  private val outsideTopDev, outsideBotDev = context.createFloatBuffer(Usage.InputOutput, maxCells * cellSize)
  private val ecountsDev = context.createFloatBuffer(Usage.InputOutput, maxCells * totalRules)
  private val termECountsDev = context.createFloatBuffer(Usage.InputOutput, maxTotalLength * cellSize / 2)
  private val offDev = context.createIntBuffer(Usage.Input, maxSentences)
  private val offPtr = offDev.allocateCompatibleMemory(context.getDevices()(0))
  private val offLengthsDev = context.createIntBuffer(Usage.Input, maxSentences)
  private val offLengthsPtr = offLengthsDev.allocateCompatibleMemory(context.getDevices()(0))
  private val lenDev = context.createIntBuffer(Usage.Input, maxSentences)
  private val lenPtr = lenDev.allocateCompatibleMemory(context.getDevices()(0))
  private val ruleVector = Pointer.allocateFloats(totalRules)

  private val posTagsDev = context.createFloatBuffer(Usage.InputOutput, maxTotalLength * cellSize / 2)
  private val posTagsPtr = posTagsDev.allocateCompatibleMemory(context.getDevices()(0))

  private val rulesDev = context.createFloatBuffer(Usage.Input, totalRules)
  ruleScores = _ruleScores

  def ruleScores_=(newRules: Array[RuleScores]) {
    _ruleScores = newRules
    val arr = new Array[Float](rulesDev.getElementCount.toInt)
    for(g <- 0 until numGrammars) {
      for(b <- 0 until ruleScores(g).binaries.length) {
        arr(b * numGrammars + g) = math.exp(ruleScores(g).binaries(b)).toFloat
      }
      for(u <- 0 until ruleScores(g).unaries.length) {
        arr(nbinaries * numGrammars + u * numGrammars + g) = math.exp(ruleScores(g).unaries(u)).toFloat
      }
    }
    val pointer = Pointer.pointerToFloats(arr:_*)
    rulesDev.write(queue, pointer, true)
    pointer.release()
  }


  def parse(sentences: IndexedSeq[IndexedSeq[W]]):IndexedSeq[BinarizedTree[L]] = synchronized {
    {for {
      partition <- getPartitions(sentences).iterator
      batch = layoutIntoMemory(partition)
      t <- doParse(batch)
    } yield {
      t
    }}.toIndexedSeq
  }

  def expectedRuleCounts(sentences: IndexedSeq[IndexedSeq[W]]): (DenseVector[Double], Array[Array[Counter[W, Double]]]) = synchronized {
    val allCounts = for {
      partition <- getPartitions(sentences).iterator
    } yield {
      val batch = layoutIntoMemory(partition)
      val (counts: Array[Float], wordCounts: Array[Array[Counter[W, Double]]]) = doExpectedCounts(batch)
      val r = new DenseVector[Double](counts.map(_.toDouble))
      r -> wordCounts
    }

    def sumCounts(words: Array[Array[Counter[W, Double]]], words2: Array[Array[Counter[W, Double]]]) {
      for( (a,b) <- words zip words2; (a2,b2) <- a zip b) a2 += b2
    }

    allCounts.reduceOption{ (c1, c2) => c1._1 += c2._1; sumCounts(c1._2, c2._2); c1}.getOrElse(DenseVector.zeros[Double](totalRules) -> Array.fill(numGrammars, nsyms)(Counter[W, Double]()))
  }

  case class Batch(lengths: Array[Int],
                   offsets: Array[Int],
                   lengthTotals: Array[Int],
                   totalLength: Int,
                   sentences: IndexedSeq[IndexedSeq[W]],
                   posTags: Array[Float]) {

  }


  private def layoutIntoMemory(sentences: IndexedSeq[IndexedSeq[W]]): Batch = {
    val lengths = sentences.map(_.length)
    val offsets = new ArrayBuffer[Int]()

    var offset = 0

    val partialLengths = new Array[Int](lengths.size)
    var totalLength = 0
    var i = 0
    while(i < partialLengths.length) {
      partialLengths(i) = totalLength
      totalLength += lengths(i)
      i += 1
    }

    val posTags = new Array[Float](totalLength * nsyms * numGrammars)

    for( (s, i) <- sentences.zipWithIndex) {
      offsets += offset
      for(pos <- (0 until s.length);
          aa <- lexicon.tagsForWord(s(pos));
          a = grammar.labelIndex(aa)) {
        for(g <- 0 until numGrammars)
          posTags(((partialLengths(i) + pos) * nsyms +  a)*numGrammars + g) = math.exp(tagScores(g)(s, pos, a)).toFloat
      }


      offset += TriangularArray.arraySize(s.length+1)
    }
    offsets += offset

    Batch(lengths.toArray, offsets.toArray, partialLengths, totalLength, sentences, posTags)
  }

  private def getPartitions(sentences: IndexedSeq[IndexedSeq[W]]): IndexedSeq[IndexedSeq[IndexedSeq[W]]] = {
    val result = ArrayBuffer[IndexedSeq[IndexedSeq[W]]]()
    var current = ArrayBuffer[IndexedSeq[W]]()
    var currentCellTotal = 0
    var currentLengthTotal = 0
    for(s <- sentences) {
      currentCellTotal += TriangularArray.arraySize(s.length+1)
      currentLengthTotal += s.length
      if(currentCellTotal > maxCells || current.size >= maxSentences || currentLengthTotal > maxTotalLength) {
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


  private def cellBottom(offset: Int, begin: Int, end: Int, grammar: Int, sym: Int) = {
    ((offset + TriangularArray.index(begin, end)) * nsyms * 2 + nsyms + sym) * numGrammars + grammar
  }

  private def doParse(batch: Batch):IndexedSeq[BinarizedTree[L]] = synchronized {
    val marginals = getMarginals(batch)

//    println(marginals.mkString("\n...\n"))
//    println(marginals.map(_.rootScore(0)).mkString("\n"))
//    println(marginals.map(m => breeze.numerics.logSum((0 until grammar.refinedGrammar.labelIndex.size).map(i => m.topOutsideScore(0,1,i) + m.topInsideScore(0, 1, i)))))


    IndexedSeq.empty
  }

  private def doExpectedCounts(batch: Batch) = synchronized {
    import batch._
    val wOB = memZero.zeroMemory(queue, ecountsDev)
    val wterm = memZero.zeroMemory(queue, termECountsDev)
    var lastEvent = insideOutside(batch)
//    var lastEvent = wOB


    offLengthsPtr.setInts(lengthTotals)
    val copyOffLengths = offLengthsDev.write(queue, offLengthsPtr, false)

    val eu, eb, r = new ArrayBuffer[CLEvent]()

    val maxLength = lengths.max
    ebinaries.setArgs(ecountsDev, insideTopDev, outsideBotDev, offDev, lenDev, Integer.valueOf(1), rulesDev)
    eunaries.setArgs(ecountsDev, insideTopDev, insideBotDev, outsideTopDev, offDev, lenDev, Integer.valueOf(1), rulesDev)
    eterms.setArgs(termECountsDev, insideBotDev, outsideBotDev, offDev, lenDev, offLengthsDev, Integer.valueOf(1))
    val maxDim1Size = queue.getDevice.getMaxWorkItemSizes()(0)
    if(maxDim1Size < nsyms * numGrammars) {
      eterms.setArg(6, numGrammars / 8 + 1)
    }


    val gramMultiplier = if(maxDim1Size < nsyms * numGrammars) {
      8
    } else {
      numGrammars
    }
    val termFinished =  eterms.enqueueNDRange(queue, Array(nsyms * gramMultiplier, lengths.length, maxLength), Array(nsyms * gramMultiplier, 1, 1), lastEvent, wterm, copyOffLengths)
    for (len <- 2 to maxLength) {
      eunaries.setArg(6, len)
      ebinaries.setArg(6, len)
      eb += ebinaries.enqueueNDRange(queue, Array(lengths.length, maxLength+1-len, numGrammars), Array(1, 1, numGrammars), lastEvent, wOB)
      eu += eunaries.enqueueNDRange(queue, Array(lengths.length, maxLength+1-len, numGrammars), Array(1, 1, numGrammars), lastEvent, wOB)
    }
    eunaries.setArg(6, 1)
    eu += eunaries.enqueueNDRange(queue, Array(lengths.length, maxLength, numGrammars), Array(1, 1, numGrammars), lastEvent, wOB)

    val termVector = Pointer.allocateFloats(totalLength * cellSize / 2)
    val termOut = termECountsDev.read(queue, termVector, true, termFinished)
    val floats = termVector.getFloats
    termVector.release()
    val wordEcounts = tallyTermExpectedCounts(floats, sentences, lengthTotals)


    queue.finish()
//    println("sum..." + ecountsDev.read(queue).getFloats.sum)

//    reduce to a single array
    lastEvent = collapseArray(ecountsDev, offsets.last, totalRules, lastEvent)

    queue.finish()
    val euCount = eu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val ebCount = eb.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val wobCount = (wOB.getProfilingCommandEnd - wOB.getProfilingCommandStart) / 1E9
//    val termCount = (termOut.getProfilingCommandEnd - termOut.getProfilingCommandStart) / 1E9

    println("ecounts: " + euCount +" " + ebCount +" " + wobCount)

    ecountsDev.read(queue, 0, totalRules, ruleVector, true,  lastEvent)
    val arr = ruleVector.getFloats
    arr -> wordEcounts
  }

  private def tallyTermExpectedCounts(counts: Array[Float], words: IndexedSeq[IndexedSeq[W]], offsets: Array[Int]) = {
    val r = Array.fill(numGrammars, nsyms)(Counter[W, Double]())
    for(s <- 0 until words.length; g <- 0 until numGrammars; i <- 0 until words(s).length; sym <- 0 until nsyms) {
      val count = counts( ((offsets(s) + i) * nsyms + sym)*numGrammars + g)
      if(count != 0) {
        r(g)(sym)(words(s)(i)) += count.toDouble
      }
    }

    r
  }

  private def collapseArray(v: CLBuffer[lang.Float], len: Int, width: Int, toAwait: CLEvent) = {
    var lastEvent = toAwait
    var numCellsLeft = len
    while(numCellsLeft > 1) {
      val half = numCellsLeft / 2
      numCellsLeft -= half
      sumVector.setArg(0, v)
      sumVector.setArg(1, half * width) // don't go past the first half, rounded down.
      sumVector.setArg(2, numCellsLeft * width) // pull from the corresponding second half.
      // the reason these are different are for odd splits.
      // if there are 5 cells remaining, we want to sum the last two into the first two, and then
      // the third into the first, and then the second into the first.
      lastEvent = sumVector.enqueueNDRange(queue, Array(half * width), lastEvent)
    }
    lastEvent
  }


  private def getMarginals(batch: Batch) =  {
    val lastEvent = insideOutside(batch)
    import batch._

    val inM = insideTopDev.read(queue, lastEvent)
    val outM = outsideTopDev.read(queue, lastEvent)
    val inBM = insideBotDev.read(queue, lastEvent)
    val outBM = outsideBotDev.read(queue, lastEvent)
    val inTop = inM.getFloats
    val outTop = outM.getFloats
    val inBot = inBM.getFloats
    val outBot = outBM.getFloats
    inM.release()
    outM.release()
    inBM.release()
    outBM.release()

    for (i <- 0 until lengths.length) yield {
      val off = offsets(i)
      val len = lengths(i)
      Marginal(inTop, inBot, outTop, inBot, off, len)
    }
  }


  private def insideOutside(batch: Batch) = synchronized {
    import batch._
    val maxLength = lengths.max
    posTagsPtr.setFloats(batch.posTags)
    offPtr.setInts(offsets)
    lenPtr.setInts(lengths)

//    println(posTags.slice(batch.lengthTotals(1) * nsyms, batch.totalLength * nsyms).mkString(", "))

    val wPos = posTagsDev.write(queue, 0, batch.posTags.length, posTagsPtr, false)
    val wIB = memZero.zeroMemory(queue, insideBotDev)
    val wIT = memZero.zeroMemory(queue, insideTopDev)
    val wOT = memZero.zeroMemory(queue, outsideTopDev)
    val wOB = memZero.zeroMemory(queue, outsideBotDev)
    val wO = offDev.write(queue, 0, lengths.length, offPtr, false)
    val wL = lenDev.write(queue, 0, lengths.length, lenPtr, false)
    offLengthsPtr.setInts(lengthTotals)
    val copyOffLengths = offLengthsDev.write(queue, offLengthsPtr, false)

    copyTags.setArgs(posTagsDev, insideBotDev, offDev, lenDev, offLengthsDev, Integer.valueOf(1))
    val maxDim1Size = queue.getDevice.getMaxWorkItemSizes()(0)
    if(maxDim1Size < nsyms * numGrammars) {
      copyTags.setArg(5, numGrammars / 8 + 1)
    }
    val gramMultiplier = if(maxDim1Size < nsyms * numGrammars) {
      8
    } else {
      numGrammars
    }
    queue.finish()
    val initCharts = copyTags.enqueueNDRange(queue,  Array(nsyms * gramMultiplier, lengths.length, maxLength), Array(nsyms * gramMultiplier, 1, 1), wPos, wIB, wL, copyOffLengths)
    queue.finish()


    binaries.setArgs(insideBotDev, insideTopDev, offDev, lenDev, Integer.valueOf(1), rulesDev)
    unaries.setArgs(insideBotDev, insideTopDev, offDev, lenDev, Integer.valueOf(1), rulesDev)

    val iu, ib, ou, ob = new ArrayBuffer[CLEvent]()

    var lastU = unaries.enqueueNDRange(queue, Array(lengths.length, maxLength, numGrammars), Array(1, 1, numGrammars), initCharts, wIT)
    iu += lastU

    // TODO: retrofit inside/outside binaries and unaries to look at posTagsPointer....
    // TODO: also get ecounts...
    for (len <- 2 to maxLength) {
      binaries.setArg(4, len)
      val b = binaries.enqueueNDRange(queue, Array(lengths.length, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      ib += b

      unaries.setArg(4, len)
      lastU = unaries.enqueueNDRange(queue, Array(lengths.length, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b)
      iu += lastU
    }

    // outside
    obinaries.setArgs(outsideBotDev, insideTopDev, offDev, lenDev, Integer.valueOf(maxLength), rulesDev)
    ounaries.setArgs(outsideTopDev, outsideBotDev, offDev, lenDev, Integer.valueOf(maxLength), rulesDev)

    lastU = ounaries.enqueueNDRange(queue, Array(lengths.length, 1, numGrammars), Array(1, 1, numGrammars), lastU, wOB, wOT)
    ou += lastU

    for (len <- (maxLength - 1) to 1 by -1) {
      obinaries.setArg(4, len)
      val b = obinaries.enqueueNDRange(queue, Array(lengths.length, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), lastU)
      ob += b
      ounaries.setArg(3, len)
      lastU = ounaries.enqueueNDRange(queue, Array(lengths.length, maxLength + 1 - len, numGrammars), Array(1, 1, numGrammars), b)
      ou += lastU
    }

    queue.finish()


    val iuCount = iu.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val ibCount = ib.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val ouCount = ou.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val obCount = ob.map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9
    val writeCounts = IndexedSeq(wIT, wOT, wIB, wOB, wO, wL, initCharts).map(e => e.getProfilingCommandEnd - e.getProfilingCommandStart).sum / 1E9

    println(iuCount + " " + ibCount + " " + ouCount + " " + obCount + " " + writeCounts)
    lastU
  }

  override protected def finalize() {
    offPtr.release()
    lenPtr.release()
    offDev.release()
    lenDev.release()
    outsideTopDev.release()
    insideTopDev.release()
    outsideBotDev.release()
    insideBotDev.release()
  }

  case class Marginal(inTop: Array[Float], inBot: Array[Float], outTop: Array[Float], outBot: Array[Float], offset: Int, length: Int) {
    def topInsideScore(begin: Int, end: Int, grammar: Int, label: Int) =  {
      val score = inTop(index(begin, end, grammar, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }


    def botInsideScore(begin: Int, end: Int, grammar: Int, label: Int) =  {
      val score = inBot(index(begin, end, grammar, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
//      math.log(score)
      math.log(score) - 10 * ((end-begin)-1) * math.log(2)
    }

    def topOutsideScore(begin: Int, end: Int, grammar: Int, label: Int) =  {
      val score = outTop(index(begin, end, grammar, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
      math.log(score) - 10 * (length-(end-begin)) * math.log(2)
    }


    def botOutsideScore(begin: Int, end: Int, grammar: Int, label: Int) =  {
      val score = outBot(index(begin, end, grammar, label))
//      java.lang.Math.scalb(score, -10 * ((end-begin)-1))
//      math.log(score)
      math.log(score) - 10 * (length-(end-begin)) * math.log(2)
    }

    @inline
    private def index(begin: Int, end: Int, grammar: Int, label: Int): Int = {
      ((offset + TriangularArray.index(begin, end)) * nsyms + label) * numGrammars + grammar
    }

    override def toString() = {
      val pieces = for {
        i <- 0 until length
        end <- (i+1) to length
        l <- 0 until nsyms
        ts = topOutsideScore(i, end, 0, l)
        bs = botOutsideScore(i, end, 0, l)
        if !ts.isInfinite || !bs.isInfinite
      } yield {
        (i,end,l) + " " + ts + " " + bs
      }

      pieces.mkString("\n")
    }

    def rootScore(grammar: Int) = topInsideScore(0, length, grammar, root)
  }
}

object GrammarKernel {
  case class Params(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                    useGPU: Boolean = true, numToParse: Int = 1000, numGrammars: Int = 1)

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
    import params._
    import params.trainer._
    println("Training Parser...")
    println(params)
    val annotator = FilterAnnotations[String]()
    val transformed = params.treebank.trainTrees.par.map { ti => annotator(ti) }.seq.toIndexedSeq
    val grammar = GenerativeParser.extractGrammar(AnnotatedLabel.TOP, transformed)

    val kern = fromSimpleGrammar(grammar, params.trainer.useGPU, numGrammars)
    println("Parsing...")
    val train = transformed.slice(0,numToParse)
    val timeIn = System.currentTimeMillis()
    kern.parse(train.map(_.words.toIndexedSeq))
    println("Done: " + (System.currentTimeMillis() - timeIn))
    println("ecounts...")
    val time2 = System.currentTimeMillis()
    val counts = kern.expectedRuleCounts(train.map(_.words.toIndexedSeq))
    val time3 = System.currentTimeMillis()
    println(counts._1.sum)
    println(counts._2.map(_.map(_.sum).sum).sum)
//    println(Encoder.fromIndex(grammar.refinedGrammar.index).decode(counts))
    println("Done ecounts: " + (time3 - time2))
    val timeX = System.currentTimeMillis()
    val feat = new ProductionFeaturizer(grammar.grammar, grammar.lexicon.knownLexicalProductions)
    val marg = train.map(_.words).foldLeft(DenseVector.zeros[Double](feat.index.size)){ (acc, s) =>
      val m = ChartMarginal(AugmentedGrammar.fromRefined(grammar), s, ParseChart.logProb)
      val counts = m.expectedCounts(feat).counts
//      println(m.partition)
      acc += counts
      acc
    }
    println("Done: " + (System.currentTimeMillis() - timeX))
    println(marg.slice(0, grammar.grammar.index.size).sum)
    println(marg.slice(grammar.grammar.index.size, marg.length).sum)

//    println(Encoder.fromIndex(grammar.grammar.index).decode(marg.slice(0, grammar.grammar.index.size)))
//    def unroll(m: ChartMarginal[ParseChart.LogProbabilityParseChart, AnnotatedLabel, String]) = {
//      for(l <- 0 until grammar.labelIndex.size; ref <- grammar.refinements.labels.localRefinements(l)) yield{
//        m.outside.top(0,1,l, ref)
//      }
//      m.partition
//    }
//    println(marg)
  }

  def fromSimpleGrammar[L, L2, W](grammar: SimpleRefinedGrammar[L, L2, W], useGPU: Boolean = true, numGrammars: Int = 1) = {
    import grammar.refinedGrammar._
    val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))
    val sortedBinary: IndexedSeq[Int] = binaryRules.sortBy{r1 => (leftChild(r1), parent(r1), rightChild(r1))}(Ordering.Tuple3)
    val sortedUnary = unaryRules.sortBy(r => parent(r) -> child(r))(Ordering.Tuple2)

    val unaryRuleScores = sortedUnary.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> (r-binaryRules.length)}
    val binaryRuleScores = sortedBinary.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> r }
    val insideBinaryText = insideTemplate(binaryRuleScores, unaryRuleScores)
    val outsideBinaryText = outsideTemplate(binaryRuleScores, unaryRuleScores)
    val ecountsText = ecountsTemplate(binaryRuleScores, unaryRuleScores)

    val headerText = header(labelIndex.size, grammar.refinedGrammar.rootIndex, binaryRuleScores, unaryRuleScores, numGrammars)

    if(true) {val o = new FileWriter("inside.cl"); o.write(headerText); o.write(insideBinaryText); o.close()}
    if(true) {val o = new FileWriter("outside.cl"); o.write(headerText); o.write(outsideBinaryText); o.close()}
    if(true) {val o = new FileWriter("ecounts.cl"); o.write(headerText); o.write(ecountsText); o.close()}

    val context = if(useGPU) {
      JavaCL.createBestContext()
    } else {
      val cpuPlatform = JavaCL.listPlatforms().filter(_.listCPUDevices(true).nonEmpty).head
      cpuPlatform.createContext(new java.util.HashMap(), cpuPlatform.listCPUDevices(true):_*)
    }
    println(context)

    println(grammar.refinedGrammar.labelIndex)
    val program = context.createProgram(headerText,insideBinaryText)
    program.setFastRelaxedMath()
    program.setUnsafeMathOptimizations()
    val outside = context.createProgram(headerText, outsideBinaryText)
    outside.setUnsafeMathOptimizations()
    outside.setFastRelaxedMath()
    val ecounts = context.createProgram(headerText, ecountsText)
    ecounts.setUnsafeMathOptimizations()
    ecounts.setFastRelaxedMath()

    val copyPos = context.createProgram(headerText, copyPosToCharts)
    copyPos.setUnsafeMathOptimizations()
    copyPos.setFastRelaxedMath()

    val rscores = RuleScores.fromRefinedGrammar(grammar)
    val grammars = new Array[RuleScores](numGrammars)
    util.Arrays.fill(grammars.asInstanceOf[Array[AnyRef]], rscores)
    // segfaults java. your guess is as good as mine.
//    val grammars2 = Array.fill(numGrammars)(RuleScores.fromRefinedGrammar(grammar, numBits))
    val scorers = Array.fill(numGrammars){ (w: IndexedSeq[W], pos: Int, label: Int) =>
      grammar.anchor(w).scoreSpan(pos, pos+1, label, 0)
    }

    val kern = new GrammarKernel(context, grammar.grammar, grammar.lexicon, grammars, scorers, program, outside, ecounts, copyPos)

    kern
  }



  private def header(numSyms: Int, root: Int, binary: IndexedSeq[(BinaryRule[Int], Int)], unary: IndexedSeq[(UnaryRule[Int], Int)], numGrammars: Int = 1) = {
    val byParent = binary.groupBy(_._1.parent).values.map(_.size).max
    """#define SCALE_FACTOR 10
#define NUM_SYMS %d
#define NUM_GRAMMARS %d
#define ROOT %d
#define NUM_BINARY %d
#define NUM_UNARY %d
#define MAX_NUM_RULES_PER_SYMBOL %d
#define TRIANGULAR_INDEX(begin, end) ((end) * ((end)+1)/2 + begin)
#define CELL(chart, begin, end)   ((chart) + TRIANGULAR_INDEX(begin, end))

typedef struct {
  float binaries[NUM_BINARY][NUM_GRAMMARS];
  float unaries[NUM_UNARY][NUM_GRAMMARS];
} rule_cell;

typedef struct {
  float syms[NUM_SYMS][NUM_GRAMMARS];
} parse_cell;
    """.format(numSyms, numGrammars, root, binary.size, unary.size, byParent)
  }

  def insideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Int)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float parent;"
    val rules2 = rules.sortBy(_._1.parent)
    var lastParent = -1
    for( (r, index) <- rules2) {
      if(r.parent != lastParent) {
        if(lastParent != -1) {
          sb += """top->syms[%d][gram] = parent;""".format(lastParent)
        }
        sb += """parent = rules->unaries[%d][gram] * bot->syms[%d][gram];""".format(index, r.child)
        lastParent = r.parent
      } else {
        sb += """parent = mad(rules->unaries[%d][gram], bot->syms[%d][gram], parent);""".format(index, r.child)
      }
    }
    if(lastParent != -1) {
      sb += """top->syms[%d][gram] = parent;""".format(lastParent)
    }
    sb.mkString("\n    ")
  }

  def outsideUnaryUpdates(rules: IndexedSeq[(UnaryRule[Int], Int)]): String = {
    val sb = new ArrayBuffer[String]
    sb += "float child;"
    val rules2 = rules.sortBy(_._1.child)
    var lastChild = -1
    for( (r, index) <- rules2) {
      if(r.child != lastChild) {
        if(lastChild != -1) {
          sb += """bot->syms[%d][gram] = child;""".format(lastChild)
        }
        sb += """child = rules->unaries[%d][gram] * top->syms[%d][gram];""".format(index, r.parent)
        lastChild = r.child
      } else {
        sb += """child = mad(rules->unaries[%d][gram], top->syms[%d][gram], child);""".format(index, r.parent)
      }
    }
    if(lastChild != -1) {
      sb += """bot->syms[%d][gram] = child;""".format(lastChild)
    }
    sb.mkString("\n    ")
  }

  def insideRuleUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    var lastLeft = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentLeftScore;"
    for((r@BinaryRule(p, l, right), index) <- rules) {
      if(lastLeft != l) {
        if(lastLeft != -1)
          sb += "}"
        sb += "currentLeftScore = left->syms[%d][gram];" format l
        sb += "if(currentLeftScore != 0.0) {"
        lastLeft = l
      }
      sb += """out[%d] = mad(rules->binaries[%d][gram], currentLeftScore * right[%d], out[%d]);""".format(r.parent, index, r.right, r.parent)
    }
    sb += "}"

    sb.mkString("\n    ")
  }


  // otarget is the left child, completion on right.
  def outsideRightCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    // resort by right child, parent, left chidl
    val newrules = rules.sortBy(r => (r._1.right, r._1.parent, r._1.left))(Ordering.Tuple3)
    var lastRight = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), index) <- newrules) {
      if(lastRight != right) {
        if(lastRight != -1)
          sb += "}"
        sb += "currentCompl = gright->syms[%d][gram];" format right
        sb += "if(currentCompl != 0.0) {"
        lastRight = right
      }
      sb += """otarget[%d] = mad(rules->binaries[%d][gram], currentCompl * oparent[%d], otarget[%d]);""".format(r.left, index, r.parent, r.left)
    }

    sb += "}"
    sb.mkString("\n    ")
  }

  // otarget is the right child, completion on left.
  def outsideLeftCompletionUpdates(rules: IndexedSeq[(BinaryRule[Int], Int)]): String = {
    var lastLeft = -1
    val sb = new ArrayBuffer[String]
    sb += "float currentCompl;"
    for((r@BinaryRule(p, l, right), index) <- rules) {
      if(lastLeft != l) {
        if(lastLeft != -1)
          sb += "}"
        sb += "currentCompl = gleft->syms[%d][gram];" format l
        sb += "if(currentCompl != 0.0) {"
        lastLeft = l
      }
      sb += """otarget[%d] = mad(rules->binaries[%d][gram], currentCompl * oparent[%d], otarget[%d]);""".format(r.right, index, r.parent, r.right)
    }
    sb += "}"

    sb.mkString("\n    ")
  }

  def insideTemplate(rules: IndexedSeq[(BinaryRule[Int], Int)], unaries: IndexedSeq[(UnaryRule[Int], Int)]): String =
    """
__kernel void inside_binaries(
              __global parse_cell * inside_bots,
              __global const parse_cell * inside_tops,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];
  float out[NUM_SYMS], right[NUM_SYMS];
  if (end <= length) {
    __global const parse_cell* chart_top =  inside_tops + offsets[sentence];
    for(int i = 0; i < NUM_SYMS; ++i) {
      out[i] = 0.0f;
    }

    for(int split = begin + 1; split < end; ++split) {
      __global const parse_cell * left_top = CELL(chart_top, begin, split); // scale factor of (2 ^ SCALE_FACTOR)^((split - begin) - 1)
      __global const parse_cell * gright_top = CELL(chart_top, split, end); // scale factor of (2^ SCALE_FACTOR)((end-split) - 1)
      for(int i = 0; i < NUM_SYMS; ++i) {
        right[i] = gright[i][gram];
      }
      %s
    }
    // out has a scale factor of (2^SCALE_FACTOR)^((end-split) + (split-begin) - 2) = (2^SCALE_FACTOR)^(end-begin-2)
    // multiply in a 2^SCALE_FACTOR to reachive balance.
    __global parse_cell* gout = CELL(inside_bots + offsets[sentence], begin, end);
    for(int i = 0; i < NUM_SYMS; ++i) {
      gout[i][gram] = ldexp(out[i], SCALE_FACTOR);
    }
  }
}


__kernel void inside_unaries(__global const parse_cell * inside_bots,
              __global parse_cell * inside_tops,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if (end <= length) {
    __global parse_cell* top = CELL(inside_tops + offsets[sentence], begin, end);
    __global const parse_cell* bot = CELL(inside_bots + offsets[sentence], begin, end);
    %s
  }
}

    """.stripMargin.format(insideRuleUpdates(rules), insideUnaryUpdates(unaries))

def outsideTemplate(rules: IndexedSeq[(BinaryRule[Int], Int)], unaries: IndexedSeq[(UnaryRule[Int], Int)]): String ="""
  __kernel void outside_unaries(__global parse_cell * outside_tops,
                __global parse_cell * outside_bots,
                __global const int* offsets,
                __global const int* lengths,
                const int spanLength,
              __global const rule_cell* rules) {
    const int sentence = get_global_id(0);
    const int begin = get_global_id(1);
    const int end = begin + spanLength;
    const int gram = get_global_id(2);
    const int length = lengths[sentence];

    if(spanLength == length) {
      __global parse_cell* outside = charts + offsets[sentence];
      CELL(outside_tops, 0, length)[ROOT][gram] = 1.0f;
    }

    if (end <= length) {
    __global const parse_cell* top = CELL(outside_tops + offsets[sentence], begin, end);
    __global parse_cell* bot = CELL(outside_bots + offsets[sentence], begin, end);
      %s
    }
  }

  __kernel void outside_binaries(__global parse_cell* outsides_bot,
                __global const parse_cell * insides_top,
                __global const int* offsets,
                __global const int* lengths,
                const int spanLength,
              __global const rule_cell* rules) {
    const int sentence = get_global_id(0);
    const int begin = get_global_id(1);
    const int gram = get_global_id(2);
    const int end = begin + spanLength;
    const int length = lengths[sentence];
    float oparent[NUM_SYMS], otarget[NUM_SYMS];
    if (end <= length) {
      __global const parse_cell* inside = insides_top + offsets[sentence];
      __global parse_cell* outside = outsides_bot + offsets[sentence];
      for(int i = 0; i < NUM_SYMS; ++i) {
        otarget[i] = 0.0f;
      }
      // complete looking right
      for(int completion = end+1; completion <= length; ++completion) {
         __global const parse_cell * gparent = CELL(outside, begin, completion); // scale factor of (2 ^ SCALE_FACTOR)^(length-(completion - begin))
         __global const parse_cell * gright = CELL(inside, end, completion); // scale factor of (2 ^ SCALE_FACTOR)^((end - completion) - 1)
         // product of gparent and gright has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
         for(int i = 0; i < NUM_SYMS; ++i) {
           oparent[i] = gparent->syms[i][gram];
         }
         %s
      }

     // complete looking left
      for(int completion = 0; completion < begin; ++completion) {
         __global const parse_cell * gparent = CELL(outside, completion, end); // scale factor of (2 ^ SCALE_FACTOR)^(length-(end-completion))
         __global const parse_cell * gleft = CELL(inside, completion, begin); // scale factor of (2 ^ SCALE_FACTOR)^((begin - completion) - 1)
         // product of gparent and gleft has scale (2^SCALE_FACTOR)^(length-(end-begin)-1), so need to scale by 1 to maintain invariant
//         for(int i = 0; i < NUM_SYMS; ++i) {
//           icompl[i] = gleft[i];
//         }
         for(int i = 0; i < NUM_SYMS; ++i) {
           oparent[i] = gparent->syms[i][gram];
         }
         %s
      }

      // multiply in a 2^SCALE_FACTOR to re-achieve balance.
      __global parse_cell* gout = CELL(outside, begin, end);
      for(int i = 0; i < NUM_SYMS; ++i) {
        gout->syms[i][gram] = ldexp(otarget[i], SCALE_FACTOR);
//        gout[i] = otarget[i];
      }
    }
  }""".format(outsideUnaryUpdates(unaries), outsideRightCompletionUpdates(rules), outsideLeftCompletionUpdates(rules))




  def ecountsTemplate(binary: IndexedSeq[(BinaryRule[Int], Int)], unary: IndexedSeq[(UnaryRule[Int], Int)]) = {
    val byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Int)]] = binary.groupBy(_._1.parent)
    val uByParent: Map[Int, IndexedSeq[(UnaryRule[Int], Int)]] = unary.groupBy(_._1.parent)
    """
__kernel void ecount_binaries(__global rule_cell* ecounts,
   __global const parse_cell * insides_top,
   __global const parse_cell* outsides_bot,
   __global const int* offsets,
   __global const int* lengths,
   const int span_length,
   __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + span_length;
  const int length = lengths[sentence];
  __global rule_cell* ruleCounts = ecounts + (offsets[sentence] + TRIANGULAR_INDEX(begin, end));
  __global const parse_cell* obot = outsides_bot + offsets[sentence];
  __global const parse_cell* itop = insides_top + offsets[sentence];
  const float root_score = CELL(itop, 0, length)->syms[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
  if(end <= length) {
    float oscore;
    __global const parse_cell* oparents = CELL(obot, begin, end);
    %s
  }
}


__kernel void ecount_unaries(
              __global rule_cell* ecounts,
              __global const parse_cell * insides_top,
              __global const parse_cell * insides_bot,
              __global const parse_cell * outside_top,
              __global const int* offsets,
              __global const int* lengths,
              const int spanLength,
              __global const rule_cell* rules) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if (end <= length) {
    __global const parse_cell* itop = insides_top + offsets[sentence];
    __global const parse_cell* outside = outsides_top + offsets[sentence];
    __global const parse_cell* inside = insides_bot + offsets[sentence];
    const float root_score = CELL(itop, 0, length)[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
    __global rule_cell* ruleCounts = ecounts + (offsets[sentence] + TRIANGULAR_INDEX(begin, end));
    __global const parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* out = CELL(outside, begin, end);
    %s
  }
}

__kernel void ecount_terminals(
   __global parse_cell* term_ecounts,
   __global const parse_cell * insides_top,
   __global const parse_cell * insides_bot,
   __global const parse_cell * outsides_bot,
   __global const int* offsets,
   __global const int* lengths,
   __global const int* lengthOffsets,
   const int numGrammarsToDo) {
  const int sym = get_global_id(0)/ NUM_GRAMMARS;
  int grammar = get_global_id(0) %% NUM_GRAMMARS;
  const int sentence = get_global_id(1);
  const int begin = get_global_id(2);
  const int end = begin  + 1;
  const int length = lengths[sentence];
  if (begin < length) {
    __global const parse_cell* inside = insides_bot + offsets[sentence];
    __global const parse_cell* itop = insides_top + offsets[sentence];
    __global const parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* out = CELL(outsides_bot + offsets[sentence], begin, end);
    __global sym_cell* mybuf = term_ecounts + (lengthOffsets[sentence] + begin);
    // ibot has scale 0, obot has scale length - 1, root_score has scale length - 1. Woot.
    for(int i = 0; i < numGrammarsToDo && grammar < NUM_GRAMMARS; ++i) {
      const float root_score = CELL(itop, 0, length)[ROOT][grammar]; // scale is 2^(SCALE_FACTOR)^(length-1)
      mybuf[sym][grammar] = (in[sym][grammar] * out[sym][grammar])/root_score;
      grammar += (NUM_GRAMMARS / numGrammarsToDo);
    }
  }
}
    """.format(ecountBinaryRules(byParent), ecountUnaries(uByParent))
  }

  private def projectMarginalsKernel[L, L2](projections: ProjectionIndexer[L, L2], odds_ratio: Boolean) = {
    """
#define NUM_PROJECTED_SYMS  %d
typedef {
  float top[NUM_PROJECTED_SYMS][NUM_GRAMMARS], bot[NUM_PROJECTED_SYMS][NUM_GRAMMARS];
} projected_parse_cell;


typedef {
  float syms[NUM_PROJECTED_SYMS][NUM_GRAMMARS];
} projected_symbols;

__kernel void project_nterms(__global projected_parse_cell* projected,
                             __global const parse_cell* insides_top,
                             __global const parse_cell* insides,
                             __global const parse_cell* outsides,
                             __global const int* offsets,
                             __global const int* lengths) {
  const int sentence = get_global_id(0);
  const int begin = get_global_id(1);
  const int gram = get_global_id(2);
  const int end = begin + spanLength;
  const int length = lengths[sentence];

  if(end <= length) {
    __global const parse_cell* out = outsides + offsets[sentence];
    __global const parse_cell* in = insides + offsets[sentence];
    __global const parse_cell* itop = insides_top + offsets[sentence];
    const float root_score = CELL(itop, 0, length)[ROOT][gram]; // scale is 2^(SCALE_FACTOR)^(length-1)
    __global const parse_cell* in = CELL(inside, begin, end);
    __global const parse_cell* out = CELL(outside, begin, end);
    __global projected_symbols* target = CELL(projected + offsets[sentence], begin, end)

    %s
  }

}

    """.format(projections.coarseIndex.size, projectNonterminalsInner(projections, odds_ratio))
  }


  private def projectNonterminalsInner[L, L2](projections: ProjectionIndexer[L, L2], odds_ratio: Boolean) = {
    val buf = new ArrayBuffer[String]()
    if(odds_ratio)
      buf += "float sum = 0.0;"
    buf += "float cur;"
    for(coarse <- 0 until projections.coarseIndex.size) {
      buf += "cur = 0.0;"
      for(ref <- projections.refinementsOf(coarse)) {
        buf += "cur = mad(in->syms[%d][gram], out->syms[%d][gram], cur);".format(ref, ref)
      }
      if(odds_ratio) {
        buf += "sum += cur;"
        buf += "target->syms[%d][gram] = cur;".format(coarse)
      } else {
        buf += "target->syms[%d][gram] = cur/root_score;".format(coarse)
      }
    }

    if(odds_ratio) {
      buf += "const float norm = root_score - sum;"
      buf += "for (int i = 0; i < NUM_PROJECTED_SYMS; ++i) {"
      buf += "  target->syms[%d][gram] /= norm;"
      buf += "}"
    }
    buf.mkString("\n      ")
  }


  private val copyPosToCharts =
  """
__kernel void copy_pos_to_charts(
  __global const parse_cell* pos_tags,
  __global parse_cell * insides_bot,
  __global const int* offsets,
  __global const int* lengths,
  __global const int* lengthOffsets,
  const int numGrammarsToDo) {
  const int sym = get_global_id(0)/ NUM_GRAMMARS;
  int grammar = get_global_id(0) % NUM_GRAMMARS;
  const int sentence = get_global_id(1);
  const int begin = get_global_id(2);
  const int end = begin  + 1;
  const int length = lengths[sentence];
  if (begin < length) {
    __global parse_cell* inside = insides_bot + offsets[sentence];
    __global parse_cell* in = CELL(inside, begin, end);
    __global const sym_cell* mybuf = pos_tags + (lengthOffsets[sentence] + begin);
    // ibot has scale 0, obot has scale length - 1, root_score has scale length - 1. Woot.
    for(int i = 0; i < numGrammarsToDo && grammar < NUM_GRAMMARS; ++i) {
      in->syms[sym][grammar] = mybuf->syms[sym][grammar];
      grammar += (NUM_GRAMMARS / numGrammarsToDo);
    }
  }
}"""

  val registersToUse = 60

  private def ecountBinaryRules(byParent: Map[Int, IndexedSeq[(BinaryRule[Int], Int)]]):String = {
    val buf = new ArrayBuffer[String]()
    buf += (0 until registersToUse).map("r" + _).mkString("float ", ", ", ";")
    for((par, rx) <- byParent) {
      val rules = rx.sortBy(r => r._1.left -> r._1.right)(Ordering.Tuple2)

      // oparent has scale length + begin - end, root has scale length - 1
      // left * right has scale (end - begin-2)
      // left * right * oparent / root has scale -1
      buf += "oscore = ldexp(oparents->syms[%d][gram]/root_score, SCALE_FACTOR);".format(par)
      buf += "if (oscore != 0.0) {"
      var r = 0
      while(r < rules.length) {
        val assignments = Index[(Symbol,Int)]()
        val setThisRound = mutable.BitSet.empty
        val ruleRegisters = ArrayBuffer[(Int, Int)]() // Register -> Rule
        val regInitializerPos = buf.size
        buf += "XXX"

        buf += "  for(int split = begin + 1; split < end; ++split) {"
        var lastLeft = -1
        assignments.index(('left -> 1))
        while(r < rules.length && assignments.size < registersToUse) {
          val (BinaryRule(_, l, right), ruleIndex) = rules(r)
          if(lastLeft != l) {
            buf += "    r0 = CELL(itop, begin, split)->syms[%d][gram];".format(l)
            lastLeft = l
          }
          val rightR = assignments.index(('right, right))
          val ruleR = assignments.index(('rule, ruleIndex))
          if(assignments.size < registersToUse) {
            ruleRegisters += (ruleR -> ruleIndex)
            if (!setThisRound(rightR)) {
              buf += "    r%d = CELL(itop, split, end)->syms[%d][gram];".format(rightR, right)
              setThisRound += rightR
            }
            buf += "    r%d = fma(rules->binaries[%d][gram], r0 * r%d * oscore, r%d);".format(ruleR, ruleIndex, rightR, ruleR)
            r += 1
          }
        }

        buf += "  }\n"

        // register flush time!
        buf += "  // flush time!"
        for( (reg, rule) <- ruleRegisters) {
          buf += "  ruleCounts->binaries[%d][gram] =  r%d;".format(rule, reg)
        }
        buf(regInitializerPos) = ruleRegisters.map { case (reg, rule) => "r%d = 0.0f;".format(reg)}.mkString("  ", " ", "");
      }
      buf += "}\n"
    }
    buf.mkString("\n    ")
  }

  private def ecountUnaries(byParent: Map[Int,IndexedSeq[(UnaryRule[Int], Int)]]): String = {
    val buf = new ArrayBuffer[String]()
    buf += "    float oscore;"
    for( (par, rules) <- byParent) {
      // oparent has scale length + begin - end, root has scale length - 1
      // child has scale (end - begin-1)
      // child * oparent / root has scale 0 (yay!)
      buf += "oscore = out->syms[%d][gram]/root_score;".format(par)
      for( (r,index) <- rules) {
        buf += "ruleCounts->unaries[%d][gram] = rules->unaries[%d][gram] * oscore * in->syms[%d][gram];".format(index, index, r.child)
      }
    }

    buf.mkString("\n    ")

  }



  private val sumECountVectors =
    """
__kernel void sum_vectors(__global float* vec, const int maxLen, int pivot) {
  int trg = get_global_id(0);
  if(trg < maxLen)
    vec[trg] += vec[trg + pivot];
}
    """

  class ZeroMemory(context: CLContext) {
    val kernel = context.createProgram{
"""
__kernel void mem_zero(__global float* data, int len) {
  int trg = get_global_id(0);
  if(trg < len)
    data[trg] = 0.0f;
}
""".stripMargin
    }.createKernel("mem_zero")

    def zeroMemory(queue: CLQueue, data: CLBuffer[java.lang.Float]): CLEvent = {
      kernel.setArgs(data, java.lang.Integer.valueOf(data.getElementCount.toInt))
      kernel.enqueueNDRange(queue, Array(data.getElementCount.toInt))
    }
  }

}
