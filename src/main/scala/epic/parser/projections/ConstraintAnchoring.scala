package epic.parser
package projections

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import breeze.collection.mutable.TriangularArray
import breeze.config.{Configuration, CommandLineParser, Help}
import breeze.util.{Encoder, Index}
import collection.immutable.BitSet
import java.io._
import epic.trees._
import collection.mutable.ArrayBuffer
import breeze.stats.distributions.{Rand, Binomial}
import epic.parser.projections.ParserChartConstraintsFactory.PruningStatistics
import breeze.linalg._
import breeze.numerics._
import java.util
import epic.lexicon.Lexicon
import epic.constraints.{CachedChartConstraintsFactory, ChartConstraints}
import epic.util.CacheBroker
import scala.collection.GenTraversable
import breeze.util.SerializableLogging
import java.util.concurrent.atomic.AtomicInteger

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
@SerialVersionUID(1L)
class ParserChartConstraintsFactory[L, W](val parser: Parser[L, W],
                                          val isIntermediate: L=>Boolean,
                                          threshold: Double = math.exp(-9)) extends ChartConstraints.Factory[L, W] with Serializable with SerializableLogging {
  require(threshold >= 0 && threshold <= 1, s"Threshold must be between 0 and 1, but whas $threshold")
  import parser._
  def labelIndex = topology.labelIndex

  def overallStatistics = {
    val xp = pruned.get()
    val xpt = prunedtags.get()
    val xnp = notpruned.get()
    val xnpt = notprunedtags.get()

    s"Pruned Non-Unit Labels: ${xp * 1.0 / (xp + xnp)}; Pruned Labels ${xpt * 1.0 / {xpt + xnpt}}"
  }

  val pruned = new AtomicInteger(0)
  val notpruned = new AtomicInteger(0)
  val prunedtags = new AtomicInteger(0)
  val notprunedtags = new AtomicInteger(0)

  private val synthetics = BitSet.empty ++ (0 until topology.labelIndex.size).filter(l => isIntermediate(labelIndex.get(l)))

  def constraints(w: IndexedSeq[W]):ChartConstraints[L] = constraints(w, GoldTagPolicy.noGoldTags[L])
  def constraints(words: IndexedSeq[W], gold: GoldTagPolicy[L]):ChartConstraints[L] = {
    val charts = parser.marginal(words)
    constraints(charts, gold)
  }

  def constraints(marg: ParseMarginal[L, W], gold: GoldTagPolicy[L]): ChartConstraints[L] = {
    logger.debug(s"Building Constraints for ${marg.words}")
    assert(marg.isMaxMarginal)
    val length = marg.length
    if (marg.logPartition.isInfinite)
      throw new NoParseException("No parse for sentence we're trying to constrain!", marg.words)
    val (botLabelScores, unaryScores) = computeScores(length, marg)

    val vit = new ViterbiDecoder[L, W].extractBestParse(marg)

    val labelThresholds = extractLabelThresholds(length,
      topology.labelIndex.size,
      botLabelScores, topology.labelIndex,
      gold.isGoldBotTag)
    val topLabelThresholds = extractLabelThresholds(length,
      topology.labelIndex.size,
      unaryScores,topology.labelIndex,
      gold.isGoldTopTag)
    for(i <- 0 until length) {
      assert(labelThresholds(i, i+1) != null && labelThresholds(i,i+1).nonEmpty, "label thresholds" + labelThresholds(i, i+1))
      assert(topLabelThresholds(i, i+1) != null && topLabelThresholds(0,length).nonEmpty, "top label thresholds" + topLabelThresholds(i, i+1))
    }
    if (topLabelThresholds(0,length) == null || !topLabelThresholds(0,length).contains(marg.topology.rootIndex))
      throw new NoParseException("No score at the root!", marg.words)

      // val hasMaximalProjection: BitSet = BitSet.empty ++ (0 to length).filter{ i =>
      //  ((labelThresholds(i) ne null) && (topLabelThresholds(i) ne null)) && ((labelThresholds(i)|topLabelThresholds(i)) -- synthetics).nonEmpty
      // }

      //, hasMaximalProjection)
    val con = ChartConstraints[L](topLabelThresholds, labelThresholds)
    // PrecacheConstraints.checkConstraints(TreeInstance("viterbi", vit, marg.words), con, this)
    con
  }

  private def extractLabelThresholds(length: Int, numLabels: Int,
                                     scores: Array[Array[Double]],
                                     index: Index[_],
                                     isGold: (Int, Int, Int)=>Boolean): TriangularArray[BitSet] = {
    TriangularArray.tabulate[BitSet](length + 1) { (i, j) =>
      val arr = scores(TriangularArray.index(i, j))
      val thresholdedTags = if (arr eq null) {
        BitSet.empty
      } else {
        BitSet.empty ++ (arr.indices filter { s =>
          arr(s) >= threshold
        })
      }

      if (arr ne null)
        if (j == i) {
        } else if (j - i > 1) {
          this.notpruned.addAndGet(thresholdedTags.size)
          this.pruned.addAndGet(arr.count(_ != 0.0) - thresholdedTags.size)
        } else {
          if (thresholdedTags.isEmpty) assert(false, arr.toIndexedSeq)
          this.notprunedtags.addAndGet(thresholdedTags.size)
          this.prunedtags.addAndGet(arr.count(_ != 0.0) - thresholdedTags.size)
        }

      val goldTags = (0 until numLabels).filter { isGold(i, j, _) }
      for(t <- goldTags if arr == null || arr(t) < threshold) {
        if (arr == null) {
          logger.warn(s"Can't even construct span that has gold tag ${labelIndex.get(t)}!")
        } else {
          logger.warn(s"Got a below threshold for a goldTag! ${arr(t)} $threshold ${labelIndex.get(t)} "
            + s"\n($i,$j) best symbol: ${labelIndex.get((0 until labelIndex.size).maxBy(arr(_)))} ${arr.max}"
          )
        }
      }
      val result = thresholdedTags //++ goldTags
      if (result.nonEmpty) result
      else null
    }
  }

  def computePruningStatistics(words: IndexedSeq[W], gold: GoldTagPolicy[L]): (PruningStatistics, PruningStatistics) = {
    val charts = parser.marginal(words)
    computePruningStatistics(charts, gold)
  }

  def computePruningStatistics(marg: ParseMarginal[L, W], gold: GoldTagPolicy[L]): (PruningStatistics, PruningStatistics) = {
    val counts = DenseVector.zeros[Double](topology.labelIndex.size)
    val (scores, topScores) = computeScores(marg.length, marg)
    var nConstructed = 0
    val thresholds = ArrayBuffer[Double]()
    var nGoldConstructed = 0
    val gThresholds = ArrayBuffer[Double]()
    for(i <-  0 until marg.length; j <- (i+1) to marg.length) {
      {
      val arr = scores(TriangularArray.index(i, j))
      if (arr ne null)
        for(c <- 0 until topology.labelIndex.size) {
          thresholds += arr(c)
          nConstructed += 1
          if (gold.isGoldBotTag(i, j, c)) {
            if (arr(c) != 0)
              nGoldConstructed += 1
            else {
              throw new RuntimeException("Can't construct gold tree for " + " " + marg.words)
            }
            gThresholds += arr(c)
          }
       }
      }
      /*{
      val arr = topScores(TriangularArray.index(i, j))
      if (arr ne null)
        for(c <- 0 until grammar.labelIndex.size) {
          thresholds += arr(c)
          nConstructed += 1
          if (gold.isGoldTopTag(i, j, c)) {
            if (arr(c) != 0)
              nGoldConstructed += 1
            else counts(c) += 1
            gThresholds += arr(c)
          }
       } }*/
    }

    import ParserChartConstraintsFactory._
    PruningStatistics(thresholds.toArray, nConstructed, counts) -> PruningStatistics(gThresholds.toArray, nGoldConstructed, counts)
  }


  private def computeScores(length: Int, marg: ParseMarginal[L, W]) = {
    val scores = TriangularArray.raw(length + 1, null: Array[Double])
    val topScores = TriangularArray.raw(length + 1, null: Array[Double])
    val visitor = new AnchoredVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {}

      override def skipBinaryRules: Boolean = true

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if (score != 0.0) {
          if (topScores(index) eq null) {
            topScores(index) = new Array[Double](topology.labelIndex.size)
          }
          topScores(index)(topology.parent(rule)) = topScores(index)(topology.parent(rule)) max score
        }
      }

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if (score != 0.0) {
          if (scores(index) eq null) {
            scores(index) = new Array[Double](topology.labelIndex.size)
          }
          scores(index)(tag) = scores(index)(tag) max score
        }
      }
    }

    marg.visit(visitor)
    for(i <- 0 until length) {
      assert(scores(TriangularArray.index(i, i + 1)).exists(_ > 0.01), scores(TriangularArray.index(i, i+ 1)).toIndexedSeq + " " + i + marg.words + " " + marg.logPartition)
    }
    (scores,topScores)
  }
}

object ParserChartConstraintsFactory {

  case class PruningStatistics(data: Array[Double], nConstructed: Double, pruningCounts: DenseVector[Double]) {
    def merge(other: PruningStatistics, nAllowed:Int = data.length): PruningStatistics = {
      if (nAllowed >= data.length + other.data.length) {
        PruningStatistics(data ++ other.data, this.nConstructed + other.nConstructed, pruningCounts + other.pruningCounts)
      } else {
        val subsetThisSize = new Binomial(nAllowed, nConstructed/(other.nConstructed + nConstructed)).draw()
        val subset1 = Rand.subsetsOfSize(data, subsetThisSize).draw()
        val subset2 = Rand.subsetsOfSize(data, nAllowed - subsetThisSize).draw()
        PruningStatistics(subset1 ++ subset2 toArray, this.nConstructed + other.nConstructed, pruningCounts + other.pruningCounts)
      }
    }
  }

  object PruningStatistics {
    def empty(nsyms: Int) = PruningStatistics(Array.empty, 0, DenseVector.zeros(nsyms))
  }

}

/**
  * Object for creating  [[epic.constraints.CachedChartConstraintsFactory]]
 * from a parser and prepopulating it with the contents of a treebank.
  */
object PrecacheConstraints extends SerializableLogging {
  case class ProjectionParams(treebank: ProcessedTreebank,
                              @Help(text="Location of the parser")
                              parser: File,
                              @Help(text="path to cache database for constraints")
                              out: File = new File("constraints.cache"),
                              @Help(text="name of the table for the cache database")
                              name: String = "parseConstraints",
                              @Help(text="Longest train sentence to build constraintFactory for.")
                              maxParseLength: Int = 80,
                              threshold: Double = -7)

  /**
   * Method for creating  [[epic.constraints.CachedChartConstraintsFactory]]
   * from a parser and prepopulating it with the contents of a [[epic.trees.ProcessedTreebank]].
   * Optionally can do checks on whether or not you prune from the training set.
   **/
   def forTreebank(constrainer: ParserChartConstraintsFactory[AnnotatedLabel, String], treebank: ProcessedTreebank, tableName: String = "parseConstraints", verifyNoGoldPruningInTrain: Boolean = true)(implicit broker: CacheBroker) = {
    val cached = forTrainingSet(constrainer, treebank.trainTrees.par.map(ti => ti.copy(tree = ti.tree.map(_.baseAnnotatedLabel))), tableName, verifyNoGoldPruning = verifyNoGoldPruningInTrain)
    treebank.devTrees.par.foreach { ti =>
      logger.info(s"Ensuring existing constraint for dev tree ${ti.id} ${ti.words}")
      val constraints = cached.constraints(ti.words)
      if (verifyNoGoldPruningInTrain)
        checkConstraints(ti.copy(tree = ti.tree.map(_.baseAnnotatedLabel)), constraints, constrainer)
    }
    treebank.testTrees.par.foreach { ti =>
      logger.info(s"Ensuring existing constraint for test sentence ${ti.id} ${ti.words}")
       cached.constraints(ti.words)
    }
    cached
  }

  /**
   * Method for creating  [[epic.constraints.CachedChartConstraintsFactory]]
   * from a parser and prepopulating it with constraints for a training set.
   * Optionally can do checks on whether or not you prune from the training set.
   **/
  def forTrainingSet[L, W](constrainer: ParserChartConstraintsFactory[L, W],
                           train: GenTraversable[TreeInstance[L, W]],
                           tableName: String = "parseConstraints",
                           verifyNoGoldPruning: Boolean = true)(implicit broker: CacheBroker): CachedChartConstraintsFactory[L, W] = {
    val parsed = new AtomicInteger(0)
    val len = train.size
    val cache = broker.make[IndexedSeq[W], ChartConstraints[L]](tableName)
    for(ti <- train.par) try {
      var located = true
      val constraints = cache.getOrElseUpdate(ti.words, {
        located = false
        logger.info(s"Building constraints for ${ti.id} ${ti.words}")
        constrainer.constraints(ti.words)
      })
      if (located) {
        logger.info(s"Already had constraints for ${ti.id} ${ti.words}.")
      } else if (verifyNoGoldPruning) {
        checkConstraints(ti, constraints, constrainer)
      }
      val count: Int = parsed.incrementAndGet()
      if (count % 10 == 0) {
        logger.info("Pruning statistics so far: " + constrainer.overallStatistics)
      }
      if (count % 100 == 0) {
        logger.info(s"Parsed $count/$len.")
      }

    } catch {
      case e: Exception =>
        logger.error(s"Error while parsing ${ti.words}.", e)
    }

    broker.commit()
    new CachedChartConstraintsFactory(constrainer, cache)
  }

  def checkConstraints[W, L](ti: TreeInstance[L, W], constraints: ChartConstraints[L], constrainer: ParserChartConstraintsFactory[L, W]) {
    //        val decoded = new ViterbiDecoder[L, W].extractBestParse(marg)
    var printTree = true
    var nerrors = 0
    var all = 0
    def logError(pos: String, t: Tree[L], allowedSpans: Set[L]) {
      logger.warn(s"Pruned gold $pos label ${t.label} over span ${t.span}:${t.span.map(ti.words)} \n\tAllowed: $allowedSpans.\n\tSentence is ${ti.words.length} words long.\n\tin ${ti.tree.render(ti.words, newline = true)}.")
      printTree = false
      nerrors += 1
    }
    ti.tree.allChildren.foreach {
      case t@UnaryTree(_, _, _, _) =>
        all += 1
        if (!constraints.top.isAllowedLabeledSpan(t.begin, t.end, constrainer.labelIndex(t.label))) {
          val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.top.isAllowedLabeledSpan(t.begin, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
          logError(if (t.span.length == 1) "length one unary" else "top", t, allowedSpans)
        }
      case t@BinaryTree(_, _, _, _) =>
        all += 1
        if (!constraints.bot.isAllowedLabeledSpan(t.begin, t.end, constrainer.labelIndex(t.label))) {
          val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.bot.isAllowedLabeledSpan(t.begin, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
          logError("bot", t, allowedSpans)
        }
      case t =>
        all += 1
        if (!constraints.bot.isAllowedLabeledSpan(t.begin, t.end, constrainer.labelIndex(t.label))) {
          val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.bot.isAllowedLabeledSpan(t.begin, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
          logError("tag", t, allowedSpans)
        }
    }

    logger.info(s"${ti.id} pruning errors: $nerrors/$all = ${nerrors * 1.0 / all}")
  }

  def main(args: Array[String]) {
    val params:ProjectionParams = CommandLineParser.readIn[ProjectionParams](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    val treebank = params.treebank.copy(maxLength = 1000000)
    val parser =  breeze.util.readObject[Parser[AnnotatedLabel, String]](params.parser)

    val out = params.out
    out.getAbsoluteFile.getParentFile.mkdirs()

    val factory = new ParserChartConstraintsFactory[AnnotatedLabel, String](parser, {(_:AnnotatedLabel).isIntermediate}, exp(params.threshold))
    implicit val broker = new CacheBroker(params.out)
    forTreebank(factory, treebank, params.name)
    broker.commit()
    broker.close()
  }

}


