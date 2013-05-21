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
import breeze.config.{CommandLineParser, Help}
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
import com.typesafe.scalalogging.log4j.{Logging, Logger}

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(2L)
class ConstraintAnchoring[L, W](val grammar: BaseGrammar[L],
                             val lexicon: Lexicon[L, W],
                             val words: IndexedSeq[W],
                             override val sparsityPattern: ChartConstraints[L]) extends CoreAnchoring[L, W] with Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0


  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    breeze.numerics.logI(sparsityPattern.top.isAllowedLabeledSpan(begin, end, grammar.parent(rule)))
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    breeze.numerics.logI(sparsityPattern.bot.isAllowedLabeledSpan(begin, end, tag))
  }
}



@SerialVersionUID(1L)
class ConstraintCoreGrammarAdaptor[L, W](val grammar: BaseGrammar[L], val lexicon: Lexicon[L, W],
                                         val constraintsFactory: ChartConstraints.Factory[L, W]) extends CoreGrammar[L, W] with Serializable {
  /**
   * Returns a [[epic.parser.CoreAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: IndexedSeq[W]): CoreAnchoring[L, W] = new ConstraintAnchoring[L, W](grammar, lexicon, words, constraintsFactory.constraints(words))
}

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
@SerialVersionUID(8620602232218134084L)
class ParserChartConstraintsFactory[L, W](val augmentedGrammar: AugmentedGrammar[L, W],
                                          val isIntermediate: L=>Boolean,
                                          alpha: Double) extends ChartConstraints.Factory[L, W] {
  import augmentedGrammar._
  def labelIndex = grammar.labelIndex


  private val synthetics = BitSet.empty ++ (0 until grammar.labelIndex.size).filter(l => isIntermediate(labelIndex.get(l)))

  def constraints(w: IndexedSeq[W]):ChartConstraints[L] = constraints(w, GoldTagPolicy.noGoldTags[L])
  def constraints(words: IndexedSeq[W], gold: GoldTagPolicy[L]):ChartConstraints[L] = {
    val charts = ChartMarginal(augmentedGrammar.anchor(words), words, maxMarginal = true)
    constraints(charts, gold)
  }

  def constraints(marg: ParseMarginal[L, W], gold: GoldTagPolicy[L]): ChartConstraints[L] = {
    val length = marg.length
    val (botLabelScores, unaryScores) = computeScores(length, marg)

    val labelThresholds = extractLabelThresholds(length,
      grammar.labelIndex.size,
      botLabelScores, grammar.labelIndex,
      gold.isGoldBotTag(_, _, _))
    val topLabelThresholds = extractLabelThresholds(length,
      grammar.labelIndex.size,
      unaryScores,grammar.labelIndex,
      gold.isGoldTopTag(_, _, _))
    assert(topLabelThresholds(0,length).contains(marg.grammar.rootIndex))

//    val hasMaximalProjection: BitSet = BitSet.empty ++ (0 to length).filter{ i =>
//      ((labelThresholds(i) ne null) && (topLabelThresholds(i) ne null)) && ((labelThresholds(i)|topLabelThresholds(i)) -- synthetics).nonEmpty
//    }

    //TODO: maximal projections
    val pattern = ChartConstraints(topLabelThresholds, labelThresholds)//, hasMaximalProjection)
    pattern
  }


  private def extractLabelThresholds(length: Int, numLabels: Int,
                                     scores: Array[Array[Double]],
                                     index: Index[_],
                                     isGold: (Int, Int, Int)=>Boolean): TriangularArray[BitSet] = {
    TriangularArray.tabulate[BitSet](length + 1) { (i, j) =>
      val arr = scores(TriangularArray.index(i, j))
      val threshold = if(arr eq null) {
        1.0
      } else {
        val nonZeroCounts = arr.filterNot(_ == 0.0)
        // threshold in log space is alpha * Vmax + (1-alpha) * 1/numSyms * sum_{!V(sym).isInfinite} V(sym)
        // Vmax = score of root
        // our v's are exp(V(sym) - Vmax), so the average above is 1/numSyms * sum_{!V(sym) == 0.0) (log V(sym) + Vmax)
        // so the vmax comes out: Vmax + (1-alpha) * avg_{V(sym) != 0.0} logV(sym)
        // then we put threshold back in normal space, giving exp(Vmax + (1-alpha) ... whatever)
        // so we compare V(sym) to exp(Vmax + (1-alpha) ...)
        // but we've divided out by exp(Vmax) on the lhs, so that gives
        // Vsym > exp( (1-alpha) * ...)
        if(nonZeroCounts.exists(x => (x- 1.0).abs < 1E-4)) {
          // probably viterbi path is through here
          exp((1-alpha)/nonZeroCounts.length * sum(log(nonZeroCounts)))
        } else {
          // viterbi path isn't here, factor the symbol "not in the tree" into the average
          exp((1-alpha) * (sum(log(nonZeroCounts)))/(nonZeroCounts.length + 1.0))
        }
      }* .99
      val thresholdedTags = if (arr eq null) {
        BitSet.empty
      } else {
        BitSet.empty ++ (0 until arr.length filter { s =>
          arr(s) > threshold // lower a little due to floating point
        })
      }
      if(i == 0 && j == length)
        assert(!thresholdedTags.isEmpty, threshold + " " + Encoder.fromIndex(labelIndex).decode(arr))
      val goldTags = (0 until numLabels).filter { isGold(i, j, _) }
      for(t <- goldTags if arr(t) < threshold) {
        println(s"Got a below threshold for a goldTag! ${arr(t)} ${threshold} ${labelIndex.get(t)} "
          + s"\n($i,$j) best symbol: ${labelIndex.get((0 until labelIndex.size).maxBy(arr(_)))} ${arr.max}"
        )
      }
      val result = thresholdedTags //++ goldTags
      if (result.nonEmpty) result
      else null
    }
  }

  def computePruningStatistics(words: IndexedSeq[W], gold: GoldTagPolicy[L]): (PruningStatistics, PruningStatistics) = {
    val charts = ChartMarginal(augmentedGrammar.anchor(words), words, maxMarginal = true)
    computePruningStatistics(charts, gold)
  }

  def computePruningStatistics(marg: ParseMarginal[L, W], gold: GoldTagPolicy[L]): (PruningStatistics, PruningStatistics) = {
    val counts = DenseVector.zeros[Double](grammar.labelIndex.size)
    val (scores, topScores) = computeScores(marg.length, marg)
    var nConstructed = 0
    val thresholds = ArrayBuffer[Double]()
    var nGoldConstructed = 0
    val gThresholds = ArrayBuffer[Double]()
    for(i <-  0 until marg.length; j <- (i+1) to marg.length) {
      {
      val arr = scores(TriangularArray.index(i, j))
      if (arr ne null)
        for(c <- 0 until grammar.labelIndex.size) {
          thresholds += arr(c)
          nConstructed += 1
          if(gold.isGoldBotTag(i, j, c)) {
            if(arr(c) != 0)
              nGoldConstructed += 1
            else {
              throw new RuntimeException("Can't construct gold tree for " + " " + marg.words)
              counts(c) += 1
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
          if(gold.isGoldTopTag(i, j, c)) {
            if(arr(c) != 0)
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
            topScores(index) = new Array[Double](grammar.labelIndex.size)
          }
          topScores(index)(grammar.parent(rule)) = topScores(index)(grammar.parent(rule)) max score
        }
      }


      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if (score != 0.0) {
          if (scores(index) eq null) {
            scores(index) = new Array[Double](grammar.labelIndex.size)
          }
          scores(index)(tag) = scores(index)(tag) max score
        }
      }
    }

    marg.visit(visitor)
    (scores,topScores)
  }
}

object ParserChartConstraintsFactory {

  case class PruningStatistics(data: Array[Double], nConstructed: Double, pruningCounts: DenseVector[Double]) {
    def merge(other: PruningStatistics, nAllowed:Int = data.length): PruningStatistics = {
      if(nAllowed >= data.length + other.data.length) {
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


case class ProjectionParams(treebank: ProcessedTreebank,
                            @Help(text="Location of the parser")
                            parser: File,
                            @Help(text="path to cache database for constraints")
                            out: File = new File("constraints.cache"),
                            @Help(text="name of the table for the cache database")
                            name: String = "parseConstraints",
                            @Help(text="Longest train sentence to build constraintFactory for.")
                            maxParseLength: Int = 80,
                            alpha: Double = 0.8) {
}

/**
  * Object for creating  [[epic.constraints.CachedChartConstraintsFactory]]
 * from a parser and prepopulating it with the contents of a treebank.
  */
object PrecacheConstraints extends Logging {

  /**
   * Method for creating  [[epic.constraints.CachedChartConstraintsFactory]]
   * from a parser and prepopulating it with the contents of a [[epic.trees.ProcessedTreebank]].
   * Optionally can do checks on whether or not you prune from the training set.
   **/
   def forTreebank(constrainer: ParserChartConstraintsFactory[AnnotatedLabel, String], treebank: ProcessedTreebank, tableName: String = "parseConstraints", verifyNoGoldPruningInTrain: Boolean = true)(implicit broker: CacheBroker) = {
    val cached = forTrainingSet(constrainer, treebank.trainTrees.par.map(ti => ti.copy(tree = ti.tree.map(_.baseAnnotatedLabel))), tableName, verifyNoGoldPruning = verifyNoGoldPruningInTrain)
    (treebank.testTrees ++ treebank.devTrees).par.foreach { ti =>
      logger.info(s"Ensuring existing constraint for dev/test tree ${ti.id} ${ti.words}")
      cached.constraints(ti.words)
    }
    if(verifyNoGoldPruningInTrain)
      for(t <- treebank.devTrees) {

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
    val cache = broker.make[IndexedSeq[W], ChartConstraints[L]](tableName)
    for(ti <- train) try {
      var located = true
      lazy val marg = ChartMarginal(constrainer.augmentedGrammar.anchor(ti.words), ti.words, maxMarginal = true)
      val constraints = cache.getOrElseUpdate(ti.words, {
        located = false
        logger.info(s"Building constraints for ${ti.id} ${ti.words}")
        constrainer.constraints(marg, GoldTagPolicy.goldTreeForcing[L](ti.tree.map(constrainer.labelIndex)))
      })
      if(located) {
        logger.info(s"Already had constraints for ${ti.id} ${ti.words}.")
      }
      if(verifyNoGoldPruning) {
        marg.checkForSimpleTree(ti.tree)
        var printTree = true
        def logError(pos: String, t: Tree[L], allowedSpans: Set[L]) {
          logger.warn(s"Pruned gold $pos label ${t.label} over span ${t.span}:${t.span.map(ti.words)} \n\tAllowed: $allowedSpans.\n\tSentence is ${ti.words.length} words long.\n\tin ${ti.tree.render(ti.words, newline = true)}. decoded tree is ${predicted.render(ti.words)}")
          printTree = false
        }
        ti.tree.allChildren.foreach {
          case t @ UnaryTree(_,_,_,_) if t.span.length > 1 =>
            if(!constraints.top.isAllowedLabeledSpan(t.start,t.end, constrainer.labelIndex(t.label))) {
              val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.top.isAllowedLabeledSpan(t.start, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
              logError("top", t, allowedSpans)
            }
          case t@BinaryTree(_,_,_,_) =>
            if(!constraints.bot.isAllowedLabeledSpan(t.start,t.end, constrainer.labelIndex(t.label))) {
              val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.bot.isAllowedLabeledSpan(t.start, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
              logError("bot", t, allowedSpans)
            }
          case t =>
            if(!constraints.bot.isAllowedLabeledSpan(t.start,t.end, constrainer.labelIndex(t.label))) {
              val allowedSpans = (0 until constrainer.labelIndex.size).filter(constraints.bot.isAllowedLabeledSpan(t.start, t.end, _)).map(constrainer.labelIndex.get(_)).toSet
              logError(if(t.isInstanceOf[UnaryTree[_]]) "length one unary" else "tag", t, allowedSpans)
            }
        }
      }
    } catch {
      case e: Exception =>
        logger.error(s"Error while parsing ${ti.words}.", e)
    }

    broker.commit()
    new CachedChartConstraintsFactory(constrainer, cache)
  }

  def main(args: Array[String]) {
    val params:ProjectionParams = CommandLineParser.readIn[ProjectionParams](args)
    val treebank = params.treebank.copy(maxLength = 1000000)
    println(params)
    val parser = loadParser[Any](params.parser)

    val out = params.out
    out.getAbsoluteFile.getParentFile.mkdirs()

    val factory = new ParserChartConstraintsFactory[AnnotatedLabel, String](parser.augmentedGrammar, {(_:AnnotatedLabel).isIntermediate}, params.alpha)
    implicit val broker = new CacheBroker(params.out)
    forTreebank(factory, treebank, params.name)
    broker.commit()
    broker.close()
  }

  def loadParser[T](loc: File): SimpleChartParser[AnnotatedLabel, String] = {
    val parser = breeze.util.readObject[SimpleChartParser[AnnotatedLabel, String]](loc)
    parser
  }

}


