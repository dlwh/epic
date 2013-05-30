package epic.parser.projections

import epic.constraints.ChartConstraints
import epic.trees.{TreeInstance, BinarizedTree}
import epic.parser._
import epic.lexicon.Lexicon
import breeze.numerics.I
import com.typesafe.scalalogging.log4j.Logging
import scala.collection.{GenTraversableLike, GenTraversable, GenTraversableOnce}
import scala.collection.generic.CanBuildFrom
import epic.util.CacheBroker

/**
 * Finds the best tree (relative to the gold tree) s.t. it's reacheable given the current anchoring.
 * Best is measured as number of correct labeled spans, as usual.
 * @author dlwh
 */
class ReachabilityProjection[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W]) extends Logging {
  private val cache = CacheBroker().make[IndexedSeq[W], BinarizedTree[L]]("ReachabilityProjection")

  private var problems  = 0
  private var total = 0

  def forTree(tree: BinarizedTree[L], words: IndexedSeq[W], constraints: ChartConstraints[L]) = cache.getOrElseUpdate(words, {
    val treeconstraints = ChartConstraints.fromTree(grammar.labelIndex, tree)
    if(constraints.top.containsAll(treeconstraints.top) && constraints.bot.containsAll(treeconstraints.bot)) {
      synchronized(total += 1)
      tree
    } else {
      val w = words
      val marg = AugmentedAnchoring.fromCore(new CoreAnchoring[L, W] {
        def words: IndexedSeq[W] = w

        def grammar: BaseGrammar[L] = ReachabilityProjection.this.grammar
        def lexicon = ReachabilityProjection.this.lexicon

        override def sparsityPattern: ChartConstraints[L] = constraints

        def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = 0.0

        def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
          val top = grammar.parent(rule)
          20 * I(treeconstraints.top.isAllowedLabeledSpan(begin, end, top))
        }

        /**
         * Scores the indexed label rule when it occurs at (begin,end). Can be used for tags, or for a
         * "bottom" label. Typically it is used to filter out impossible rules (using Double.NegativeInfinity)
         */
        def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
          20 * I(treeconstraints.bot.isAllowedLabeledSpan(begin, end, tag))
        }
      }).maxMarginal

      val closest = new ViterbiDecoder[L,W]().extractBestParse(marg)
      logger.warn {
        val stats = new ParseEval(Set.empty).apply(closest, tree)
        val ratio =  synchronized{problems += 1; total += 1; problems * 1.0 / total}
        f"Gold tree for $words not reachable. $ratio%.2f are bad so far. Best has score: $stats."
      }

      closest
    }
  })

  def projectCorpus[CC, CCR](constrainer: ChartConstraints.Factory[L, W], data: CC)(implicit ccview: CC<:<GenTraversableLike[TreeInstance[L, W], CC], cbf: CanBuildFrom[CC, TreeInstance[L, W], CCR]) = {
    data.map(ti => ti.copy(tree=forTree(ti.tree, ti.words, constrainer.constraints(ti.words))))(cbf)
  }

}
