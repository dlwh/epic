package epic.sequences

import epic.trees.TreeInstance
import epic.framework.LossAugmentation
import epic.parser.projections.GoldTagPolicy
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints
import epic.parser
import breeze.numerics.I

/**
 * TODO
 *
 * @author dlwh
 **/
class HammingLossAugmentation[L, W](grammar: BaseGrammar[L],
                                    lexicon: Lexicon[L, W],
                                    precisionScale: Double = 1.0,
                                      recallScale: Double = 1.0) extends LossAugmentation[TreeInstance[L, W], CoreAnchoring[L, W]] {
  def lossAugmentation(datum: TreeInstance[L, W]): CoreAnchoring[L, W] = {
    val gt = GoldTagPolicy.goldTreeForcing(datum.tree.map(grammar.labelIndex))

    new HammingLossAugmentation.CoreAnchoring(grammar, lexicon, datum.words, gt, precisionScale, recallScale)
}

object HammingLossAugmentation {
  case class CoreAnchoring[L, W](grammar: BaseGrammar[L],
                                 lexicon: Lexicon[L, W],
                                 words: IndexedSeq[W],
                                 gt: GoldTagPolicy[L],
                                 precisionScale: Double,
                                 recallScale: Double,
                                 override val sparsityPattern: ChartConstraints[L] = ChartConstraints.noSparsity[L])  extends epic.parser.CoreAnchoring[L, W]{
    def addConstraints(cs: ChartConstraints[L]): parser.CoreAnchoring[L, W] = copy(sparsityPattern = cs & sparsityPattern)

    /**
     * Scores the indexed [[epic.trees.BinaryRule]] rule when it occurs at (begin,split,end)
     */
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = 0.0

    /**
     * Scores the indexed [[epic.trees.UnaryRule]] rule when it occurs at (begin,end)
     */
    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
      val p = grammar.parent(rule)
      if(gt.isGoldTopTag(begin, end, p)) -precisionScale
      else recallScale
    }

    /**
     * Scores the indexed label rule when it occurs at (begin,end). Can be used for tags, or for a
     * "bottom" label. Typically it is used to filter out impossible rules (using Double.NegativeInfinity)
     */
    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
      if(gt.isGoldTopTag(begin, end, tag)) -precisionScale
      else recallScale
    }
  }

  }
}