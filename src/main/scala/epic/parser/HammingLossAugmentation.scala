package epic.parser

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
case class HammingLossAugmentation[L, W](grammar: BaseGrammar[L],
                                    lexicon: Lexicon[L, W],
                                    proj: L=>L,
                                    labelScale: Double = 1.0) extends LossAugmentation[TreeInstance[L, W], CoreAnchoring[L, W]] {
  def lossAugmentation(datum: TreeInstance[L, W]): CoreAnchoring[L, W] = {
    val gt = GoldTagPolicy.goldTreeForcing[L](datum.tree.map(proj) map (grammar.labelIndex))

    new HammingLossAugmentationCoreAnchoring(grammar, lexicon, datum.words, gt, labelScale)
  }

  def asCoreGrammar(training: IndexedSeq[TreeInstance[L, W]]):CoreGrammar[L, W] = new CoreGrammar[L, W] with Serializable {
    def grammar: BaseGrammar[L] = HammingLossAugmentation.this.grammar
    def lexicon: Lexicon[L, W] = HammingLossAugmentation.this.lexicon

    val trainingMap = training.iterator.map(ti => ti.words -> ti).toMap

    def anchor(words: IndexedSeq[W]): CoreAnchoring[L, W] = {
      trainingMap.get(words).map(lossAugmentation).getOrElse(CoreAnchoring.identity(grammar, lexicon, words))
  }

}

object HammingLossAugmentation {

}

case class HammingLossAugmentationCoreAnchoring[L, W](grammar: BaseGrammar[L],
                                 lexicon: Lexicon[L, W],
                                 words: IndexedSeq[W],
                                 gt: GoldTagPolicy[L],
                                 labelScale: Double,
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
      I(!gt.isGoldTopTag(begin, end, p))*labelScale + I(!gt.isGoldSpan(begin, end))
    }

    /**
     * Scores the indexed label rule when it occurs at (begin,end). Can be used for tags, or for a
     * "bottom" label. Typically it is used to filter out impossible rules (using Double.NegativeInfinity)
     */
    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
      I(!gt.isGoldBotTag(begin, end, tag))*labelScale
    }
  }

}