package epic.sentiment

import epic.trees.{Span, AnnotatedLabel, TreeInstance}
import epic.framework.LossAugmentation
import epic.parser.projections.{ConstraintCoreGrammarAdaptor, GoldTagPolicy}
import epic.lexicon.Lexicon
import scala.collection.immutable.BitSet
import epic.parser.{CoreGrammar, CoreAnchoring, BaseGrammar}
import epic.constraints.ChartConstraints
import epic.everything.LiftedParser.L

/**
 * TODO
 *
 * @author dlwh
 **/
case class SentimentLossAugmentation[W](trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, W]],
                                        grammar: BaseGrammar[AnnotatedLabel],
                                        lexicon: Lexicon[AnnotatedLabel, W],
                                        constraintFactory: ChartConstraints.Factory[AnnotatedLabel, W],
                                        loss: (Int, Int)=>Double = SentimentLossAugmentation.defaultLoss) extends LossAugmentation[TreeInstance[AnnotatedLabel, W], CoreAnchoring[AnnotatedLabel, W]] with CoreGrammar[AnnotatedLabel, W] {

  val losses = Array.tabulate(5,5)(loss)

  def projectedLabel(l: AnnotatedLabel) =   if(l == AnnotatedLabel.TOP) -1 else l.label.toInt
  val sentimentScores: Array[Int] = grammar.labelEncoder.tabulateArray(projectedLabel)

  val trainingMap = trainTrees.iterator.map(ti => ti.words -> ti).toMap

  def lossAugmentation(datum: TreeInstance[AnnotatedLabel, W]): CoreAnchoring[AnnotatedLabel, W] = {
    // drop the root
    val goldMap = datum.tree.map(projectedLabel).preorder.drop(1).map{t => t.span -> t.label}.toMap

    new SentimentLossAnchoring(grammar, lexicon, datum.words, goldMap, constraintFactory.constraints(datum.words))
  }


  /**
   * Returns a [[epic.parser.CoreAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: IndexedSeq[W]): CoreAnchoring[AnnotatedLabel, W] = {
    trainingMap.get(words)
      .map( ti =>lossAugmentation(ti) )
      .getOrElse ( CoreAnchoring.identity(grammar, lexicon, words, constraintFactory.constraints(words)) )
  }

  case class SentimentLossAnchoring[L, W](grammar: BaseGrammar[L],
                                          lexicon: Lexicon[L, W],
                                          words: IndexedSeq[W],
                                          goldLabels: Map[Span, Int],
                                          override val sparsityPattern: ChartConstraints[L] = ChartConstraints.noSparsity[L])  extends epic.parser.CoreAnchoring[L, W]{
    def addConstraints(cs: ChartConstraints[L]): CoreAnchoring[L, W] = copy(sparsityPattern = cs & sparsityPattern)

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = 0

    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
       goldLabels.get(Span(begin,end)) match {
        case Some(goldLabel) =>
          assert(goldLabel != -1)
          val guessLabel = sentimentScores(tag)
          if(guessLabel == -1) {
            breeze.numerics.I(goldLabel == guessLabel) * 10000
          } else {
            losses(goldLabel)(guessLabel)
          }
        case None =>
           0
      }
    }

  }


}

object SentimentLossAugmentation {
  def defaultLoss(gold: Int, guess: Int) = (gold - guess).abs.toDouble
  def noLoss(gold: Int, guess: Int) = 0
}
