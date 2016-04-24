package epic.sentiment

import epic.trees.{Span, AnnotatedLabel, TreeInstance}
import epic.framework.LossAugmentation
import epic.lexicon.Lexicon
import epic.parser.{UnrefinedGrammarAnchoring, RuleTopology}
import epic.constraints.ChartConstraints

/**
 * TODO
 *
 * @author dlwh
 **/
case class SentimentLossAugmentation[W](trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, W]],
                                        topology: RuleTopology[AnnotatedLabel],
                                        lexicon: Lexicon[AnnotatedLabel, W],
                                        constraintFactory: ChartConstraints.Factory[AnnotatedLabel, W],
                                        loss: (Int, Int)=>Double = SentimentLossAugmentation.defaultLoss,
                                        rootLossScaling:Double = 1.0) extends LossAugmentation[TreeInstance[AnnotatedLabel, W], UnrefinedGrammarAnchoring[AnnotatedLabel, W]] {

  val losses = Array.tabulate(5,5)(loss)

  def projectedLabel(l: AnnotatedLabel) =   if (l == AnnotatedLabel.TOP) -1 else l.label.toInt
  val sentimentScores: Array[Int] = topology.labelEncoder.tabulateArray(projectedLabel)

  val trainingMap = trainTrees.iterator.map(ti => ti.words -> ti).toMap

  def lossAugmentation(datum: TreeInstance[AnnotatedLabel, W]): UnrefinedGrammarAnchoring[AnnotatedLabel, W] = {
    // drop the root
    val goldMap = datum.tree.map(projectedLabel).preorder.filter(_.label != -1).map{t => t.span -> t.label}.toMap
    new SentimentLossAnchoring(topology, lexicon, datum.words, goldMap, constraintFactory.constraints(datum.words))
  }

  /**
   * Returns a [[epic.parser.UnrefinedGrammarAnchoring]] for this particular sentence.
   * @param words
   * @return
   */
  def anchor(words: IndexedSeq[W]): UnrefinedGrammarAnchoring[AnnotatedLabel, W] = {
    trainingMap.get(words)
      .map( ti =>lossAugmentation(ti) )
      .getOrElse ( UnrefinedGrammarAnchoring.identity(topology, lexicon, words, constraintFactory.constraints(words)) )
  }

  case class SentimentLossAnchoring[L, W](topology: RuleTopology[L],
                                          lexicon: Lexicon[L, W],
                                          words: IndexedSeq[W],
                                          goldLabels: Map[Span, Int],
                                          sparsityPattern: ChartConstraints[L])  extends epic.parser.UnrefinedGrammarAnchoring[L, W]{
    override def addConstraints(cs: ChartConstraints[L]): UnrefinedGrammarAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & cs)

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = 0.0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = 0

    def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
       goldLabels.get(Span(begin,end)) match {
        case Some(goldLabel) =>
          assert(goldLabel != -1)
          val guessLabel = sentimentScores(tag)
          if (guessLabel == -1) {
            breeze.numerics.I(goldLabel == guessLabel) * 10000
          } else {
            losses(goldLabel)(guessLabel) * (if (begin == 0 && end == words.size) rootLossScaling else 1.0)
          }
        case None =>
           0
      }
    }

  }

}

object SentimentLossAugmentation {
  def defaultLoss(gold: Int, guess: Int) = (gold - guess).abs.toDouble
  def posNegLoss(gold: Int, guess: Int) = {
    if (gold > 2) {
      if (guess > 2) 0 else 1
    } else if (gold < 2) {
      if (guess < 2) 0 else 1
    } else {
      if (guess == 2) 0 else 1
    }
  }
  def hammingLoss(gold: Int, guess: Int) = if (gold != guess) 1 else 0
  def noLoss(gold: Int, guess: Int) = 0
}
