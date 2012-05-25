package scalanlp.parser

import projections.ProjectingCoreGrammar
import scalanlp.trees.AnnotatedLabel

/**
 *
 * @author dlwh
 */

final case class AugmentedGrammar[L, W](refined: RefinedGrammar[L, W], core: CoreGrammar[L, W]) {
  def grammar: BaseGrammar[L] = refined.grammar
  def lexicon: Lexicon[L, W] = refined.lexicon
  assert(grammar.eq(core.grammar) || grammar == core.grammar, "Grammars of core and refined do not match!")
  assert(lexicon.eq(core.lexicon) || lexicon == core.lexicon, "Lexicons of core and refined do not match!")

  def specialize(words: Seq[W]): AugmentedAnchoring[L, W] = {
    AugmentedAnchoring[L, W](refined.specialize(words), core.specialize(words))
  }

}

object AugmentedGrammar {
  def fromCore[L, W](core: CoreGrammar[L, W]) = {
    AugmentedGrammar(RefinedGrammar.identity(core.grammar, core.lexicon), core)
  }

  def fromRefined[L, W](refined: RefinedGrammar[L, W]) = {
    AugmentedGrammar(refined, CoreGrammar.identity(refined.grammar, refined.lexicon))
  }
}

final case class AugmentedAnchoring[L, W](refined: RefinedAnchoring[L, W], core: CoreAnchoring[L, W]) {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    val cScore = core.scoreBinaryRule(begin, split, end, rule)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreBinaryRule(begin, split, end, rule, ref)
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    val cScore = core.scoreUnaryRule(begin, end, rule)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreUnaryRule(begin, end, rule, ref)
  }

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    val cScore = core.scoreSpan(begin, end, label)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreSpan(begin, end, label, ref)
  }

  def grammar: BaseGrammar[L] = refined.grammar
  def lexicon: Lexicon[L, W] = refined.lexicon
  def words = refined.words

  lazy val marginal = ChartMarginal(this, words, ParseChart.logProb)
}


object AugmentedAnchoring {
  def fromRefined[L, W](refined: RefinedAnchoring[L, W]) = {
    AugmentedAnchoring(refined, CoreAnchoring.identity(refined.grammar, refined.lexicon, refined.words))
  }


  def fromCore[L, W](core: CoreAnchoring[L, W]) = {
    AugmentedAnchoring(RefinedAnchoring.identity(core.grammar, core.lexicon, core.words), core)
  }
}

