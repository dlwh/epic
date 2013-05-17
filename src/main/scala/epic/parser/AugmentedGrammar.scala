package epic.parser

import projections.ConstraintAnchoring
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints

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

/**
 * An AugmentedGrammar is the pairing of a [[epic.parser.RefinedGrammar]]
 * and a [[epic.parser.CoreGrammar]]. It's the main entity in the EPIC
 * parser. Mostly it is a factory for creating
 * [[epic.parser.AugmentedAnchoring]], which are used to create parsers.
 * @author dlwh
 * */
@SerialVersionUID(1L)
final case class AugmentedGrammar[L, W](refined: RefinedGrammar[L, W], core: CoreGrammar[L, W]) {
  def grammar: BaseGrammar[L] = refined.grammar
  def lexicon: Lexicon[L, W] = refined.lexicon
  assert(grammar.eq(core.grammar) || grammar == core.grammar, "Grammars of core and refined do not match!")
  assert(lexicon.eq(core.lexicon) || lexicon == core.lexicon, "Lexicons of core and refined do not match!")

  def anchor(words: IndexedSeq[W]): AugmentedAnchoring[L, W] = {
    AugmentedAnchoring[L, W](refined.anchor(words), core.anchor(words))
  }

}

object AugmentedGrammar {
  /**
   * Creates an AugmentedGrammar from a [[epic.parser.CoreGrammar]]
   * and an identity [[epic.parser.RefinedGrammar]]
   */
  def fromCore[L, W](core: CoreGrammar[L, W]) = {
    AugmentedGrammar(RefinedGrammar.identity(core.grammar, core.lexicon), core)
  }

  /**
   * Creates an AugmentedGrammar from a [[epic.parser.RefinedGrammar]]
   * and an identity [[epic.parser.CoreGrammar]]
   */
  def fromRefined[L, W](refined: RefinedGrammar[L, W]) = {
    AugmentedGrammar(refined, CoreGrammar.identity(refined.grammar, refined.lexicon))
  }
}

/**
 * An AugmentedAnchoring is the primary "grammar"-like interface
 * in Epic. It can score refined grammars, using scores from both
 * the refined and core anchorings it contains.
 */
@SerialVersionUID(2L)
final case class AugmentedAnchoring[L, W](refined: RefinedAnchoring[L, W], core: CoreAnchoring[L, W], viterbi: Boolean = false) {
  def grammar: BaseGrammar[L] = refined.grammar
  def lexicon: Lexicon[L, W] = refined.lexicon
  def words = refined.words

  /** The marginal associated with this anchoring (i.e. inside and outside scores) */
  def marginal = ChartMarginal(this, words)

  /**
   * Scores the [[epic.trees.BinaryRule]] with its refinement in this context.
   * Indices are with respect to the [[epic.parser.BaseGrammar]] and refined's refinements
   * @param begin
   * @param split
   * @param end
   * @param rule the index of the rule
   * @param ref the index of the rule's refinements
   * @return
   */
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    val cScore = core.scoreBinaryRule(begin, split, end, rule)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreBinaryRule(begin, split, end, rule, ref)
  }

  /**
   * Scores the [[epic.trees.UnaryRule]] with its refinement in this context.
   * Indices are with respect to the [[epic.parser.BaseGrammar]] and refined's refinements
   * @param begin
   * @param end
   * @param rule the index of the rule
   * @param ref the index of the rule's refinements
   * @return
   */
  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    val cScore = core.scoreUnaryRule(begin, end, rule)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreUnaryRule(begin, end, rule, ref)
  }

  /**
   * Scores the label (of type L) with its refinement in this context.
   * Indices are with respect to the [[epic.parser.BaseGrammar]] and refined's refinements
   * @param begin
   * @param end
   * @param label the index of the label
   * @param ref the index of the label's refinements
   * @return
   */
  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    val cScore = core.scoreSpan(begin, end, label)
    if(cScore == Double.NegativeInfinity) cScore
    else cScore + refined.scoreSpan(begin, end, label, ref)
  }

}

object AugmentedAnchoring {
  def fromRefined[L, W](refined: RefinedAnchoring[L, W], viterbi: Boolean = false) = {
    AugmentedAnchoring(refined, CoreAnchoring.identity(refined.grammar, refined.lexicon, refined.words), viterbi)
  }

  def apply[L, W](refined: RefinedAnchoring[L, W], sparsity: ChartConstraints[L]) = {
    new AugmentedAnchoring(refined, new ConstraintAnchoring(refined.grammar, refined.lexicon, refined.words, sparsity))
  }


  def fromCore[L, W](core: CoreAnchoring[L, W], viterbi: Boolean = false) = {
    AugmentedAnchoring(RefinedAnchoring.identity(core.grammar, core.lexicon, core.words), core, viterbi)
  }
}

