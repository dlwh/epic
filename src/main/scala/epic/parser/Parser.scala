package epic.parser
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


import epic.trees._
import epic.constraints.ChartConstraints
import epic.lexicon.Lexicon


/**
 * A Parser produces a syntactic representation of a sentence, called a [[epic.trees.Tree]], which
 * has internal nodes that demarcate syntactic functions
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
final case class Parser[L,W](topology: RuleTopology[L],
                             lexicon: Lexicon[L, W],
                             constraintsFactory: ChartConstraints.Factory[L, W],
                             marginalFactory: RefinedChartMarginal.Factory[L, W],
                             decoder: ChartDecoder[L, W] = ChartDecoder[L, W]()) extends (IndexedSeq[W]=>Tree[L]) {

  /**
   * Returns the best parse (calls bestParse) for the sentence
   *
   * @param s the sentence
   */
  def apply(s: IndexedSeq[W]) = parse(s)

  /**
   * Returns the best parse for the sentence.
   * @param s sentence
   */
  def parse(s: IndexedSeq[W]):BinarizedTree[L] = {
    decoder.extractBestParse(marginal(s))
  }

  def marginal(w: IndexedSeq[W]) = marginalFactory.apply(w, constraintsFactory.constraints(w))
}

object Parser {

  def apply[L, W](ref: RefinedGrammar[L, W]): Parser[L, W]= {
    Parser(ref.topology, ref.lexicon, ChartConstraints.Factory.noSparsity, StandardChartFactory(ref))
  }

  def apply[L, W](refined: RefinedGrammar[L, W], decoder: ChartDecoder[L, W]): Parser[L, W] = {
    apply(ChartConstraints.Factory.noSparsity, refined, decoder)
  }


  def apply[L, W](core: ChartConstraints.Factory[L, W], refinedGrammar: RefinedGrammar[L, W], decoder: ChartDecoder[L, W]): Parser[L, W] = {
    Parser(refinedGrammar.topology, refinedGrammar.lexicon, core, StandardChartFactory(refinedGrammar, decoder.wantsMaxMarginal), decoder)
  }


  def apply[L, W](core: ChartConstraints.Factory[L, W], refinedGrammar: RefinedGrammar[L, W]): Parser[L, W] = {
    apply(core, refinedGrammar, new MaxConstituentDecoder)
  }
}
