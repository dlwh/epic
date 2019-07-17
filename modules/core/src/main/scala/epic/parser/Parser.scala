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


import epic.constraints.ChartConstraints
import epic.lexicon.Lexicon
import epic.trees._


/**
 * A Parser produces a syntactic representation of a sentence, called a [[epic.trees.Tree]], which
 * has internal nodes that demarcate syntactic functions
 *
 * @author dlwh
 */
@SerialVersionUID(2L)
final case class Parser[L,W](topology: RuleTopology[L],
                             lexicon: Lexicon[L, W],
                             constraintsFactory: ChartConstraints.Factory[L, W],
                             marginalFactory: ParseMarginal.Factory[L, W],
                             decoder: ChartDecoder[L, W] = ChartDecoder[L, W]())
                            (implicit val debinarizer: Debinarizer[L]) extends (IndexedSeq[W]=>Tree[L]) {

  /**
   * Returns the best parse (calls bestParse) for the sentence.
   *
   * @param s the sentence
   */
  def apply(s: IndexedSeq[W]): Tree[L] = debinarizer(bestBinarizedTree(s))

  /**
   * Returns the best parse for the sentence without debinarizing
   * @param s sentence
   */
  def bestBinarizedTree(s: IndexedSeq[W]):BinarizedTree[L] = {
    decoder.extractBestParse(marginal(s))
  }

  def marginal(w: IndexedSeq[W]): ParseMarginal[L, W] = try {
    marginalFactory.apply(w, constraintsFactory.constraints(w))
  } catch {
    case ex: NoParseException =>
      try {
        marginalFactory.apply(w, ChartConstraints.noSparsity)
      } catch {
        case ex: NoParseException =>
          ???
      }
  }

}

object Parser {

  def apply[L, W](grammar: Grammar[L, W])(implicit deb: Debinarizer[L]): Parser[L, W]= {
    Parser(grammar.topology, grammar.lexicon, ChartConstraints.Factory.noSparsity, StandardChartFactory(grammar), ChartDecoder())
  }

  def apply[L, W](refined: Grammar[L, W], decoder: ChartDecoder[L, W])(implicit deb: Debinarizer[L]): Parser[L, W] = {
    apply(ChartConstraints.Factory.noSparsity, refined, decoder)
  }

  def apply[L, L2, W](refinedGrammar: SimpleGrammar[L, L2, W], decoder: ChartDecoder[L, W])(implicit deb: Debinarizer[L]): Parser[L, W] = {
    new Parser(refinedGrammar.topology, refinedGrammar.lexicon, ChartConstraints.Factory.noSparsity[L, W], new SimpleChartMarginal.SimpleChartFactory(refinedGrammar, decoder.wantsMaxMarginal), decoder)
  }

  def apply[L, W](core: ChartConstraints.Factory[L, W], grammar: Grammar[L, W], decoder: ChartDecoder[L, W])(implicit deb: Debinarizer[L]): Parser[L, W] = {
    Parser(grammar.topology, grammar.lexicon, core, StandardChartFactory(grammar, decoder.wantsMaxMarginal), decoder)
  }

  def apply[L, W](core: ChartConstraints.Factory[L, W], refinedGrammar: Grammar[L, W])(implicit deb: Debinarizer[L]): Parser[L, W] = {
    apply(core, refinedGrammar, new MaxConstituentDecoder)
  }
}
