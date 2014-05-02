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


/**
 * A Parser produces a syntactic representation of a sentence, called a [[epic.trees.Tree]], which
 * has internal nodes that demarcate syntactic functions
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
final case class Parser[L,W](coreGrammar: CoreGrammar[L, W],
                             marginalFactory: RefinedChartMarginal.Factory[L, W],
                             decoder: ChartDecoder[L, W] = ChartDecoder[L, W]()) extends (IndexedSeq[W]=>Tree[L]) {

  def topology = coreGrammar.topology
  def lexicon = coreGrammar.lexicon

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

  def marginal(w: IndexedSeq[W]) = marginalFactory.apply(w, coreGrammar.anchor(w))
}

object Parser {
  def apply[L, W](grammar: AugmentedGrammar[L, W]): Parser[L, W] = {
    Parser(grammar.core, grammar.refined)

  }


  def apply[L, W](ref: RefinedGrammar[L, W]): Parser[L, W]= {
    Parser(CoreGrammar.identity(ref.topology, ref.lexicon), SimpleChartFactory(ref))
  }

  def apply[L, W](refined: RefinedGrammar[L, W], decoder: ChartDecoder[L, W]): Parser[L, W] = {
    Parser(CoreGrammar.identity(refined.topology, refined.lexicon), SimpleChartFactory(refined, decoder.wantsMaxMarginal), decoder)
  }


  def apply[L, W](core: CoreGrammar[L, W], refinedGrammar: RefinedGrammar[L, W], decoder: ChartDecoder[L, W]): Parser[L, W] = {
    Parser(core, SimpleChartFactory(refinedGrammar, decoder.wantsMaxMarginal), decoder)
  }


  def apply[L, W](core: CoreGrammar[L, W], refinedGrammar: RefinedGrammar[L, W]): Parser[L, W] = {
    Parser(core, refinedGrammar, ChartDecoder[L, W]())
  }
}
