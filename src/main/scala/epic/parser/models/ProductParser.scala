package epic.parser
package models

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
import projections.AnchoredRuleMarginalProjector

/**
 * TODO
 *
 * @author dlwh
 */
class ProductParser[L, W](grammar: BaseGrammar[L],
                          lexicon: Lexicon[L, W],
                          factories: RefinedGrammar[L, W]*) extends Parser[L, W] with Serializable {
  val proj = new AnchoredRuleMarginalProjector[L, W]

  override def bestParse(s: IndexedSeq[W]) = {
    val augments = factories.map(_.anchor(s).marginal).map(proj.project(_))
    val marg = augments.reduceLeft[CoreAnchoring[L, W]](_ * _).marginal
    ChartDecoder(grammar, lexicon).extractBestParse(marg)
  }

}

object ProductParser {
  def fromChartParsers[L, W](grammar: BaseGrammar[L],
                             lexicon: Lexicon[L, W],
                             grammars: (RefinedGrammar[L, W])*) = {
    new ProductParser(grammar, lexicon, grammars: _*)
  }
}
