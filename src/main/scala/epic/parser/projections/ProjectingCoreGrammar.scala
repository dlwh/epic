package epic.parser
package projections
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
 * 
 * @author dlwh
 */
case class ProjectingCoreGrammar[L, W](parser: AugmentedGrammar[L, W],
                                       projector: ChartProjector[L, W]) extends CoreGrammar[L, W] {


  def grammar = parser.grammar

  def lexicon = parser.lexicon

  def anchor(words: IndexedSeq[W]) = {
    project(words)
  }

  def project(s: IndexedSeq[W], goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags) = {
    val charts = ChartMarginal(parser, s)

    projector.project(charts, goldTagPolicy)
  }

}

