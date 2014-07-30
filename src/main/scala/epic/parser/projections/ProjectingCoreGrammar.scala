package epic.parser
package projections

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
 * 
 * @author dlwh
 */
case class ProjectingCoreGrammar[L, W](parser: Parser[L, W],
                                       projector: ChartProjector[L, W]) extends Grammar[L, W] {


  def topology = parser.topology

  def lexicon = parser.lexicon


  override def withPermissiveLexicon: Grammar[L, W] = {
    ???
  }

  def anchor(words: IndexedSeq[W], constraints: ChartConstraints[L]) = {
    project(words)
  }

  def project(s: IndexedSeq[W], goldTagPolicy: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags) = {
    val charts = parser.marginal(s)

    projector.project(charts, goldTagPolicy)
  }

}

