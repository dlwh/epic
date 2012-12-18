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

import epic.parser.{RefinedAnchoring, ParseChart, ChartMarginal}
import epic.parser.projections.AnchoredPCFGProjector
import epic.trees.TreeInstance

trait EPProjector[L, W] {
  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ParseMarginal[L, W]): CoreAnchoring[L, W]
}

@SerialVersionUID(1)
class AnchoredRuleApproximator[L, W](pruningThreshold: Double = Double.NegativeInfinity) extends EPProjector[L, W] with Serializable {

  def project(inf: ParserInference[L, W],
              instance: TreeInstance[L, W],
              marginal: ParseMarginal[L, W]):CoreAnchoring[L, W] = {
    val factory = new AnchoredPCFGProjector[L, W](marginal.grammar)
    factory.project(marginal)
  }

}
