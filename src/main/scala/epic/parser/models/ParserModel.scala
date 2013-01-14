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
import epic.framework._
import breeze.linalg._
import epic.parser.GenerativeParser
import epic.trees.{TreeInstance, UnaryRule, BinaryRule}

/**
 * Base trait for "normal" Parser-type models
 * @author dlwh
 */
trait ParserModel[L, W] extends Model[TreeInstance[L, W]] with ParserExtractable[L, W] {
  type ExpectedCounts = StandardExpectedCounts[Feature]
  type Inference <: ParserInference[L, W]
  type Marginal = epic.parser.ParseMarginal[L, W]

  def extractParser(weights: DenseVector[Double]) = {
    val inf = inferenceFromWeights(weights)
    SimpleChartParser(AugmentedGrammar(inf.grammar, inf.baseMeasure))
  }
}


trait ParserInference[L, W] extends ProjectableInference[TreeInstance[L, W], CoreAnchoring[L, W]] {
  type ExpectedCounts = StandardExpectedCounts[Feature]
  type Marginal = epic.parser.ParseMarginal[L, W]


  def emptyCounts = StandardExpectedCounts.zero(featurizer.index)

  def grammar: RefinedGrammar[L, W]
  def featurizer: RefinedFeaturizer[L, W, Feature]
  def baseMeasure: CoreGrammar[L, W]

  def marginal(v: TreeInstance[L, W], aug: CoreAnchoring[L, W]) = {
    val fullGrammar = AugmentedAnchoring(grammar.anchor(v.words), aug)
    val charts = try {
      fullGrammar.marginal
    } catch {
      case e =>
      try {
        e.printStackTrace()
        AugmentedAnchoring.fromRefined(grammar.anchor(v.words)).marginal
      } catch {
        case e2 =>
          throw e
      }
    }
    charts -> charts.logPartition
  }

  def countsFromMarginal(v: TreeInstance[L, W], marg: Marginal, accum: ExpectedCounts, scale: Double) = {
    marg.expectedCounts(featurizer, accum, scale)
  }

  def baseAugment(v: TreeInstance[L, W])  = baseMeasure.anchor(v.words)

  protected def projector: EPProjector[L, W] = new AnchoredRuleApproximator(-14)

  def project(v: TreeInstance[L, W], m: Marginal, oldAugment: CoreAnchoring[L, W]) = {
    projector.project(this, v, m)
  }
}

trait ParserModelFactory[L, W] extends ParserExtractableModelFactory[L, W] {
  type MyModel <: ParserModel[L, W]

  protected def extractBasicCounts[L, W](trees: IndexedSeq[TreeInstance[L, W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
    GenerativeParser.extractCounts(trees)
  }
}