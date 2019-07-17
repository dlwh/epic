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
import epic.trees.{Debinarizer, TreeInstance, UnaryRule, BinaryRule}
import epic.constraints.ChartConstraints

/**
 * Base trait for "normal" Parser-type models
 * @author dlwh
 */
trait ParserModel[L, W] extends epic.framework.StandardExpectedCounts.Model[TreeInstance[L, W]] with ParserExtractable[L, W] {
  type Inference <: ParserInference[L, W]
  type Marginal = epic.parser.ParseMarginal[L, W]
  type Scorer = GrammarAnchoring[L, W]

  def extractParser(weights: DenseVector[Double])(implicit deb: Debinarizer[L]) = {
    val inf = inferenceFromWeights(weights).forTesting
    Parser(constrainer, inf.grammar, ChartDecoder[L, W]())
  }
}

trait ParserInference[L, W] extends ProjectableInference[TreeInstance[L, W], UnrefinedGrammarAnchoring[L, W]] {
  type ExpectedCounts = StandardExpectedCounts[Feature]
  type Marginal = epic.parser.ParseMarginal[L, W]
  type Scorer = GrammarAnchoring[L, W]

  def grammar: Grammar[L, W]
  def constrainer: ChartConstraints.Factory[L, W]

  override def forTesting: ParserInference[L, W] = this

  def scorer(v: TreeInstance[L, W]): Scorer = {
     grammar.anchor(v.words, constrainer.constraints(v.words))
  }

  /**
   * Produces the "guess marginal" which is the marginal conditioned on only the input data
   * @param v the example
   * @return gold marginal
   */
  def marginal(scorer: Scorer, v: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {
    val charts = try {
      (scorer * aug).marginal
    } catch {
      case e: Exception =>
      try {
        e.printStackTrace()
        scorer.marginal
      } catch {
        case e2: Exception =>
          throw e
      }
    }
    charts
  }

  def baseAugment(v: TreeInstance[L, W])  = UnrefinedGrammarAnchoring.identity(grammar.topology, grammar.lexicon, v.words, ChartConstraints.noSparsity)

  def project(v: TreeInstance[L, W], s: Scorer, m: Marginal, oldAugment: UnrefinedGrammarAnchoring[L, W]): UnrefinedGrammarAnchoring[L, W] = {
    projector.project(this, v, m)
  }

  protected def projector: EPProjector[L, W] = new AnchoredRuleApproximator(-15)
}

trait ParserModelFactory[L, W] extends ParserExtractableModelFactory[L, W] {
  type MyModel <: ParserModel[L, W]
}


