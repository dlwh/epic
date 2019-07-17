package epic.parser.models

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
import epic.framework.Feature
import epic.trees.{TreeInstance, BinarizedTree}
import epic.parser._
import epic.constraints.ChartConstraints

/**
 * Parser inference for parses that have only observed annotations. Handles Structured, Span, and Lexicalized
 * annotations.
 * @param featurizer
 * @param annotator
 * @param grammar
 * @tparam L
 * @tparam W
 *
 * @author dlwh
 */
case class AnnotatedParserInference[L, W](featurizer: RefinedFeaturizer[L, W, Feature],
                                          annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[(L, Int)],
                                          grammar: Grammar[L, W],
                                          constrainer: ChartConstraints.Factory[L, W]) extends ParserInference[L, W] {

  override def forTesting = copy(featurizer.forTesting, constrainer = ChartConstraints.Factory.noSparsity)

  def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {
    import ti._
    val annotated = annotator(tree, words)
    TreeMarginal(scorer, annotated)
  }

}
