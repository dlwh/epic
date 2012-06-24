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

case class DiscParserInference[L, W](featurizer: RefinedFeaturizer[L, W, Feature],
                                     ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[(L, Int)],
                                     grammar: RefinedGrammar[L, W],
                                     baseMeasure: CoreGrammar[L, W]) extends ParserInference[L, W] {

  // E[T-z|T, params]
  def goldCounts(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]) = {
    val tree = ti.tree
    val words = ti.words
    val annotated = ann(tree, words)


    TreeMarginal(AugmentedGrammar.fromRefined(grammar), words, annotated).expectedCounts(featurizer)
  }

}
