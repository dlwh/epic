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
import breeze.linalg._
import epic.framework.{EPModel, EPInference}
import epic.trees.{TreeInstance, BinarizedTree}

/**
 * Parser that runs EP using the EPInference object and extracts a parse
 *
 * @author dlwh
 */
class EPParser[L, W](grammar: BaseGrammar[L],
                     lexicon: Lexicon[L, W],
                     inference: EPInference[TreeInstance[L, W], CoreAnchoring[L, W]]) extends Parser[L, W] with Serializable {
  def bestParse(s: Seq[W]) = {
    val inst = new TreeInstance[L, W]("", null, s)
    val augment = inference.marginal(inst, inference.baseAugment(inst))
    val marg = augment
    ChartDecoder(grammar, lexicon).extractBestParse(marg.marginals.head.asInstanceOf[ChartMarginal[L, W]])
  }

}

object EPParser {

  /**
   * EPModels that support pulling out a tree.
   * @tparam L
   * @tparam W
   */
  trait Extractor[L, W] extends EPModel[TreeInstance[L, W], CoreAnchoring[L, W]] with ParserExtractable[L, W] {
    def grammar: BaseGrammar[L]

    def lexicon: Lexicon[L, W]

    def extractParser(weights: DenseVector[Double]) = {
      new EPParser(grammar, lexicon, inferenceFromWeights(weights))
    }
  }

  def fromChartParsers[L, W](grammar: BaseGrammar[L],
                             lexicon: Lexicon[L, W],
                             base: CoreGrammar[L, W],
                             grammars: (RefinedGrammar[L, W])*) = {
    val infs = grammars.map {
      p =>
        new AnnotatedParserInference(null, {
          (a: BinarizedTree[L], b: Seq[W]) => a.map(_ -> 0)
        },
        p,
        base)
    }
    val ep = new EPInference(infs.toIndexedSeq, 5)
    new EPParser(grammar, lexicon, ep)
  }
}