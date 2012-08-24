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

import epic.parser._
import java.io.File
import epic.framework._

import breeze.util._
import epic.trees.{TreeInstance, AnnotatedLabel}
import breeze.config.Help

case class EPParams(maxIterations: Int = 5, pruningThreshold: Double = -15)

object EPParserModelFactory {
  type CompatibleFactory = ModelFactory[TreeInstance[AnnotatedLabel, String]] {
    type MyModel <: EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], CoreAnchoring[AnnotatedLabel, String]]
  }
}

case class EPParserModelFactory(ep: EPParams,
                                baseParser: ParserParams.XbarGrammar,
                                @Help(text="ModelFactories to use. use --model.0, --model.1, etc.")
                                model: Seq[EPParserModelFactory.CompatibleFactory],
                                @Help(text="Path to old weights used for initialization")
                                oldWeights: File = null) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = (
    EPModel[TreeInstance[AnnotatedLabel, String], CoreAnchoring[AnnotatedLabel, String]]
      with EPParser.Extractor[AnnotatedLabel, String]
    )

  def make(train: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(train)

    type ModelType = EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], CoreAnchoring[AnnotatedLabel, String]]
    val models = model.filterNot(_ eq null) map {
      model =>
        model.make(train): ModelType
    }

    val featureCounter = readWeights(oldWeights)

    new EPModel(ep.maxIterations, {
      featureCounter.get(_)
    }, models: _*) with EPParser.Extractor[AnnotatedLabel, String] with Serializable {
      def grammar = xbarGrammar

      def lexicon = xbarLexicon
    }
  }
}

