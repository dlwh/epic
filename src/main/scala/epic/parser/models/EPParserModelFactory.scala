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
import epic.constraints.ChartConstraints.Factory
import epic.util.CacheBroker
import epic.lexicon.Lexicon

case class EPParams(maxIterations: Int = 5, pruningThreshold: Double = -15, dropOutFraction: Double = 0.0)

object EPParserModelFactory {
  type CompatibleFactory = ParserModelFactory[AnnotatedLabel, String] {
    type MyModel <: EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], CoreAnchoring[AnnotatedLabel, String]]
  }
}

case class EPParserModelFactory(ep: EPParams,
                                baseParser: ParserParams.XbarGrammar,
                                @Help(text="ModelFactories to use. use --model.0, --model.1, etc.")
                                model: Seq[EPParserModelFactory.CompatibleFactory],
                                @Help(text="Path to old weights used for initialization")
                                oldWeights: File = null) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = EPParserModel[AnnotatedLabel, String]


  def make(train: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String])(implicit broker: CacheBroker): MyModel = {
    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(train)

    type ModelType = EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], CoreAnchoring[AnnotatedLabel, String]]
    val models = model.filterNot(_ eq null) map {
      model =>
        model.make(train, constrainer): ModelType
    }

    val featureCounter = readWeights(oldWeights)

    new EPParserModel[AnnotatedLabel, String](xbarGrammar, xbarLexicon, ep.maxIterations, featureCounter.get, false, ep.dropOutFraction)(models:_*)
  }
}

@SerialVersionUID(1L)
class EPParserModel[L, W](val baseGrammar: BaseGrammar[L],
                          val lexicon: Lexicon[L, W],
                          maxEPIter: Int,
                          initFeatureValue: Feature => Option[Double] = {(_:Feature) => None},
                          epInGold: Boolean = false, dropOutFraction: Double = 0.0)(models:EPModel.CompatibleModel[TreeInstance[L, W], CoreAnchoring[L, W]]*) extends EPModel[TreeInstance[L, W], CoreAnchoring[L, W]](maxEPIter, initFeatureValue, epInGold, dropOutFraction)(models:_*) with EPParser.Extractor[L, W] with Serializable {


}
