package scalanlp.parser.models

import scalanlp.parser._
import java.io.File
import scalala.tensor.Counter
import scalanlp.epic._

import scalanlp.util._
import scalanlp.trees.{TreeInstance, AnnotatedLabel}

case class EPParams(iterations: Int = 5, pruningThreshold: Double = -15)

object EPParserModelFactory {
  type CompatibleFactory = ModelFactory[TreeInstance[AnnotatedLabel, String]] {
    type MyModel <: EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], DerivationScorer[AnnotatedLabel, String]]
  }
}

case class EPParserModelFactory(ep: EPParams,
                                baseParser: ParserParams.BaseParser,
                                // I realy need ot figure out how to get this into my config language...
                                model1: EPParserModelFactory.CompatibleFactory = null,
                                model2: EPParserModelFactory.CompatibleFactory = null,
                                model3: EPParserModelFactory.CompatibleFactory = null,
                                model4: EPParserModelFactory.CompatibleFactory = null,
                                model5: EPParserModelFactory.CompatibleFactory = null,
                                model6: EPParserModelFactory.CompatibleFactory = null,
                                model7: EPParserModelFactory.CompatibleFactory = null,
                                model8: EPParserModelFactory.CompatibleFactory = null,
                                oldWeights: File = null) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = (
    EPModel[TreeInstance[AnnotatedLabel, String], DerivationScorer[AnnotatedLabel, String]]
      with EPParser.Extractor[AnnotatedLabel, String]
    )

  def make(train: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(train)

    type ModelType = EPModel.CompatibleModel[TreeInstance[AnnotatedLabel, String], DerivationScorer[AnnotatedLabel, String]]
    val models = Seq(model1, model2, model3, model4, model5, model6, model7, model8).filterNot(_ eq null) map {
      model =>
        model.make(train): ModelType
    }

    val featureCounter = if (oldWeights ne null) {
      readObject[Counter[Feature, Double]](oldWeights)
    } else {
      Counter[Feature, Double]()
    }

    new EPModel(ep.iterations, {
      featureCounter.get(_)
    }, models: _*) with EPParser.Extractor[AnnotatedLabel, String] with Serializable {
      def grammar = xbarGrammar

      def lexicon = xbarLexicon
    }
  }
}

