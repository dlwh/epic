package epic.parser

import epic.parser.{GenerativeParser, Parser, ParserPipeline}
import breeze.config.Help
import epic.trees.annotations._
import epic.trees.{TreeInstance, AnnotatedLabel}
import epic.trees.annotations.Markovize
import epic.trees.annotations.ForgetHeadTag
import epic.trees.annotations.PipelineAnnotator
import java.io.File
import epic.parser.models.BerkeleyModel
import epic.parser.ParserParams.XbarGrammar
import epic.parser.projections.ConstraintCoreGrammarAdaptor
import epic.constraints.CachedChartConstraintsFactory
import epic.util.CacheBroker
import com.typesafe.scalalogging.slf4j.Logging

/**
 * @author jda
 */

object BerkeleyTrainer extends epic.parser.ParserPipeline with Logging {

  case class Params(
    numSplits: Int = 1,
    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
    implicit val threads: Int = -1
  )

  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: Parser[AnnotatedLabel, String] => ParseEval.Statistics,
                  params: Params) = {

    val initialModel = BerkeleyModel.makeInitial(trainTrees, params.annotator)
    var lastModel = initialModel

    // for (iter <- 0 to params.numSplits) yield {
    //   if (iter == 0)
    //     initialModel.parser
    //   else {
    //     val toGive = BerkeleyModel.split(lastParser)
    //     lastParser = toGive
    //     (s"Berkeley-$iter", toGive.extractParser)
    //   }
    // }

    Iterable.iterate(initialModel, params.numSplits) {
      _.split
    }

    Iterator.single(("Berkeley-0", initialModel.parser))

    /*val annotatedTrees = trainTrees.map(t => annotator(t)).seq.toIndexedSeq

    val initialParser = {
      val (grammar, lexicon) = XbarGrammar().xbarGrammar(trainTrees)
      GenerativeParser.annotatedParser(grammar, lexicon, annotator, trainTrees)
    }
    val uncachedConstraints = new ParserChartConstraintsFactory[AnnotatedLabel, String](initialParser,
                                                                                       (_:AnnotatedLabel).isIntermediate)
    val cachedConstraints = new CachedChartConstraintsFactory[AnnotatedLabel, String](uncachedConstraints)
    val baseMeasure = new ConstraintCoreGrammarAdaptor(initialParser.grammar, initialParser.lexicon, cachedConstraints)

    val modelFactory = BerkeleyModelFactory()
    val model = modelFactory.make(annotatedTrees, baseMeasure)

    Iterator.single(("Berkeley-0", model.extractParser()))*/

  }

}
