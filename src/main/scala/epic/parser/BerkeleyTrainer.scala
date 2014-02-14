package epic.parser

import epic.trees.annotations._
import epic.trees.{TreeInstance, AnnotatedLabel}
import epic.parser.models.BerkeleyModel
import com.typesafe.scalalogging.slf4j.Logging
import java.io.{FileWriter, PrintWriter}

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

    // Iterator.single(("Berkeley-0", initialModel.parser))

    Iterable.iterate(initialModel, params.numSplits){_.split}.zipWithIndex.map({
      case (model, i) => (s"Berkeley-$i", model.parser)
    }).iterator

  }

}
