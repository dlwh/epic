package epic.sequences

import java.io._

import breeze.config.{CommandLineParser, Configuration}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.util.SerializableLogging
import epic.trees.{AnnotatedLabel, ProcessedTreebank}

/**
 *
 * @author dlwh
 */
object TrainPosTagger extends SerializableLogging {
  case class Params(opt: OptParams, treebank: ProcessedTreebank, modelOut: File = new File("pos-model.ser.gz"))

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    import params._
    val train = treebank.trainTrees.map(_.mapLabels(_.baseAnnotatedLabel)).map(_.asTaggedSequence)
    val test = treebank.devTrees.map(_.mapLabels(_.baseAnnotatedLabel)).map(_.asTaggedSequence)

    val crf = CRF.buildSimple(train, AnnotatedLabel("TOP"), opt = opt)
    breeze.util.writeObject(modelOut, crf)
    val stats = TaggedSequenceEval.eval(crf, test)
    println("Final Stats: " + stats)
    println("Confusion Matrix:\n" + stats.confusion)

  }

}


/**
 * Mostly for debugging SemiCRFs. Just uses a SemiCRF as a CRF.
 * @author dlwh
 */
object SemiPOSTagger extends SerializableLogging {
  case class Params(opt: OptParams, treebank: ProcessedTreebank)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    import params._
    val train = treebank.trainTrees.map(_.asTaggedSequence.asSegmentation)
    val test = treebank.devTrees.map(_.asTaggedSequence.asSegmentation)

    val crf = SemiCRF.buildSimple(train, opt = opt)
    val inf = crf.asInstanceOf[SemiCRFInference[_, _]]
    // val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream("weights.txt")))
    // Encoder.fromIndex(inf.featureIndex).decode(inf.weights).iterator foreach {case (x, v) if v.abs > 1E-6 => out.println(x -> v) case _ => }
    // out.close()
    val stats = SegmentationEval.eval(crf, test)
    println("Final Stats: " + stats)
  }

}
