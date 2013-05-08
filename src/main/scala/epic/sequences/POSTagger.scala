package epic.sequences

import breeze.optimize.FirstOrderMinimizer.OptParams
import java.io._
import epic.trees.{AnnotatedLabel, ProcessedTreebank}
import breeze.config.CommandLineParser
import breeze.util.Encoder

/**
 *
 * @author dlwh
 */
object POSTagger {
  case class Params(opt: OptParams, treebank: ProcessedTreebank)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    import params._
    val train = treebank.trainTrees.map(_.toTaggedSequence)
    val test = treebank.devTrees.map(_.toTaggedSequence)

    val crf = CRF.buildSimple(train, AnnotatedLabel("TOP"), opt = opt)
    val inf = crf.asInstanceOf[CRFInference[_, _]]
    val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream("weights.txt")))
    Encoder.fromIndex(inf.featureIndex).decode(inf.weights).iterator foreach {case (x, v) if v.abs > 1E-6 => out.println(x -> v) case _ => }
    out.close()
    val stats = TaggedSequenceEval.eval(crf, test)
    println("Final Stats: " + stats)

  }

}


/**
 * Mostly for debugging SemiCRFs. Just uses a SemiCRF as a CRF.
 * @author dlwh
 */
object SemiPOSTagger {
  case class Params(opt: OptParams, treebank: ProcessedTreebank)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    import params._
    val train = treebank.trainTrees.map(_.toTaggedSequence.asSegmentation)
    val test = treebank.devTrees.map(_.toTaggedSequence.asSegmentation)

    val crf = SemiCRF.buildSimple(train, AnnotatedLabel("TOP"), AnnotatedLabel("TOP"), opt = opt)
    val inf = crf.asInstanceOf[SemiCRFInference[_, _]]
//    val out = new PrintWriter(new BufferedOutputStream(new FileOutputStream("weights.txt")))
//    Encoder.fromIndex(inf.featureIndex).decode(inf.weights).iterator foreach {case (x, v) if v.abs > 1E-6 => out.println(x -> v) case _ => }
//    out.close()
    val stats = SegmentationEval.eval(crf, test, AnnotatedLabel("TOP"))
    println("Final Stats: " + stats)

  }

}