package epic.srl

import java.io.File
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.config.CommandLineParser
import epic.ontonotes.ConllOntoReader

/**
 *
 *
 * @author dlwh
 */
object SemiSRLPipeline {
  case class Params(path: File,
                    modelOut: File = new File("ner.model.gz"),
                    name: String = "eval/ner",
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    useCorefFeatures: Boolean = false,
                    useCorefSpans: Boolean = false,
                    opt: OptParams)

  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    val (train, test) = {
      val instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
        s <- doc.sentences
        srlInstance <- s.srlInstances
      } yield srlInstance
      val train = instances.take(instances.length * 9 / 10)
      val test = instances.drop(instances.length * 9 / 10)
      (train,test)
    }






  }
}
