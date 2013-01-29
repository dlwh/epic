package epic.srl

import java.io.File
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.config.CommandLineParser
import epic.ontonotes.ConllOntoReader
import epic.framework.ModelObjective
import breeze.optimize.{BatchDiffFunction, FirstOrderMinimizer, RandomizedGradientCheckingFunction, CachedBatchDiffFunction}
import breeze.linalg.DenseVector
import epic.sequences.{Segmentation, SegmentationEval}
import breeze.util.Implicits._

/**
 *
 *
 * @author dlwh
 */
object SemiSRLPipeline {
  case class Params(path: File,
                    modelOut: File = new File("srl.model.gz"),
                    name: String = "eval/srl",
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

    val model = new SemiSRLModelFactory("#", "O").makeModel(train)

    val obj = new ModelObjective(model, train)
    val cached = new CachedBatchDiffFunction(obj)

    val checking = new RandomizedGradientCheckingFunction[Int, DenseVector[Double]](cached, toString={ (i: Int) => model.featureIndex.get(i).toString})
//
    def eval(state: FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State) {
      val crf = model.inferenceFromWeights(state.x)

      val results = for {
        inst <- test
        guessSeg: Segmentation[String, String] = crf.viterbi(inst.words, inst.lemma, inst.pos)
      } yield SegmentationEval.evaluateExample(Set("#", "O"), guessSeg, inst.asSegmentation("O"))
      val theResult = results.reduceLeft(_ + _)
      println("Eval + " + (state.iter+1) + " " + theResult)
    }

    val finalState = params.opt.iterations(cached, obj.initialWeightVector(randomize=true)).tee(state => if((state.iter +1) % params.iterPerEval == 0) eval(state)).take(params.opt.maxIterations).last
    eval(finalState)







  }
}
