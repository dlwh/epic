package scalanlp.parser.epic

import scalanlp.epic._
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.parser.{Parser, TreeInstance}
import scalanlp.parser.ParseEval.Statistics
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.{RandomizedGradientCheckingFunction, BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}
import scalanlp.trees.AnnotatedLabel

object ParserPipeline extends scalanlp.parser.ParserPipeline {
  case class Params(modelFactory: ParserExtractableModelFactory[AnnotatedLabel,String],
                    opt: OptParams,
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 1002,
                    iterPerValidate: Int = 10,
                    randomize: Boolean = false);
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: (Parser[AnnotatedLabel, String]) => Statistics, params: Params) = {
    import params._

    val model = modelFactory.make(trainTrees)

    val obj = new ModelObjective(model,trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-4, toString = {(i:Int) => model.featureIndex.get(i).toString})
    val init = obj.initialWeightVector(randomize)

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair
      val weights = state.x
      if(iter % iterPerValidate == 0) {
        println("Validating...")
        val parser = model.extractParser(weights)
        println(validate(parser))
      }
    }

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }
  }
}