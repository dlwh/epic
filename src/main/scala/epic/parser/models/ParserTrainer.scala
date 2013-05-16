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
import epic.framework._
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.parser.Parser
import epic.parser.ParseEval.Statistics
import breeze.linalg._
import breeze.optimize._
import epic.trees.{TreeInstance, AnnotatedLabel}
import breeze.config.Help
import breeze.util.Implicits._


/**
 * The main entry point for training discriminative parsers.
 * Has a main method inherited from ParserPipeline.
 * Use --help to see options, or just look at the Params class.
 *
 *
 */
object ParserTrainer extends epic.parser.ParserPipeline {

  case class Params(@Help(text="What parser to build. LatentModelFactory,StructModelFactory,LexModelFactory,SpanModelFactory")
                    modelFactory: ParserExtractableModelFactory[AnnotatedLabel, String],
                    opt: OptParams,
                    @Help(text="How often to run on the dev set.")
                    iterationsPerEval: Int = 100,
                    @Help(text="How many iterations to run.")
                    maxIterations: Int = 1002,
                    @Help(text="How often to look at a small set of the dev set.")
                    iterPerValidate: Int = 10,
                    @Help(text="How many threads to use, default is to use whatever Scala thinks is best.")
                    threads: Int = -1,
                    @Help(text="Should we randomize weights? Some models will force randomization.")
                    randomize: Boolean = false,
                    @Help(text="Should we check the gradient to maek sure it's coded correctly?")
                    checkGradient: Boolean = false)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: (Parser[AnnotatedLabel, String]) => Statistics, params: Params) = {
    import params._

//    if(threads >= 1)
//      collection.parallel.ForkJoinTasks.defaultForkJoinPool.setParallelism(params.threads)

    val model = modelFactory.make(trainTrees)

    val obj = new ModelObjective(model, trainTrees, params.threads)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector(randomize)
    if(checkGradient)
      GradientTester.test(cachedObj, obj.initialWeightVector(true), toString={(i: Int) => model.featureIndex.get(i).toString})

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState, Int)) {
      val (state, iter) = pair
      val weights = state.x
      if (iter % iterPerValidate == 0) {
        println("Validating...")
        val parser = model.extractParser(weights)
        println(validate(parser))
      }
    }

    for ((state, iter) <- params.opt.iterations(cachedObj, init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x)
      (s"LatentDiscrim-$iter", parser)
    } catch {
      case e: Exception => e.printStackTrace(); throw e
    }
  }
}