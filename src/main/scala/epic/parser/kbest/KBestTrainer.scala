package epic.parser.kbest

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
import epic.parser._
import breeze.linalg._
import breeze.optimize._
import epic.trees.AnnotatedLabel
import breeze.config.Help
import com.typesafe.scalalogging.log4j.Logging
import epic.parser.models._
import breeze.numerics._
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.parser.ParseEval.Statistics
import breeze.util._
import breeze.util.Implicits._
import java.io.File
import epic.util.CacheBroker
import epic.parser.ParserParams.XbarGrammar
import epic.trees.annotations.StripAnnotations


/**
 * The main entry point for training discriminative parsers.
 * Has a main method inherited from ParserPipeline.
 * Use --help to see options, or just look at the Params class.
 *
 *
 */
object KBestTrainer extends epic.parser.ParserPipeline with Logging {

  case class Params(@Help(text="What parser to build. LatentModelFactory,StructModelFactory,LexModelFactory,SpanModelFactory")
                    modelFactory: ParserModelFactory[AnnotatedLabel, String],
                    cache: CacheBroker,
                    k: Int = 200,
                    parser: File = null,
                    opt: OptParams,
                    @Help(text="How often to run on the dev set.")
                    iterationsPerEval: Int = 100,
                    @Help(text="How many iterations to run.")
                    maxIterations: Int = 1002,
                    @Help(text="How often to look at a small set of the dev set.")
                    iterPerValidate: Int = 40,
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
    val kbest = {
      val parser = params.parser match {
        case null =>
          GenerativeParser.annotated(XbarGrammar(), new StripAnnotations[String](), trainTrees)
        case f =>
          readObject[SimpleChartParser[AnnotatedLabel, String]](f)
      }

      KBestParser.cached(KBestParser(parser.augmentedGrammar))(cache)
    }
    val rerankingModel = new LocalRerankingModel(model, kbest, params.k)

    val obj = new ModelObjective(rerankingModel, trainTrees, params.threads)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector(randomize)
    if(checkGradient)
      GradientTester.test(cachedObj, obj.initialWeightVector(true), toString={(i: Int) => model.featureIndex.get(i).toString})

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState, Int)) {
      val (state, iter) = pair
      val weights = state.x
      if (iter % iterPerValidate == 0) {
        logger.info("Validating...")
        val parser = model.extractParser(weights)
        val stats = validate(parser)
        logger.info("Overall statistics for validation: " + stats)
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

class LocalRerankingModel[L, W](val model: ParserModel[L, W],
                                kbest: KBestParser[L, W], k: Int = 200) extends Model[TreeInstance[L, W]] with ParserExtractable[L, W] {
  type ExpectedCounts = model.ExpectedCounts
  type Marginal = KBestListMarginal[L, W]
  type Inference = LocalRerankingInference[L, W]


  def extractParser(weights: DenseVector[Double]): Parser[L, W] = model.extractParser(weights)

  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  def featureIndex: Index[Feature] = model.featureIndex

  def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =  {
    val inf = model.inferenceFromWeights(weights)
    new LocalRerankingInference[L, W](inf, kbest, k)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    model.expectedCountsToObjective(ecounts)
  }


}

class LocalRerankingInference[L, W](val inf: ParserInference[L, W], val kbest: KBestParser[L, W], val k: Int) extends Inference[TreeInstance[L, W]] {
  type Marginal = KBestListMarginal[L, W]
  type ExpectedCounts= inf.ExpectedCounts
  def emptyCounts: ExpectedCounts = inf.emptyCounts

  /**
   * Produces the "gold marginal" which is the marginal conditioned on the output label/structure itself.
   * @param v the example
   * @return gold marginal
   */
  def goldMarginal(v: TreeInstance[L, W]):Marginal = {
    new KBestListMarginal(inf.goldMarginal(v))
  }

  /**
   * Produces the "guess marginal" which is the marginal conditioned on only the input data
   * @param v the example
   * @return gold marginal
   */
  def marginal(v: TreeInstance[L, W]):Marginal = {
    val marg = kbest.bestKParses(v.words, k).map{ case (t,_ ) => inf.goldMarginal(TreeInstance("",t, v.words))}
    KBestListMarginal(marg.head.anchoring, marg)
  }

  def countsFromMarginal(v: TreeInstance[L, W], marg: Marginal, accum: ExpectedCounts, scale: Double):ExpectedCounts = {
    marg.expectedCounts(inf.featurizer, accum.asInstanceOf[StandardExpectedCounts[Feature]], scale)
    accum
  }




}

case class KBestListMarginal[L, W](anchoring: AugmentedAnchoring[L, W],
                                   marginals: IndexedSeq[ParseMarginal[L, W]]) extends ParseMarginal[L, W] {

  def this(marg: ParseMarginal[L, W]) = this(marg.anchoring, IndexedSeq(marg))

  val probsPerTree = DenseVector(marginals.map(_.logPartition):_*)
  val logPartition: Double = softmax(probsPerTree)
  probsPerTree -= logPartition
  exp.inPlace(probsPerTree)

  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double) {
    for ((m, i) <- marginals.zipWithIndex if probsPerTree(i) >= math.exp(spanThreshold)) {
      m.visitPostorder(new AnchoredVisitor[L] {
        def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitUnaryRule(begin, end, rule, ref, score * probsPerTree(i))

        }

        def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
          spanVisitor.visitSpan(begin, end, tag, ref, score * probsPerTree(i))

        }

        def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitBinaryRule(begin, split, end, rule, ref, score * probsPerTree(i))
        }
      }, spanThreshold - math.log(probsPerTree(i)))
    }
  }
}
