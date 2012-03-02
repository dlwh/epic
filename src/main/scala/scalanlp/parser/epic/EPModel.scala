package scalanlp.parser.epic

import scalala.tensor.::
import collection.mutable.ArrayBuffer
import scalanlp.inference.{Factor, ExpectationPropagation}
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.FirstOrderMinimizer.OptParams
import scalanlp.parser.ParseEval.Statistics
import scalanlp.parser.Grammar._
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalanlp.parser._
import projections.GrammarProjections
import scalala.library.Library._
import scalanlp.optimize.{BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}

/**
 * 
 * @author dlwh
 */

class EPModel[Datum, Augment](models: Model[Datum] { type Inference <: ProjectableInference[Datum,Augment]}*)(implicit aIsFactor: Augment<:<Factor[Augment]) extends Model[Datum] {
  type ExpectedCounts = EPExpectedCounts
  type Inference = EPInference[Datum, Augment]

  val numFeatures = models.map(_.numFeatures).sum

  import scalanlp.util._
  private val offsets = models.map(_.numFeatures).unfold(0)(_ + _)

  def emptyCounts = {
    val counts = for( (m: Model[Datum] { type Inference <: ProjectableInference[Datum,Augment]}) <- models.toIndexedSeq) yield m.emptyCounts
    EPExpectedCounts(0.0,counts)
  }

  def expectedCountsToObjective(ecounts: EPModel[Datum, Augment]#ExpectedCounts) = {
    val vectors = for( (m,e) <- models zip ecounts.counts) yield m.expectedCountsToObjective(e.asInstanceOf[m.ExpectedCounts])._2
    ecounts.loss -> DenseVector.vertcat(vectors:_*)
  }

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val allWeights = partitionWeights(weights)
    val builders = ArrayBuffer.tabulate(models.length) { i =>
      models(i).inferenceFromWeights(allWeights(i))
    }
    new EPInference(builders)
  }

  private def partitionWeights(weights: DenseVector[Double]): Array[DenseVector[Double]] = {
    Array.tabulate(models.length)(m => projectWeights(weights, m))
  }

  private def projectWeights(weights: DenseVector[Double], modelIndex: Int) = {
    val result = DenseVector.zeros[Double](models(modelIndex).numFeatures)
    for(i <- 0 until result.size) {
      result(i) = weights(i + offsets(modelIndex))
    }
    result
  }
}

case class EPExpectedCounts(var loss: Double, counts: IndexedSeq[ExpectedCounts[_]]) extends ExpectedCounts[EPExpectedCounts] {
  def +=(other: EPExpectedCounts) = {
    for( (t,u) <- counts zip other.counts) {
      t.asInstanceOf[{ def +=(e: ExpectedCounts[_]):ExpectedCounts[_]}] += u
    }
    this.loss += other.loss
    this
  }

  def -=(other: EPExpectedCounts) = {
    for( (t,u) <- counts zip other.counts) {
      t.asInstanceOf[{ def -=(e: ExpectedCounts[_]):ExpectedCounts[_]}] -= u
    }
    this.loss -= other.loss
    this
  }
}

case class EPInference[Datum, Augment](inferences: IndexedSeq[ProjectableInference[Datum, Augment]])(implicit aIsFactor: Augment<:<Factor[Augment]) extends AugmentableInference[Datum, Augment] {
  type ExpectedCounts = EPExpectedCounts

  def baseAugment(v: Datum) = inferences(0).baseAugment(v)

  // assume we don't need gold  to do EP, at least for now
  def goldCounts(value: Datum, augment: Augment) = {
    val counts = inferences.map(_.goldCounts(value,augment))
    EPExpectedCounts(counts.foldLeft(0.0){_ + _.loss}, counts)
  }

  def guessCounts(datum: Datum, augment: Augment) = {
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      val (marg,contributionToLikelihood) = inf.marginal(datum,augment)
      val newAugment = inf.project(datum,marg,q)
      marginals(i) = marg
      newAugment -> contributionToLikelihood
    }
    val ep = new ExpectationPropagation(project _)

    val maxEPIter = 4
    var state : ep.State = null
    val iterates = ep.inference(augment, 0 until inferences.length, inferences.map(_.baseAugment(datum)))
    var iter = 0
    var converged = false
    while(!converged && iter < maxEPIter && iterates.hasNext) {
      val s = iterates.next()
      if(state != null) {
        converged = (s.logPartition - state.logPartition).abs/math.max(s.logPartition,state.logPartition) < 1E-4
      }
      iter += 1
      print(iter +" ")
      state = s
    }

    val partition = state.logPartition

    val finalCounts = for( ((inf,f_~),i) <- (inferences zip state.f_~).zipWithIndex) yield {
      val marg = marginals(i)
      val augment = f_~
      inf.guessCountsFromMarginals(datum, marg.asInstanceOf[inf.Marginal], augment)
    }

    EPExpectedCounts(partition, finalCounts)
  }
}

object EPPipeline extends ParserPipeline {
  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    numStates: Int= 2,
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 202,
                    iterPerValidate: Int = 10);
  protected val paramManifest = manifest[Params]


  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i)
  }

  def unsplit(x: (String,Int)) = x._1

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]], validate: (Parser[String, String]) => Statistics, params: Params) = {
    import params._
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees)

    val xbarParser = parser.optParser.getOrElse {
      println("building a parser from scratch...")
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries))
      val lexicon = new SimpleLexicon(initLexicon)
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb)
    }
    val feat = new SumFeaturizer(new SimpleFeaturizer[String,String], new WordShapeFeaturizer[String](initLexicon))
    val latentFeat = new SubstateFeaturizer(feat)
    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates), unsplit)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates).iterator ) yield t2
    }

    val knownTagWords = {
      for( (t,w) <- xbarParser.lexicon.knownTagWords.toIterable; t2 <- split(t,numStates)) yield (t2,w)
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }

    val model = new LatentParserModel[String,(String,Int),String](latentFeat, ("",0), indexedProjections, knownTagWords, openTags, closedWords)
    val projector = new AnchoredRuleApproximator(xbarParser,indexedProjections,-20)
    val epModel = new EPModel(new ParserEPComponent(model, projector),new ParserEPComponent(model, projector))

    val obj = new ModelObjective(epModel,trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair
      val weights = state.x
      if(iter % iterPerValidate == 0) {
        println("Validating...")
        val parser = model.extractParser(state.x.asCol.apply(0 until model.numFeatures))
        println(validate(parser))
      }
    }

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x.asCol.apply(0 until model.numFeatures))
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }
  }
}