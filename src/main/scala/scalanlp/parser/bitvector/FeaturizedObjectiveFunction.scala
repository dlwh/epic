package scalanlp.parser.bitvector

import scalala.Scalala._;
import scalala.tensor.counters.Counters
import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.DiffFunction
import scalanlp.optimize.LBFGS
import scalanlp.util.ConsoleLogging
import scalanlp.util.Index
import scalanlp.collection.mutable.Grid2
import scalanlp.collection.mutable.SparseArray
import scalanlp.data.VectorBroker
import scalanlp.util.Log

abstract class FeaturizedObjectiveFunction extends DiffFunction[Int,DenseVector]  {
  type Context;
  type Decision;
  type Feature;

  protected def decisionsForContext(c: Context): Iterator[Decision]
  protected def allContexts: Iterator[Context]
  protected def features(d: Decision, c: Context):Seq[Feature];
  protected def initialFeatureWeight(f: Feature):Double;
  /** Should compute marginal likelihood and expected counts for the data */
  protected def expectedCounts(logThetas: LogPairedDoubleCounter[Context,Decision]):(Double,PairedDoubleCounter[Context,Decision]);

  val contextIndex: Index[Context] = Index(allContexts);
  protected val contextBroker = VectorBroker.fromIndex(contextIndex);

  val (decisionIndex,indexedDecisionsForContext:Seq[Seq[Int]]) = {
    val decisionIndex = Index[Decision];
    val indexedDecisionsForContext = contextBroker.mkArray[Seq[Int]];
    for( (c,cI) <- contextIndex.pairs) {
      indexedDecisionsForContext(cI) = decisionsForContext(c).map(decisionIndex.index _).toSeq;
    }
    (decisionIndex,indexedDecisionsForContext:Seq[Seq[Int]]);
  }
  protected val decisionBroker = VectorBroker.fromIndex(decisionIndex);

  // feature grid is contextIndex -> decisionIndex -> Seq[feature index]
  val (featureIndex: Index[Feature], featureGrid: Array[SparseArray[Seq[Int]]]) = {
    val index = Index[Feature]();
    val grid = contextBroker.fillArray(decisionBroker.fillSparseArray(Seq[Int]()));
    for(cI <- 0 until contextIndex.size;
        c = contextIndex.get(cI);
        dI <- indexedDecisionsForContext(cI)) {
      val d = decisionIndex.get(dI);
      val f = features(d,c);
      if(!f.isEmpty)
        grid(cI)(dI) = f.iterator map {index.index _ } toSeq;
    }
    (index,grid:Array[SparseArray[Seq[Int]]]);
  }

  /*
  {
    for((dIs,cI) <- featureGrid.zipWithIndex;
        c = contextIndex.get(cI);
        (dI,features) <- dIs) {
      val d = decisionIndex.get(dI);
      println( (c,d) -> features.map(featureIndex.get _ ));
    }
  }
  */

  val initWeights = Counters.aggregate(featureIndex.map{ f => (f,initialFeatureWeight(f))});
  val initIndexedWeights = VectorBroker.fromIndex(featureIndex).encodeDense(initWeights);

  private def decodeMatrix(m: DenseVector): DoubleCounter[Feature] = VectorBroker.fromIndex(featureIndex).decode(m);
  private def computeLogThetas(weights: DenseVector) = {
    val thetas = LogPairedDoubleCounter[Context,Decision]
    for((dIs,cI) <- featureGrid.zipWithIndex;
        c = contextIndex.get(cI);
        cTheta = thetas(c);
        (dI,features) <- dIs) {
      val d = decisionIndex.get(dI);
      val score = sumWeights(features,weights);
      cTheta(d) = score;
    }
    LogCounters.logNormalizeRows(thetas);
  }

  private def sumWeights(indices: Seq[Int], weights: DenseVector) = {
    var i = 0;
    var sum = 0.0;
    while(i < indices.length) {
      val f = indices(i);
      sum += weights(f);
      i += 1;
    }
    sum;
  }

  override def calculate(weights: DenseVector) = {
    val logThetas = computeLogThetas(weights);
    val (marginalLogProb,eCounts) = expectedCounts(logThetas);

    val (expCompleteLogProb,grad) = computeGradient(logThetas, eCounts);
    (-marginalLogProb,grad);
  }

  // computes expComplete log Likelihood and gradient
  private def computeGradient(logThetas: LogPairedDoubleCounter[Context,Decision], eCounts: PairedDoubleCounter[Context,Decision]) = {
    val featureGrad = VectorBroker.fromIndex(featureIndex).mkDenseVector(0.0);
    var logProb = 0.0;
    // gradient is \sum_{d,c} e(d,c) * (f(d,c) - \sum_{d'} exp(logTheta(c,d')) f(d',c))
    // = \sum_{d,c} (e(d,c)  - e(*,c) exp(logTheta(d,c))) f(d,c)
    // = \sum_{d,c} margin(d,c) * f(d,c)
    //
    // e(*,c) = \sum_d e(d,c) == eCounts(c).total
    for((c,ctr) <- eCounts.rows) {
      val cI = contextIndex(c);
      val cTheta = logThetas(c);
      val logTotal = Math.log(ctr.total);
      for((d,e) <- ctr) {
        val lT = cTheta(d);
        logProb += e * lT;

        val margin = e - Math.exp(logTotal + lT);
        val dI = decisionIndex(d);

        for( f <- featureGrid(cI)(dI))
          featureGrad(f) += margin;
      }
    }

    (-logProb,-featureGrad value);
  }

  class mStepObjective(eCounts: PairedDoubleCounter[Context,Decision]) extends DiffFunction[Int,DenseVector]   {
    override def calculate(weights: DenseVector) = {
      val logThetas = computeLogThetas(weights);
      computeGradient(logThetas,eCounts);
    }

  }

  protected def postIteration(iterNumber: Int, weights: LogPairedDoubleCounter[Context,Decision]) { }

  final case class State(encodedWeights: DenseVector, marginalLikelihood: Double) {
    lazy val logThetas = computeLogThetas(encodedWeights);
  }

  def emIterations(initialWeights: DoubleCounter[Feature] = initWeights): Iterator[State] = {
    val log = Log.globalLog;

    val optimizer = new LBFGS[Int,DenseVector](90,5) with ConsoleLogging;
    val weightsIterator = Iterator.iterate(State(initIndexedWeights,Double.NegativeInfinity)) { state =>
      log(Log.INFO)("E step");
      val (marginalLogProb,eCounts) = expectedCounts(state.logThetas);
      log(Log.INFO)("M step");
      val obj = new mStepObjective(eCounts);
      val newWeights = optimizer.minimize(obj, state.encodedWeights);
      val nrm = norm(state.encodedWeights - newWeights,2) / newWeights.size;
      log(Log.INFO)("M Step finished: " + nrm);
      State(newWeights,marginalLogProb);
    }

    weightsIterator drop 1 // initial iteration is crap
  }


}
