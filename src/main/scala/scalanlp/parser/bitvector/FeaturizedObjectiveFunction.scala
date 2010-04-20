package scalanlp.parser.bitvector

import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.DiffFunction
import scalanlp.util.Index
import scalanlp.collection.mutable.Grid2
import scalanlp.data.VectorBroker

trait FeaturizedObjectiveFunction extends DiffFunction[Int,DenseVector]  {
  type Context;
  type Decision;
  type Feature;

  protected def allDecisions: Iterator[Decision]
  protected def allContexts: Iterator[Context]
  protected def features(d: Decision, c: Context):Seq[Feature];
  protected def expectedCounts(logThetas: LogPairedDoubleCounter[Context,Decision]):PairedDoubleCounter[Context,Decision];

  val decisionIndex: Index[Decision] = Index(allDecisions);
  val contextIndex: Index[Context] = Index(allContexts);
  val (featureIndex: Index[Feature], featureGrid: Grid2[Seq[Int]]) = {
    val index = Index[Feature]();
    val grid = new Grid2[Seq[Int]](decisionIndex.size,contextIndex.size,Seq.empty);
    for(dI <- 0 until decisionIndex.size; cI <- 0 until contextIndex.size) {
      val d = decisionIndex.get(dI);
      val c = contextIndex.get(cI);
      val f = features(d,c);
      grid(dI,cI) = f map (index.index _ );
    }
    (index,grid);
  }

  private def decodeMatrix(m: DenseVector): DoubleCounter[Feature] = VectorBroker.fromIndex(featureIndex).decode(m);
  private def computeLogThetas(weights: DoubleCounter[Feature]) = {
    val thetas = LogPairedDoubleCounter[Context,Decision]
    for(c <- contextIndex;
        d <- decisionIndex) {
      val score = (for( f <- features(d,c) iterator ) yield weights(f)).foldLeft(0.0){_+_};
      thetas(c,d) = score;
    }
    LogCounters.logNormalizeRows(thetas);
  }

  override def calculate(weights: DenseVector) = {
    val featureWeights = decodeMatrix(weights);
    val logThetas = computeLogThetas(featureWeights);

    val eCounts = expectedCounts(logThetas);

    computeGradient(logThetas, eCounts);
  }

  override def valueAt(weights: DenseVector) = calculate(weights)._1;
  override def gradientAt(weights: DenseVector) = calculate(weights)._2;

  def computeGradient(logThetas: LogPairedDoubleCounter[Context,Decision], eCounts: PairedDoubleCounter[Context,Decision]) = {
    val featureGrad = VectorBroker.fromIndex(featureIndex).mkDenseVector(0.0);
    var logProb = 0.0;
    for((c,ctr) <- eCounts.rows;
        cI = contextIndex(c);
        (d,e) <- ctr) {
      val dI = decisionIndex(d);
      logProb += e * logThetas(c,d);
      for( f <- featureGrid(cI,dI))
        featureGrad(f) += e;

      val logE = Math.log(e);
      for(dd <- ctr.activeDomain) {
        val ddI = decisionIndex(dd);
        for( f <- featureGrid(cI,ddI)) {
          featureGrad(f) -= Math.exp(logE + logThetas(c,dd));
        }
      }
    }

    (logProb,featureGrad);
  }

  class mStepObjective(eCounts: PairedDoubleCounter[Context,Decision]) extends DiffFunction[Int,DenseVector]   {
    override def calculate(weights: DenseVector) = {
      val featureWeights = decodeMatrix(weights);
      val logThetas = computeLogThetas(featureWeights);
      computeGradient(logThetas,eCounts);
    }

    override def valueAt(weights: DenseVector) = calculate(weights)._1;
    override def gradientAt(weights: DenseVector) = calculate(weights)._2;
  }


}
