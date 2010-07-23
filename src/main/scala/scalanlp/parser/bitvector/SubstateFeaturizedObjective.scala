package scalanlp.parser.bitvector

import scala.collection.mutable.ArrayBuffer;

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalala.tensor.adaptive.AdaptiveVector;
import scalala.tensor.sparse.SparseVector;

import scalala.tensor.counters.Counters
import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.dense.DenseVector
import scalanlp.concurrent.ParallelOps._;
import scalanlp.optimize.DiffFunction
import scalanlp.optimize.LBFGS
import scalanlp.util.ConsoleLogging
import scalanlp.util.Index
import scalanlp.util.Encoder
import scalanlp.math.Numerics;
import scalanlp.collection.mutable.SparseArray
import scalanlp.util.Log

abstract class SubstateFeaturizedObjective(val regularizationConstant: Double= 0.01) extends DiffFunction[Int,DenseVector]  {

  type Context;
  type Decision;
  type Feature;

  protected def decisionsForContext(c: Context): Iterator[Decision]
  protected def numSubstatesForContext(c: Context): Int
  protected def numSubstatesForDecision(d: Decision): Int
  protected def numSubstatesForFeature(f: Feature): Int
  protected def allContexts: Iterator[Context]
  protected def features(d: Decision, c: Context):IndexedSeq[Feature];
  protected def priorForFeature(f: Feature, sub: Int):Double;
  protected def initialValueForFeature(f: Feature, sub: Int):Double;
  protected def substatesForFeature(f: Feature, contextSub: Int, decisionSub: Int):Iterator[Int];
  /** Should compute marginal likelihood and expected counts for the data */
  protected def expectedCounts(logThetas: LogPairedDoubleCounter[(Context,Int),(Decision,Int)]):(Double,PairedDoubleCounter[(Context,Int),(Decision,Int)]);


  // Methods for the objective function.
  
  override def calculate(weights: DenseVector) = {
    val encodedThetas = computeLogThetas(weights);
    val logThetas = decodeThetas(encodedThetas);
    val (marginalLogProb,eCounts) = expectedCounts(logThetas);

    val (encodedCounts,encodedTotals) = encodeCounts(eCounts);
    val (expCompleteLogProb,grad) = computeGradient(weights, encodedThetas, encodedCounts, encodedTotals);
    (-marginalLogProb,grad);
  }

  override def valueAt(weights: DenseVector) = {
    val encodedThetas = computeLogThetas(weights);
    val logThetas = decodeThetas(encodedThetas);
    val (marginalLogProb,eCounts) = expectedCounts(logThetas);

    -marginalLogProb + regularizationValue(weights);
  }

  // various indices we need:
  val rawContextIndex = Index(allContexts);
  val contextIndex = new SubstateIndex(rawContextIndex,numSubstatesForContext _ );
  protected val contextEncoder = Encoder.fromIndex(contextIndex);

  val (rawDecisionIndex,indexedDecisionsForContext:Seq[Seq[Int]]) = {
    val decisionIndex = Index[Decision];
    val indexedDecisionsForContext = contextEncoder.mkArray[Seq[Int]];
    for( (c,cI) <- contextIndex.pairs) {
      indexedDecisionsForContext(cI) = scala.util.Sorting.stableSort(decisionsForContext(c._1).map(decisionIndex.index _).toSeq);
    }
    (decisionIndex,indexedDecisionsForContext:Seq[Seq[Int]]);
  }
  protected val decisionIndex = new SubstateIndex(rawDecisionIndex,numSubstatesForDecision);
  protected val decisionEncoder = Encoder.fromIndex(decisionIndex);

  // feature grid is contextIndex -> decisionIndex -> Seq[feature index]
  val (rawFeatureIndex: Index[Feature], featureGrid: Array[SparseArray[Array[Int]]]) = {
    val index = Index[Feature]();
    val grid = contextEncoder.fillArray(decisionEncoder.fillSparseArray(Array[Int]()));
    for(cI <- 0 until contextIndex.size;
        c = contextIndex.get(cI);
        dI <- indexedDecisionsForContext(cI)) {
      val d = decisionIndex.get(dI);
      val f = features(d._1,c._1);
      if(!f.isEmpty) {
        grid(cI)(dI) = f.map(index.index).toArray.sorted;
      }
    }
    (index,grid:Array[SparseArray[Array[Int]]]);
  }
  protected val featureIndex = new SubstateIndex(rawFeatureIndex,numSubstatesForFeature _)
  protected val featureEncoder = Encoder.fromIndex(featureIndex);

  // default means and such.
  val priorMean: DenseVector = featureEncoder.tabulateDenseVector(t => priorForFeature(t._1,t._2));

  def defaultInitWeights : DenseVector = featureEncoder.tabulateDenseVector(t => initialValueForFeature(t._1,t._2)); 

  // encodes the counts as a sparsematrix of sorts and also maintains the sums of all the counts.
  private def encodeCounts(eCounts: PairedDoubleCounter[(Context,Int),(Decision,Int)]): (Array[Vector],Array[Double]) = {
    val encCounts = contextEncoder.mkArray[Vector];
    val totals = contextEncoder.mkArray[Double];
    for( (c,ctr) <- eCounts.rows) {
      val cI = contextIndex(c);
      val contextOffset = contextIndex.offsets(cI);
      val encCtr = decisionEncoder.encode(ctr);
      encCounts(cI) = encCtr;
      totals(cI) = ctr.total;
    }

    (encCounts,totals);
  }

  // Inverts the previous funciton, more or less.
  private def decodeThetas(m: Array[Vector]): LogPairedDoubleCounter[(Context,Int),(Decision,Int)] = {
    val result = LogPairedDoubleCounter[(Context,Int),(Decision,Int)];
    for( (vec,cI) <- m.iterator.zipWithIndex) {
      result(contextIndex.get(cI)) := decisionEncoder.decode(vec);
    }
    result;
  }

  // computes log p(decision,decision substate | context, context substate) by summing together feature weights.
  private def computeLogThetas(weights: DenseVector) = {
    val thetas = contextEncoder.mkArray[Vector];
    for( ((c,cSub),cI) <- contextIndex.pairs) {
      thetas(cI) = decisionEncoder.mkVector(Double.NegativeInfinity);
      val rawCI = rawContextIndex(c); // TODO: maybe infer from cI
      val rawDIs: Seq[Int] = indexedDecisionsForContext(rawCI);
      for(rawDI <- rawDIs;
          features = featureGrid(rawCI)(rawDI);
          dOffset = decisionIndex.offsets(rawDI);
          dSub <- 0 until decisionIndex.numSubstates(rawDI)) {
        val score = sumWeights(features, cSub, dSub, weights);
        thetas(cI)(dOffset + dSub) = score;
      }
    }
    
    thetas.map(Numerics.logNormalize _ );
  }

  // computes \sum_{f is a feature for (context,decision) w_f)
  // indices are over coarse features
  private def sumWeights(indices: Array[Int], contextSub: Int, decisionSub: Int, weights: DenseVector): Double = {
    var i = 0;
    var sum = 0.0;
    while(i < indices.length) {
      val rawFI = indices(i);
      val f = rawFeatureIndex.get(rawFI);
      val fOff = featureIndex.offsets(rawFI);
      for(s <- substatesForFeature(f,contextSub, decisionSub)) {
        sum += weights(fOff + s);
      }
      i += 1;
    }
    sum;
  }

  private def unwrapVector(v: Vector) = v match {
    case a: AdaptiveVector => a.innerVector;
    case _ => v;
  }

  // Computes the value of the objective
  private def computeValue(featureWeights: Vector, logThetas: Array[Vector], eCounts: Array[Vector], eTotals: Array[Double]) = {
    var logProb = 0.0;

    for( (vec,c) <- eCounts.zipWithIndex) {
      val cTheta = logThetas(c);
      unwrapVector(vec) match {
        case vec: SparseVector =>
          var i = 0;
          while(i < vec.used) {
            val d = vec.index(i);
            val e = vec.data(i);
            val lT = cTheta(d);
            logProb += e * lT;
            i += 1;
          }
        case _ =>
          for((d,e) <- vec.activeElements) {
            val lT = cTheta(d);
            logProb += e * lT;
          }
      }
    }
    -logProb + regularizationValue(featureWeights);
  }

  // computes expComplete log Likelihood and gradient
  private def computeGradient(featureWeights: Vector, logThetas: Array[Vector], eCounts: Array[Vector], eTotals: Array[Double]): (Double,DenseVector) = {
    // gradient is \sum_{d,c} e(d,c) * (f(d,c) - \sum_{d'} exp(logTheta(c,d')) f(d',c))
    // = \sum_{d,c} (e(d,c)  - e(*,c) exp(logTheta(d,c))) f(d,c)
    // = \sum_{d,c} margin(d,c) * f(d,c)
    //
    // e(*,c) = \sum_d e(d,c) == eCounts(c).total
    def featureGrad = featureEncoder.mkDenseVector(0.0);

    val zippedECounts = zipExpectedCounts(eCounts);

    val (grad:DenseVector,prob:Double) = zippedECounts.par(2000).fold( (featureGrad,0.0) ) { (gradObj,zipped) =>
      val ZippedCount(vec,c,cs,rawCI,cI) = zipped;
            
      var (featureGrad,logProb) = gradObj;
      val cTheta = logThetas(cI);
      val logTotal = math.log(eTotals(cI));
      
      unwrapVector(vec) match {
        case vec: SparseVector =>
          var i = 0;
          var rawDI = 0; // index into substate offsets of index. corresponds to a raw decision.
          while(i < vec.used) {
            val dI = vec.index(i); // decision index
            val e = vec.data(i); // count for decision

            // have we moved past the previous decision
            if(dI >= decisionIndex.offsets(rawDI+1)) {
              rawDI += 1;
            }
            // offset for this raw decision, corresponding to substate 0 for it.
            val offset = decisionIndex.offsets(rawDI);
            val ds = dI - offset; // substate for this decision.
            
            val lT = cTheta(dI);
            logProb += e * lT;

            val margin = e - math.exp(logTotal + lT);

            var j = 0;
            val grid = featureGrid(rawCI)(rawDI);
            while(j < grid.size) {
              val f = grid(j); // raw feature index
              val feat = rawFeatureIndex.get(f);
              val offsetForFeat = featureIndex.offsets(f);
              for(fs <- substatesForFeature(feat,cs,ds)) {
                featureGrad(fs + offsetForFeat) += margin;
              }
              j += 1;
            }
            i += 1;
          }
        case _ =>
        error("David fix this. TODO"); // TODO
      }
      (featureGrad,logProb)

    } { (gradObj1,gradObj2) =>
      gradObj1._1 += gradObj2._1
      (gradObj1._1, gradObj1._2 + gradObj2._2)
    }

    val realProb = - prob + regularizationValue(featureWeights);
    val finalGrad = -grad + regularizationGradient(featureWeights);

    (realProb,finalGrad value);
  }

  private case class ZippedCount(counts: Vector, context: Context, substate: Int, rawIndex: Int, index: Int);
  
  private def zipExpectedCounts(ecounts: Array[Vector]): IndexedSeq[ZippedCount] = {
    val res = new ArrayBuffer[ZippedCount](ecounts.length);
    var rawIndex = 0;
    for(i <- 0 until ecounts.length) {
      while(i < contextIndex.offsets(rawIndex) ) {
        rawIndex += 1; 
      }
      val c = rawContextIndex.get(rawIndex);
      val cs = i - contextIndex.offsets(rawIndex);
      ZippedCount(ecounts(i), c, cs, rawIndex, i);
    }
    res
  }

  private def regularizationGradient(weights: Vector):Vector = (weights - priorMean) * regularizationConstant
  private def regularizationValue(weights: Vector):Double = {
    val diff = (weights - priorMean) value;
    1.0 /2 * (diff dot diff) * regularizationConstant;
  }

  class mStepObjective(eCounts: PairedDoubleCounter[(Context,Int),(Decision,Int)]) extends DiffFunction[Int,DenseVector]   {
    val (encodedCounts,encodedTotals) = encodeCounts(eCounts);
    override def calculate(weights: DenseVector) = {
      val logThetas = computeLogThetas(weights);
      computeGradient(weights,logThetas,encodedCounts,encodedTotals);
    }

    override def valueAt(weights: DenseVector) = {
      val logThetas = computeLogThetas(weights);
      computeValue(weights,logThetas,encodedCounts,encodedTotals);
    }

  }

  final case class State(encodedWeights: DenseVector, marginalLikelihood: Double) {
    lazy val logThetas = decodeThetas(computeLogThetas(encodedWeights));
    lazy val weights = featureEncoder.decode(encodedWeights);
  }

  def emIterations(initialWeights: DoubleCounter[(Feature,Int)] = featureEncoder.decode(defaultInitWeights), maxMStepIterations: Int=90): Iterator[State] = {
    val log = Log.globalLog;

    val optimizer = new LBFGS[Int,DenseVector](maxMStepIterations,5) with ConsoleLogging;
    val weightsIterator = Iterator.iterate(State(featureEncoder.encodeDense(initialWeights),Double.NegativeInfinity)) { state =>
      val (marginalLogProb,eCounts) = expectedCounts(state.logThetas);
      val obj = new mStepObjective(eCounts);
      val newWeights = optimizer.minimize(obj, state.encodedWeights);
      val nrm = norm(state.encodedWeights - newWeights,2) / newWeights.size;
      State(newWeights,marginalLogProb);
    }

    weightsIterator drop 1 // initial iteration is crap
  }


}
