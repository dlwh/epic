package epic.framework

import collection.GenTraversable
import breeze.optimize.BatchDiffFunction
import breeze.linalg.DenseVector
import breeze.util.Encoder
import java.util.concurrent.atomic.AtomicInteger

/**
 * The objective function for training a [[epic.framework.Model]]. Selects
 * a batch, creates an [[epic.framework.Inference]] object using the model,
 * computes expected counts using the inference, and then turns them into
 * the objective value.
 * @author dlwh
 */
class ModelObjective[Datum](val model: Model[Datum],
                            batchSelector: IndexedSeq[Int]=>GenTraversable[Datum],
                            val fullRange: IndexedSeq[Int]) extends BatchDiffFunction[DenseVector[Double]] {
  def this(model: Model[Datum], data: IndexedSeq[Datum]) = this(model,_.par.map(data), 0 until data.length)
  import model.{ExpectedCounts => _, _}

  type Builder = model.Inference

  // Selects a set of data to use
  protected def select(batch: IndexedSeq[Int]):GenTraversable[Datum] = batchSelector(batch)

  def initialWeightVector(randomize: Boolean): DenseVector[Double] = {
   val v = Encoder.fromIndex(featureIndex).tabulateDenseVector(f => model.initialValueForFeature(f))
    if(randomize) {
      v += DenseVector.rand(numFeatures) * 1E-6
    }
    v
  }

  var iter = 0
  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
    if(iter % 30 == 0) {
      model.cacheFeatureWeights(x, "weights")
    }
    iter += 1
    val inference = inferenceFromWeights(x)
    val timeIn = System.currentTimeMillis()
    val success = new AtomicInteger(0)
    val finalCounts = select(batch).aggregate(null:inference.ExpectedCounts)({ ( _countsSoFar,datum) =>
      try {
        val countsSoFar:inference.ExpectedCounts = if (_countsSoFar ne null) _countsSoFar else inference.emptyCounts
        inference.expectedCounts(datum, countsSoFar, 1.0)
        success.incrementAndGet()
        countsSoFar
      } catch {
        case e: Exception =>
          e.printStackTrace()
//          new Exception("While processing " + datum, e).printStackTrace()
          _countsSoFar
      }
    },{ (a,b) => if(a eq null) b else if (b eq null) a else b += a})
    val timeOut = System.currentTimeMillis()
    println(f"Inference took: ${(timeOut - timeIn) * 1.0/1000}%.3fs" )

    val (loss,grad) = expectedCountsToObjective(finalCounts)
    val timeOut2 = System.currentTimeMillis()
    (loss/success.intValue() * fullRange.size,  grad * (fullRange.size * 1.0 / success.intValue))
  }
}
