package scalanlp.parser.epic

import scalanlp.optimize.BatchDiffFunction
import scalanlp.util.Index
import collection.GenTraversable
import scalala.tensor.sparse.SparseVector
import scalala.tensor.dense.{DenseVectorCol, DenseVector}
import actors.threadpool.AtomicInteger


trait Model[Datum] { self =>
  type ExpectedCounts <: scalanlp.parser.epic.ExpectedCounts[ExpectedCounts]
  type Inference <: scalanlp.parser.epic.Inference[Datum] {
    type ExpectedCounts = self.ExpectedCounts
  }

  def numFeatures: Int

  def inferenceFromWeights(weights: DenseVector[Double]):Inference

  def emptyCounts: ExpectedCounts
  def expectedCountsToObjective(ecounts: ExpectedCounts):(Double,DenseVectorCol[Double])
}
/**
 * 
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

  def initialWeightVector: DenseVector[Double] = DenseVector.rand(numFeatures)


  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
    val model = inferenceFromWeights(x)
    var success = new AtomicInteger(0)
    val finalCounts = select(batch).aggregate(emptyCounts)({ (countsSoFar,datum) =>
      try {
        val counts = model.expectedCounts(datum) += countsSoFar
        success.incrementAndGet()
        counts
      } catch {
        case e =>
          new Exception("While processing " + datum, e).printStackTrace()
          countsSoFar
      }
    },{ (a,b) => b += a})

    val (loss,grad) = expectedCountsToObjective(finalCounts)
    (loss/success.intValue(),  grad / success.doubleValue())
  }
}

trait ExpectedCounts[Self<:ExpectedCounts[Self]] { this:Self =>
  def +=(other: Self):Self
  def -=(other: Self):Self
  def loss: Double
}
