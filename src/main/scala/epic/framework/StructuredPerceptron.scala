package epic.framework

import breeze.linalg.DenseVector
import breeze.stats.distributions.Rand

/**
 * TODO
 *
 * @author dlwh
 **/
class StructuredPerceptron[Datum](model: Model[Datum], maxPasses: Int = 100, batchSize: Int = 1) {
  def train(data: IndexedSeq[Datum]) = {
    val averageWeights = DenseVector.zeros[Double](model.featureIndex.size)
    val weights = new ModelObjective(model, data).initialWeightVector(false)
    var converged = false
    val numBatches = (data.length + batchSize - 1)/batchSize
    for(i <- 0 until maxPasses if !converged) {
      for(i <- 0 until  numBatches) {
        val inf = model.inferenceFromWeights(weights)
        val batch = Rand.subsetsOfSize(data, batchSize).draw()

        val totalCounts = (for(d <- batch.par; ec = inf.expectedCounts(d, inf.emptyCounts, 1.0) if ec.loss != 0.0) yield {
          assert(ec.loss > 0)
          ec
        }).reduceOption(_ += _)

        totalCounts.foreach( weights += model.expectedCountsToObjective(_)._2)

      }

      converged = (weights - averageWeights).norm(Double.PositiveInfinity) < 1E-6

      averageWeights *= (i/(i+1).toDouble)
      averageWeights += weights
    }
  }
}
