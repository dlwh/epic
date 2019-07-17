package epic.framework

import breeze.linalg._
import breeze.stats.distributions.Rand
import java.util.concurrent.atomic.AtomicInteger
import breeze.util.SerializableLogging

/**
 * TODO
 *
 * @author dlwh
 **/
class StructuredPerceptron[Datum](model: Model[Datum], maxPasses: Int = 100, batchSize: Int = 1) extends SerializableLogging {
  def train(data: IndexedSeq[Datum]) = {
    val averageWeights = DenseVector.zeros[Double](model.featureIndex.size)
    val weights = new ModelObjective(model, data).initialWeightVector(randomize = true)
    var converged = false
    val numBatches = (data.length + batchSize - 1)/batchSize
    for(i <- 0 until maxPasses if !converged) {
      var lossThisPass = 0.0
      var numTotalBad = 0
      var numTotal = 0
      for(i <- 0 until  numBatches) {
        val inf = model.inferenceFromWeights(weights)
        val batch = Rand.subsetsOfSize(data, batchSize).draw()
        val numBad = new AtomicInteger(0)
        numTotal += batch.size

        val totalCounts = (
          for {
            d <- batch.par
            ec = model.expectedCounts(inf, d)
            if ec.loss > 0.0
          } yield {
          assert(ec.loss > 0)
          numBad.incrementAndGet()
          ec
        }).reduceOption(_ += _)

        numTotalBad += numBad.get

        for(ec <- totalCounts) {
          lossThisPass += ec.loss
          weights -= model.expectedCountsToObjective(ec)._2
          logger.info(f"this instance ${ec.loss}%.2f loss, ${numBad.get}/${batch.size} instances were not right!")
        }

        if (totalCounts.isEmpty)
          logger.info(f"this instance everything was fine!")

      }

      logger.info(f"this pass $lossThisPass%.2f loss, $numTotalBad/$numTotal instances were not right!")

      converged = norm(weights - averageWeights, Double.PositiveInfinity) < 1E-4

      averageWeights *= (i/(i+1).toDouble)
      axpy(1/(i+1).toDouble, weights, averageWeights)
    }

    averageWeights
  }
}
