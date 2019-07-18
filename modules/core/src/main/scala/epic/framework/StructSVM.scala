package epic.framework

import breeze.linalg._
import breeze.stats.distributions.Rand
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import breeze.util.SerializableLogging

/*
class StructSVM[Datum](val model: Model[Datum],
                       maxIter: Int = 100,
                       batchSize: Int = 100,
                       maxSMOIterations: Int = 100,
                       C: Double = 100) extends SerializableLogging {

  import model._


  def train(data: IndexedSeq[Datum]) = {
    val weights = new ModelObjective(model, data).initialWeightVector(randomize = true)
    var alphas = DenseVector.zeros[Double](0)
    var constraints = IndexedSeq.empty[Constraint]
    var converged = false
    val numBatches = (data.length + batchSize - 1)/batchSize
    for(i <- 0 until maxIter if !converged) {
      val newWeights = weights.copy
      for(i <- 0 until  numBatches) {
        val smoTol = if (i < 5) math.pow(10, -(i + 1)) else 1E-6
        val inf = model.inferenceFromWeights(newWeights)
        val batch = Rand.subsetsOfSize(data, batchSize).draw()
        constraints ++= findNewConstraints(inf, batch)
        alphas = DenseVector.vertcat(alphas, DenseVector.zeros[Double](constraints.size - alphas.size))

        smo(inf, newWeights, alphas, constraints, smoTol)
        val (newAlphas, newConstraints) = removeOldConstraints(alphas, constraints)
        constraints = newConstraints
        alphas = newAlphas
      }

      logger.info(s"${constraints.size} total constraints. ${alphas.findAll(_.abs > 1E-5).size} active.")

      converged = constraints.size == 0 || (weights - newWeights).norm(Double.PositiveInfinity) < 1E-6
      weights := newWeights
    }
    weights
  }


  private case class Constraint(loss: Double, gold: Datum, guess: Datum, ftf: Double) {
    def dot(w: DenseVector[Double]) = {
      val counts =  model.countsFromMarginal(d, guessMarginal)
      model.accumulateCounts(d, goldMarginal, counts, -1)
      val feats = model.expectedCountsToObjective(counts)._2
      feats dot w
    }

    lazy val ftf = {
    }
    var age = 0

    def axpy(scale: Double, weights: DenseVector[Double]) = {
      val ec = model.emptyCounts
      model.accumulateCounts(d, guessMarginal, ec, scale)
      model.accumulateCounts(d, goldMarginal, ec, -scale)
      weights += model.expectedCountsToObjective(ec)._2
    }
  }


  private def findNewConstraints(inf: model.Inference, data: IndexedSeq[Datum]): GenTraversableOnce[Constraint] = {
    for {
      d <- data.par
      guessMarginal = inf.marginal(d)
      goldMarginal = inf.goldMarginal(d)
      if guessMarginal.logPartition > goldMarginal.logPartition
    } yield {
      val counts = model.countsFromMarginal(d, guessMarginal)
      model.accumulateCounts(d, goldMarginal, counts, -1)
      val feats = model.expectedCountsToObjective(counts)._2
      val ftf = feats dot feats
      Constraint(d, gm, m, ftf)
    }

  }

  private def removeOldConstraints(alphas: DenseVector[Double],
                                   constraints: IndexedSeq[Constraint]):(DenseVector[Double], IndexedSeq[Constraint]) = {
    val newAlphas = Array.newBuilder[Double]
    val newConstraints = new ArrayBuffer[Constraint]()
    for( i <- 0 until alphas.length) {
      if (alphas(i).abs < 1E-5) constraints(i).age += 1
      else constraints(i).age = 0

      if (constraints(i).age < MAX_CONSTRAINT_AGE) {
        newConstraints += constraints(i)
        newAlphas += alphas(i)
      }
    }

    new DenseVector(newAlphas.result()) -> newConstraints
  }

  val MAX_CONSTRAINT_AGE = 50

  private def smo(inf: model.Inference,
                  weights: DenseVector[Double],
                  alphas: DenseVector[Double],
                  constraints: IndexedSeq[Constraint],
                  smoTol: Double): Unit = {
    if (alphas.sum < C) {
      alphas += (C-alphas.sum)/alphas.length
    }
    for(i <- 0 until alphas.length) {
      if (alphas(i) != 0.0) {
        constraints(i).axpy(alphas(i), weights)
      }
    }
    var largestChange = 10000.0
    for(iter <- 0 until maxSMOIterations if largestChange > smoTol) {
      largestChange = 0.0
      val perm = Rand.permutation(constraints.length).draw()
      for( i <- perm) {
        val con1 = constraints(i)
        val oldA1 = alphas(i)
        val j = perm(i)
        val oldA2 = alphas(j)
        if ( (oldA1 != 0 && oldA2 != 0)) {
          val con2 = constraints(j)
          var t = ((con1.loss - con2.loss) - ( (con2.dot(weights)) - (con1.dot(weights))))/(con1.ftf + con2.ftf)
          val tt = t
          if (!t.isNaN && t != 0.0) {
            t = t max (-oldA1)
            val newA1 = (oldA1 + t) min (oldA1 + oldA2)
            val newA2 = (oldA2 - t) max 0
            alphas(i) = newA1
            alphas(j) = newA2
            println(newA1,newA2, tt, t, oldA1, oldA2)
            con1.axpy(oldA1 - newA1, weights)
            con2.axpy(oldA2 - newA2, weights)
            largestChange = largestChange max (oldA1 - newA1).abs
            largestChange = largestChange max (oldA2 - newA2).abs
          }
        }
      }

    }

  }

}
*/
