package epic.framework

import breeze.linalg._
import breeze.stats.distributions.Rand
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import com.typesafe.scalalogging.log4j.Logging

class StructSVM[Datum](model: Model[Datum],
                       maxIter: Int = 100,
                       batchSize: Int = 100,
                       maxSMOIterations: Int = 100,
                       C: Double = 100) extends Logging {

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
        val smoTol = if(i < 5) math.pow(10, -(i + 1)) else 1E-6
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


  private case class Constraint(ec: ExpectedCounts) {
    lazy val (loss, feats) = model.expectedCountsToObjective(ec)
    lazy val ftf = feats dot feats
    var age = 0
  }


  private def findNewConstraints(inf: model.Inference, data: IndexedSeq[Datum]): GenTraversableOnce[Constraint] = {
    for {
      d <- data.par
      ec = inf.expectedCounts(d, inf.emptyCounts, 1.0)
      if ec.loss > 0
    } yield Constraint(ec)

  }

  private def removeOldConstraints(alphas: DenseVector[Double],
                                   constraints: IndexedSeq[Constraint]):(DenseVector[Double], IndexedSeq[Constraint]) = {
    val newAlphas = Array.newBuilder[Double]
    val newConstraints = new ArrayBuffer[Constraint]()
    for( i <- 0 until alphas.length) {
      if(alphas(i).abs < 1E-5) constraints(i).age += 1
      else constraints(i).age = 0

      if(constraints(i).age < MAX_CONSTRAINT_AGE) {
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
    if(alphas.sum < C) {
      alphas += (C-alphas.sum)/alphas.length
    }
    weights := 0.0
    for(i <- 0 until alphas.length) {
      axpy(alphas(i), constraints(i).feats, weights)
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
        if( (oldA1 != 0 && oldA2 != 0)) {
          val con2 = constraints(j)
          var t = ((con1.loss - con2.loss) - ( (weights dot con2.feats) - (weights dot con1.feats)))/(con1.ftf + con2.ftf)
          val tt = t
          if(!t.isNaN && t != 0.0) {
            t = t max (-oldA1)
            val newA1 = (oldA1 + t) min (oldA1 + oldA2)
            val newA2 = (oldA2 - t) max 0
            alphas(i) = newA1
            alphas(j) = newA2
            println(newA1,newA2, tt, t, oldA1, oldA2)
            axpy(oldA1 - newA1, con1.feats, weights)
            axpy(oldA2 - newA2, con2.feats, weights)
            largestChange = largestChange max (oldA1 - newA1).abs
            largestChange = largestChange max (oldA2 - newA2).abs
          }
        }
      }

    }

  }

}
