package epic.framework

import breeze.linalg._
import breeze.stats.distributions.Rand
import breeze.util.Encoder

class StructSVM[Datum](model: Model[Datum], maxIter: Int = 100, batchSize: Int = 100, maxSMOIterations: Int = 100) {

  def train(initWeights: DenseVector[Double]) = {
    val weights = initWeights.copy
    for(i <- 0 until maxIter) {


    }
  }

  private case class Constraint(guess: Datum, gold: Datum, feats: DenseVector[Double], loss: Double) {
    lazy val ftf = feats dot feats
  }

  private def smo(inf: model.Inference,
                  lastWeights: DenseVector[Double],
                  lastAlphas: DenseVector[Double],
                  constraints: IndexedSeq[Constraint]):(DenseVector[Double], DenseVector[Double]) = {
    val alphas = lastAlphas.copy
    val weights  = lastWeights.copy
    var largestChange = 10000.0
    for(iter <- 0 until maxSMOIterations if largestChange > 1E-4) {
      largestChange = 0.0
      val perm = Rand.permutation(constraints.length).draw()
      for( i <- perm) {
        val con1 = constraints(i)
        val oldA1 = alphas(i)
        val j = perm(i)
        val oldA2 = alphas(j)
        if(oldA1 != 0 && oldA2 != 0) {
          val con2 = constraints(j)
          var t = (con1.loss - con2.loss) - ( (weights dot con1.feats) - (weights dot con2.feats))/(con1.ftf + con2.ftf)
          if(!t.isNaN && t != 0.0) {
            t = t max (-oldA1)
            val newA1 = (oldA1 + t) min (oldA1 + oldA2)
            val newA2 = (oldA2 - t) max 0
            alphas(i) = newA1
            alphas(j) = newA2
            axpy(oldA1 - newA1, con1.feats, weights)
            axpy(oldA2 - newA2, con2.feats, weights)
            largestChange = largestChange max (oldA1 - newA1).abs
            largestChange = largestChange max (oldA2 - newA2).abs
          }
        }
      }

    }

    lastWeights -> alphas
  }

}
