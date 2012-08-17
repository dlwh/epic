package epic.coref
import epic.framework._
import breeze.util.Index
import breeze.linalg._
import breeze.numerics._
import java.util

/*
class PropModel(val featureIndex: Index[Feature]) extends Model[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts
  type Inference = PropInference

  def initialValueForFeature(f: Feature) = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = new PropInference(weights)

  def emptyCounts = StandardExpectedCounts.zero(featureIndex)
}


class PropInference(properties: Array[Property], weights: DenseVector[Double]) extends GoldGuessInference[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts

  def goldCounts(inst: IndexedCorefInstance) = {
    var ll = Double.NegativeInfinity
    var ecounts:DenseVector[Double] = null
    var converged = false
    var iter = 0
    val maxIterations = 5

    val props = initializeProperties(inst)

    while (!converged && iter < maxIterations) {
      val lastLL = ll
      ll = 0.0
      ecounts = DenseVector.zeros[Double](weights.size)

      for (cluster <- inst.goldClusters) {
        var isFirst = true
        for (i <- cluster) {
          if (isFirst) {
            ll += computeScore(None, Some(props(i)), inst.featuresFor(0, i))
            ecounts += inst.featuresFor(0, i)
            isFirst = false
          } else {
            val scores = DenseVector.zeros[Double](i)
            java.util.Arrays.fill(scores.data, Double.NegativeInfinity)
            for (j <- cluster if j < i) {
              scores(j) = computeScore(Some(props(i)), Some(props(j)), inst.featuresFor(i, j))
            }
            val sm = softmax(scores)
            ll += sm

            for (j <- cluster if j < i) {
              addIntoScale(ecounts, inst.featuresFor(j, i), math.exp(scores(j) - sm))
            }
          }
        }


      }

      converged = closeTo(lastLL, ll)
      iter += 1
    }

    new StandardExpectedCounts(ll, ecounts)
  }

  def guessCounts(value: IndexedCorefInstance) = {
    null

  }

  private def addIntoScale(v: DenseVector[Double], sv: SparseVector[Double], scale: Double) {
    if (scale != 0) {
      var i = 0
      val bi = sv.index
      val bd = sv.data
      val ad = v.data
      val aoff = v.offset
      val astride = v.stride
      while (i < sv.used) {
        ad(aoff + astride * bi(i)) += bd(i) * scale
        i += 1
      }
    }
  }

  private def initializeProperties(inst: IndexedCorefInstance):PropertyBeliefs = {
    PropertyBeliefs.forInstance(properties, inst)
  }

}

*/
