package epic.coref
import epic.framework._
import breeze.util.Index
import breeze.linalg._
import breeze.numerics._
import java.util

class PropModel(propIndexer: PropIndexer, val featureIndex: Index[Feature]) extends Model[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts
  type Inference = PropInference

  def initialValueForFeature(f: Feature) = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = new PropInference(propIndexer, weights)

  def emptyCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts) =  {
    ecounts.loss -> ecounts.counts
  }
}


class PropInference(propIndexer: PropIndexer, weights: DenseVector[Double]) extends GoldGuessInference[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts

  def goldCounts(inst: IndexedCorefInstance) = {
    var ll = Double.NegativeInfinity
    var ecounts:DenseVector[Double] = null
    var converged = false
    var iter = 0
    val maxIterations = 5

    val assignmentVariables = Array.fill(inst.numMentions+1)()

    while (!converged && iter < maxIterations) {
      val lastLL = ll
      ll = 0.0
      ecounts = DenseVector.zeros[Double](weights.size)

      for (cluster <- inst.goldClusters) {
        var isFirst = true
        for (i <- cluster) {
          if (isFirst) {
//            ll += computeScore(None, Some(props(i)), inst.featuresFor(0, i))
            ecounts += inst.featuresFor(0, i)
            isFirst = false
          } else {
            val scores = DenseVector.zeros[Double](i)
            java.util.Arrays.fill(scores.data, Double.NegativeInfinity)
            for (j <- cluster if j < i) {
//              scores(j) = computeScore(Some(props(i)), Some(props(j)), inst.featuresFor(i, j))
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

  def guessCounts(inst: IndexedCorefInstance) = {
    var ll = Double.NegativeInfinity
    var ecounts:DenseVector[Double] = null
    var converged = false
    var iter = 0
    val maxIterations = 5

    val anaBeliefs = Array.tabulate(inst.numMentions + 1){i =>
      val dv = DenseVector.zeros[Double](i)
      if(i != 0)
        dv := 1.0 / i
      dv
    }
    val mainToAnaMessages = Array.tabulate(inst.numMentions + 1){i =>
      DenseVector.ones[Double](i)
    }
    val anaZ = new Array[Double](inst.numMentions + 1)

    val propBeliefs = initializeProperties(inst)

    while (!converged && iter < maxIterations) {
      val lastLL = ll
      ll = 0.0
      ecounts = DenseVector.zeros[Double](weights.size)

    }
    null
  }

  private def computeScore(beliefs: PropertyBeliefs, a: Int, b: Int, features: SparseVector[Double]):Double = {
    val baseScore = weights dot features
    val ma = beliefs.marginals(a)
    val mb = beliefs.marginals(b)
    var agreeScore =  0.0

    0.0
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
    propIndexer.initialBeliefs(inst)
  }

}

