package epic.coref

import epic.framework._
import breeze.linalg._
import breeze.linalg.DenseVector
import breeze.util.Index
import java.util
import collection.{immutable, mutable}
import collection.mutable.ArrayBuffer
import collection.immutable.IndexedSeq

/**
 *
 * @author dlwh
 */

class PairwiseModel(feat: PairwiseFeaturizer, val featureIndex: Index[Feature]) extends Model[IndexedCorefInstance] {
  type Inference = PairwiseInference
  type ExpectedCounts = ECounts

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    new PairwiseInference(feat, weights)
  }

  def emptyCounts: PairwiseModel#ExpectedCounts = ECounts(0.0, DenseVector.zeros[Double](featureIndex.size))

  def expectedCountsToObjective(ecounts: PairwiseModel#ExpectedCounts): (Double, DenseVector[Double]) = {
    ecounts.loss -> ecounts.counts
  }
}

case class ECounts(var loss: Double, counts: DenseVector[Double]) extends ExpectedCounts[ECounts] {
  def +=(that: ECounts): ECounts =  {this.loss += that.loss; this.counts += that.counts; this}
  def -=(that: ECounts): ECounts =  {this.loss -= that.loss; this.counts -= that.counts; this}
}


class PairwiseInference(feat: PairwiseFeaturizer,
                        weights: DenseVector[Double]) extends GoldGuessInference[IndexedCorefInstance] {
  type ExpectedCounts = ECounts


  /*
  def expectedCounts(inst: IndexedCorefInstance) = {
    var ll = 0.0
    var n = 0
    val goldAssignments = new Array[BitSet](inst.numMentions + 1)
    for(cluster <- inst.goldClusters; i <- cluster) {
      assert(goldAssignments(i) == null)
      goldAssignments(i) = cluster
    }
    assert(goldAssignments(0) == null)
    val ecounts = DenseVector.zeros[Double](weights.size)
    for(i <- 1 to inst.numMentions) {
      val scores = DenseVector.zeros[Double](i)
      val goldScores = DenseVector.zeros[Double](i)
      util.Arrays.fill(goldScores.data, Double.NegativeInfinity)
      assert(goldAssignments(i) != null, i + " " + " :(")
      for(j <- 0 until i) {
        scores(j) = weights dot inst.featuresFor(j, i)
        if(j == 0 || goldAssignments(i)(j)) {
          goldScores(j) = scores(j)
        }
      }
      val sm = softmax(scores)
      val gsm = softmax(goldScores)
      ll += sm
      ll -= gsm
      println("Gg: " + sm + " " + gsm)

      for(j <- 0 until i) {
        ecounts +=  inst.featuresFor(j, i) * math.exp(scores(j) - sm)
        if(j == 0 || goldAssignments(i)(j)) {
          ecounts -= inst.featuresFor(j, i) * math.exp(goldScores(j) - gsm)
        }
        n += 1
      }
    }
    new ECounts(ll, ecounts)
  }
  */

  def goldCounts(inst: IndexedCorefInstance) = {
    var ll = 0.0
    val ecounts = DenseVector.zeros[Double](weights.size)
    for(cluster <- inst.goldClusters) {
      var isFirst = true
      for(i <- cluster) {
        if(isFirst) {
          ll += weights dot inst.featuresFor(0, i)
          ecounts += inst.featuresFor(0, i)
          isFirst = false
        } else {
          val scores = DenseVector.zeros[Double](i)
          util.Arrays.fill(scores.data, Double.NegativeInfinity)
          for(j <- cluster if j < i) {
            scores(j) = weights dot inst.featuresFor(j, i)
          }
          val sm = softmax(scores)
          ll += sm

          for(j <- cluster if j < i) {
            addIntoScale(ecounts, inst.featuresFor(j, i), math.exp(scores(j) - sm))
          }
        }
      }


    }
    new ECounts(ll, ecounts)
  }

  def guessCounts(inst: IndexedCorefInstance) = {
    var ll = 0.0
    val ecounts = DenseVector.zeros[Double](weights.size)
    for(i <- 1 to inst.numMentions) {
      val scores = DenseVector.zeros[Double](i)
      for(j <- 0 until i) {
        scores(j) = weights dot inst.featuresFor(j, i)
      }
      val sm = softmax(scores)
      ll += sm

      for(j <- 0 until i) {
        addIntoScale(ecounts, inst.featuresFor(j, i), math.exp(scores(j) - sm))
      }
    }
    new ECounts(ll, ecounts)
  }

  private def addIntoScale(v: DenseVector[Double], sv: SparseVector[Double], scale: Double) {
    if(scale != 0)  {
      var i = 0
      val bi = sv.index
      val bd = sv.data
      val ad = v.data
      val aoff = v.offset
      val astride = v.stride
      while(i < sv.used) {
        ad(aoff + astride * bi(i)) += bd(i) * scale
        i += 1
      }
    }
  }

  def decode(inst: IndexedCorefInstance): IndexedSeq[Set[MentionCandidate]] = {
    val links = Array.fill(inst.numMentions + 1)(null:mutable.BitSet)
    val trueClusters = ArrayBuffer[mutable.BitSet]()
    for(i <- 1 to inst.numMentions) {
      var maxJ = -1
      var maxScore = Double.NegativeInfinity
      for(j <- 0 until i) {
        val score = weights dot inst.featuresFor(j, i)
        if(score > maxScore) {
          maxJ = j
          maxScore = score
        }

      }
      if(maxJ == 0) {
        links(i) = mutable.BitSet(i)
        trueClusters += links(i)
      } else {
        links(i) = links(maxJ)
        links(i) += i
      }
    }

    {for(cluster <- trueClusters) yield {
      for(i <- cluster) yield inst.unindexed.mentions(i-1)
    }.toSet}.toIndexedSeq
  }
}
