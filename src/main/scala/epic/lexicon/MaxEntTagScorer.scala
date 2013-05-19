package epic.lexicon

import breeze.linalg._
import breeze.numerics._
import epic.features.{WordFeaturizer, FeatureIndex, IndexedWordFeaturizer}
import epic.framework.{StandardExpectedCounts, Feature}
import breeze.linalg.NumericOps.Arrays._
import breeze.features.FeatureVector
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.{CachedBatchDiffFunction, BatchDiffFunction}
import scala.collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class MaxEntTagScorer[L, W](feat: IndexedWordFeaturizer[W],
                            lexicon: Lexicon[L, W],
                            index: FeatureIndex[L, Feature],
                            weights: DenseVector[Double]) extends TagScorer[L, W] with Serializable {
  def labelIndex = lexicon.labelIndex

  def anchor(w: IndexedSeq[W]): Anchoring = new Anchoring {
    def words: IndexedSeq[W] = w

    val features = feat.anchor(w)
    val allowedTags = lexicon.anchor(w)

    // p(tag|w)
    val scores = Array.tabulate(w.length) { i =>
      val wfeats = features.featuresForWord(i)
      val scores = Counter[L, Double]()
      for(l <- allowedTags.allowedTags(i)) {
        val myfeats = index.crossProduct(Array(l), wfeats)
        val score = weights dot new FeatureVector(myfeats)
        scores(labelIndex.get(l)) = score
      }
      logNormalize(scores)
    }

    def scoreTag(pos: Int, l: L): Double = {
      scores(pos)(l)
    }
  }
}

object MaxEntTagScorer {
  def make[L, W](feat: WordFeaturizer[W],
                 lexicon: Lexicon[L, W],
                 data: IndexedSeq[TreeInstance[L, W]],
                 params: OptParams = OptParams()) = {
    val featurizer = IndexedWordFeaturizer.fromData(feat, data.map(_.words))
    val featureCounts = ArrayBuffer[Double]()
    val featureIndex = FeatureIndex.build(lexicon.labelIndex, featurizer.wordFeatureIndex, 20000) { addToIndex =>
      for(ti <- data.map(_.asTaggedSequence)) {
        val lexanch = lexicon.anchor(ti.words)
        val featanch = featurizer.anchor(ti.words)
        for (i <- 0 until ti.words.length) {
          val features = featanch.featuresForWord(i)
          val indexed = addToIndex(Array(lexicon.labelIndex(ti.label(i))),features)
          for(x <- indexed.view.reverse) {
            if(x >= featureCounts.length) {
              featureCounts ++= Array.fill(x-featureCounts.length + 1)(0.0)
            }
            featureCounts(x) += 1
          }
        }

      }
    }


    println("Number of features: " + featureIndex.size)

    val obj = makeObjective(lexicon, featurizer, featureIndex, data)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val initialWeights = DenseVector.zeros[Double](featureIndex.size)
    initialWeights(0 until (initialWeights.size - 20000)) := new DenseVector(featureCounts.toArray)
    initialWeights((initialWeights.size - 20000) until initialWeights.size) := -3.0
    initialWeights /= initialWeights.norm(2)
    val weights = params.minimize(cachedObj, initialWeights)


    new MaxEntTagScorer(featurizer, lexicon, featureIndex, weights)
  }

  private def makeObjective[W, L](lexicon: Lexicon[L, W], featurizer: IndexedWordFeaturizer[W], featureIndex: FeatureIndex[L, Feature], data: IndexedSeq[TreeInstance[L, W]]): BatchDiffFunction[DenseVector[Double]] with Object {def fullRange: IndexedSeq[Int]; def calculate(weights: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double])} = {
    new BatchDiffFunction[DenseVector[Double]] {
      def fullRange: IndexedSeq[Int] = (0 until data.length)

      def calculate(weights: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double]) = {
        val finalCounts = fullRange.map(data).par.map(_.asTaggedSequence).aggregate(null: StandardExpectedCounts[Feature])({
          (_ec, ti) =>
            val countsSoFar = if (_ec ne null) _ec else StandardExpectedCounts.zero(featureIndex)

            val w = ti.words
            val features = featurizer.anchor(w)
            val allowedTags = lexicon.anchor(w)

            for (i <- 0 until ti.length) {
              val wfeats = features.featuresForWord(i)
              val myTags = allowedTags.allowedTags(i).toArray
              val myFeatures = myTags.map(l => new FeatureVector(featureIndex.crossProduct(Array(l), wfeats)))
              val goldLabel = lexicon.labelIndex(ti.label(i))

              val scores = myFeatures.map(weights dot _)
              val logNormalizer = softmax(scores)

              countsSoFar.loss += (logNormalizer - scores(myTags.indexOf(goldLabel)))
              // normalize and exp
              exp.inPlace(scores -= logNormalizer)
              for (ii <- 0 until myTags.length) {
                val label = myTags(ii)
                val myfeats = myFeatures(ii)
                val prob = scores(ii)
                axpy(prob - I(label == goldLabel), myfeats, countsSoFar.counts)
              }
            }

            countsSoFar
        }, {
          (x, y) => if (x eq null) y else if (y eq null) x else x += y
        })

        finalCounts.toObjective
      }
    }
  }
}