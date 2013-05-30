package epic.lexicon

import breeze.linalg._
import breeze.numerics._
import epic.features.{HashFeature, WordFeaturizer, FeatureIndex, IndexedWordFeaturizer}
import epic.framework.{StandardExpectedCounts, Feature}
import breeze.linalg.NumericOps.Arrays._
import breeze.features.FeatureVector
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.{CachedBatchDiffFunction, BatchDiffFunction}
import scala.collection.mutable.ArrayBuffer
import breeze.util.OptionIndex
import com.typesafe.scalalogging.log4j.Logging
import epic.constraints.TagConstraints

/**
 * The MaxEnt assigns tag scores using a maximum entropy (aka logistic) classifier.
 * Scores are log p(t|w). You will want to use MaxEntTagScorer.train to train one of these.
 * @author dlwh
 */
@SerialVersionUID(1L)
class MaxEntTagScorer[L, W](feat: IndexedWordFeaturizer[W],
                            lexicon: TagConstraints.Factory[L, W],
                            index: FeatureIndex[L, Feature],
                            ts: TagScorer[L, W],
                            logProbFeature: Int,
                            weights: DenseVector[Double]) extends TagScorer[L, W] with Serializable {
  def labelIndex = lexicon.labelIndex

  def anchor(w: IndexedSeq[W]): Anchoring = new Anchoring {
    def words: IndexedSeq[W] = w

    val features = feat.anchor(w)
    val allowedTags = lexicon.anchor(w)
    val baseAnch = ts.anchor(w)

    // p(tag|w)
    val scores = Array.tabulate(w.length) { i =>
      val wfeats = features.featuresForWord(i)
      val scores = Counter[L, Double]()
      for(l <- allowedTags.allowedTags(i)) {
        val myfeats = index.crossProduct(Array(l), wfeats)
        val score = weights dot new FeatureVector(myfeats)
        scores(labelIndex.get(l)) = score + weights(logProbFeature) * baseAnch.scoreTag(i,labelIndex.get(l))
      }
      logNormalize(scores)
    }

    def scoreTag(pos: Int, l: L): Double = {
      scores(pos).get(l).getOrElse(Double.NegativeInfinity)
    }
  }
}

object MaxEntTagScorer extends Logging {
  /**
   * Creatse a MaxEntTagScroer using the featurizer and data.
   * @param feat Featurizer for picking out relevant surface features
   * @param lexicon the Lexicon type
   * @param data training data
   * @param params optimization parameters
   * @tparam L label type
   * @tparam W word type
   * @return a MaxEntTagScorer
   */
  def make[L, W](feat: WordFeaturizer[W],
                 lexicon: Lexicon[L, W],
                 data: IndexedSeq[TreeInstance[L, W]],
                 params: OptParams = OptParams()) = {
    val featurizer = IndexedWordFeaturizer.fromData(feat, data.map(_.words))
    val featureCounts = ArrayBuffer[Double]()
    val featureIndex = FeatureIndex.build(lexicon.labelIndex, featurizer.featureIndex, HashFeature.Relative(1)) { addToIndex =>
      for(ti <- data.map(_.asTaggedSequence)) {
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


    val basescorer = new SimpleTagScorer(Counter2.count(data.flatMap(ti => (ti.tree.leaves.map(_.label) zip ti.words))).mapValues(_.toDouble))


    logger.info("Number of features: " + featureIndex.size)

    val (obj, logProbFeature) = makeObjective(lexicon, featurizer, featureIndex, basescorer, data)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val initialWeights = DenseVector.zeros[Double](featureIndex.size+1)
    initialWeights(logProbFeature) = 1.0
    initialWeights := params.copy(useStochastic=true, maxIterations=data.length / 100, batchSize=300).minimize(cachedObj, initialWeights)
    val weights = params.minimize(cachedObj, initialWeights)


    new MaxEntTagScorer(featurizer, lexicon, featureIndex, basescorer, logProbFeature, weights)
  }

  private def makeObjective[L, W](lexicon: Lexicon[L, W],
                                  featurizer: IndexedWordFeaturizer[W],
                                  featureIndex: FeatureIndex[L, Feature],
                                  baseScorer: TagScorer[L, W],
                                  data: IndexedSeq[TreeInstance[L, W]]) = {
    // the None feature is for the log probability. TODO hack
    val actualFeatureIndex = new OptionIndex(featureIndex)
    val logProbFeature = actualFeatureIndex(None)
    new BatchDiffFunction[DenseVector[Double]] {
      def fullRange: IndexedSeq[Int] = (0 until data.length)

      def calculate(weights: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double]) = {
        val finalCounts = batch.map(data).par.map(_.asTaggedSequence).aggregate(null: StandardExpectedCounts[Option[Feature]])({
          (_ec, ti) =>
            val countsSoFar = if (_ec ne null) _ec else StandardExpectedCounts.zero(actualFeatureIndex)

            val w = ti.words
            val features = featurizer.anchor(w)
            val allowedTags = lexicon.anchor(w)
            val baseAnch = baseScorer.anchor(w)

            for (pos <- 0 until ti.length) {
              val wfeats = features.featuresForWord(pos)
              val myTags = allowedTags.allowedTags(pos).toArray
              val myFeatures = myTags.map(l => new FeatureVector(featureIndex.crossProduct(Array(l), wfeats)))
              val goldLabel = lexicon.labelIndex(ti.label(pos))
              val baseTagScores = myTags.map(l => weights(logProbFeature) * baseAnch.scoreTag(pos, lexicon.labelIndex.get(l)))

              val scores = myFeatures.map(weights dot _)
              for(ii <- 0 until myTags.length) {
                scores(ii) += baseTagScores(ii)
              }
              val logNormalizer = softmax(scores)

              countsSoFar.loss += (logNormalizer - scores(myTags.indexOf(goldLabel)))
              // normalize and exp
              exp.inPlace(scores -= logNormalizer)
              for (iTag <- 0 until myTags.length) {
                val label = myTags(iTag)
                val myfeats = myFeatures(iTag)
                val prob = scores(iTag)
                val margin = prob - I(label == goldLabel)
                axpy(margin, myfeats, countsSoFar.counts)
                countsSoFar.counts(logProbFeature) += margin * baseTagScores(iTag)
              }
            }

            countsSoFar
        }, {
          (x, y) => if (x eq null) y else if (y eq null) x else x += y
        })

        finalCounts *= (fullRange.size * 1.0 / batch.size)
        finalCounts.toObjective
      }
    } -> logProbFeature
  }
}