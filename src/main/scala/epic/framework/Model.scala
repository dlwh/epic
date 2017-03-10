package epic.framework

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import java.io.File

import breeze.linalg._
import breeze.util.{ Index, SerializableLogging }
import epic.util.WeightsCache

/**
 * A Model represents a class for turning weight vectors into [[epic.framework.Inference]]s.
 * It's main job is to hook up with a [[epic.framework.ModelObjective]] and mediate
 * computation of ExpectedCounts and conversion to the objective that's
 * needed for optimization.
 *
 * @tparam Datum the kind of
 */
trait Model[Datum] extends SerializableLogging { self =>
  type ExpectedCounts >: Null <: epic.framework.ExpectedCounts[ExpectedCounts]
  type Marginal <: epic.framework.Marginal
  type Scorer

  type Inference <: epic.framework.Inference[Datum] {
    type Marginal = self.Marginal
    type Scorer = self.Scorer
  }

  def emptyCounts: ExpectedCounts
  def accumulateCounts(inf: Inference, s: Scorer, d: Datum, m: Marginal, accum: ExpectedCounts, scale: Double):Unit

  final def expectedCounts(inf: Inference, d: Datum, scale: Double = 1.0):ExpectedCounts = {
    val ec = emptyCounts
    accumulateCounts(inf, d, ec, scale)
    ec
  }

  final def accumulateCounts(inf: Inference, d: Datum, accum: ExpectedCounts, scale: Double):Unit = {
    val s = inf.scorer(d)
    val m = inf.marginal(s, d)
    val gm = inf.goldMarginal(s, d)
    accumulateCounts(inf, s, d, m, accum, scale)
    accumulateCounts(inf, s, d, gm, accum, -scale)
  }


  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  def featureIndex: Index[Feature]

  def numFeatures = featureIndex.size

  /**
   * Caches the weights using the cache broker.
   */
  def cacheFeatureWeights(weights: DenseVector[Double], suffix: String="") {
    WeightsCache.write(new File(weightsCacheName+suffix+".txt.gz"), featureIndex, weights)
  }

  protected def weightsCacheName = getClass.getName+".weights"

  /**
   * just saves feature weights to disk as a serialized counter. The file is prefix.ser.gz
   */
  def readCachedFeatureWeights(suffix:String=""):Option[DenseVector[Double]] = {
    val file = new File(weightsCacheName+suffix+".txt.gz")
    logger.info(s"Reading old weights from $file")
    if (file.exists)  {
      Some(WeightsCache.read(file, featureIndex))
    } else {
      None
    }
  }

  def initialValueForFeature(f: Feature): Double // = 0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference


  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double])
}



