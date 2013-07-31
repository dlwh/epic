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
import breeze.linalg._
import collection.mutable.ArrayBuffer
import nak.inference.Factor
import breeze.util._
import epic.util.CacheBroker

/**
 *
 * @author dlwh
 */
class EPModel[Datum, Augment](maxEPIter: Int, initFeatureValue: Feature => Option[Double] = {(_:Feature) => None}, epInGold: Boolean = false)(
                              _models: EPModel.CompatibleModel[Datum, Augment]*)(implicit aIsFactor: Augment <:< Factor[Augment]) extends Model[Datum] {
  def models = _models
  type ExpectedCounts = EPExpectedCounts
  type Inference = EPInference[Datum, Augment]
  type Marginal = EPMarginal[Augment, ProjectableInference[Datum, Augment]#Marginal]

  private val offsets = models.map(_.numFeatures).unfold(0)(_ + _)
  for(i <- 0 until models.length) { println(models(i) + " " + models(i).featureIndex.size)}


  def emptyCounts = {
    val counts = for (m <- models) yield m.emptyCounts
    EPExpectedCounts(0.0, counts.toIndexedSeq)
  }


  override def accumulateCounts(datum: Datum, marg: Marginal, accum: EPExpectedCounts, scale: Double) = {
    import marg._
    for ( (inf, i) <- models.zipWithIndex) yield {
      val marg = marginals(i)
      inf.accumulateCounts(datum, marg.asInstanceOf[inf.Marginal], accum.counts(i).asInstanceOf[inf.ExpectedCounts], scale)
    }
    accum.loss += scale * marg.logPartition
  }
  def numModels = models.length

  val featureIndex: Index[Feature] = {
    val index = Index[Feature]()
    for ((m, i) <- models.zipWithIndex; f <- m.featureIndex) index.index(ComponentFeature(i, f))
    index
  }

  override def initialValueForFeature(f: Feature) = initFeatureValue(f) getOrElse {
    f match {
      case ComponentFeature(m, ff) => models(m).initialValueForFeature(ff)
      case _ => 0.0
    }
  }


  /**
   * just saves feature weights to disk as a serialized counter. The file is prefix.ser.gz
   */
  override def readCachedFeatureWeights(suffix:String=""):Option[DenseVector[Double]] = {
    var any = false
    val initWeights = DenseVector.zeros[Double](featureIndex.size)
    for(cachedWeights <- super.readCachedFeatureWeights(suffix)) {
      any = true
      initWeights := cachedWeights
    }
    for(i <- 0 until numModels) {
      val mySlice = initWeights.slice(offsets(i), offsets(i+1))
      if(mySlice.valuesIterator.exists(_ == 0)) {
        for(cw <- models(i).readCachedFeatureWeights(suffix+"-"+i)) {
          any = true
          var j = 0
          while(j < cw.length) {
            if(mySlice(j) == 0.0) {
              mySlice(j) = cw(j)
            }
            j += 1
          }
        }
      }
    }
    if(any)
      Some(initWeights)
    else
      None

  }


  /**
   * Caches the weights using the cache broker.
   */
  override def cacheFeatureWeights(weights: DenseVector[Double], suffix: String) {
    super.cacheFeatureWeights(weights, suffix)
    for( (m, i) <- models.zipWithIndex) m.cacheFeatureWeights(weights.slice(offsets(i), offsets(i+1)), suffix+"-"+i)
  }

  def expectedCountsToObjective(ecounts: EPModel[Datum, Augment]#ExpectedCounts) = {
    val vectors = for ((m, e) <- models zip ecounts.counts) yield m.expectedCountsToObjective(e.asInstanceOf[m.ExpectedCounts])._2
    ecounts.loss -> DenseVector.vertcat(vectors: _*)
  }

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val allWeights = partitionWeights(weights)
    val builders = ArrayBuffer.tabulate(models.length) {
      i =>
        models(i).inferenceFromWeights(allWeights(i))
    }
    new EPInference(builders, maxEPIter, epInGold = epInGold)
  }

  private def partitionWeights(weights: DenseVector[Double]): Array[DenseVector[Double]] = {
    Array.tabulate(models.length)(m => projectWeights(weights, m))
  }

  private def projectWeights(weights: DenseVector[Double], modelIndex: Int) = {
    val result = DenseVector.zeros[Double](models(modelIndex).numFeatures)
    for (i <- 0 until result.size) {
      result(i) = weights(i + offsets(modelIndex))
    }
    result
  }

}

object EPModel {
  type CompatibleModel[Datum, Augment] = Model[Datum] { type Inference <: ProjectableInference[Datum, Augment]}
}

case class EPExpectedCounts(var loss: Double, counts: IndexedSeq[ExpectedCounts[_]]) extends epic.framework.ExpectedCounts[EPExpectedCounts] {
  def +=(other: EPExpectedCounts) = {
    for( (t, u) <- counts zip other.counts) {
      t.asInstanceOf[{ def +=(e: ExpectedCounts[_]):ExpectedCounts[_]}] += u
    }
    this.loss += other.loss
    this
  }

  def -=(other: EPExpectedCounts) = {
    for( (t, u) <- counts zip other.counts) {
      t.asInstanceOf[{ def -=(e: ExpectedCounts[_]):ExpectedCounts[_]}] -= u
    }
    this.loss -= other.loss
    this
  }

}

case class ComponentFeature(index: Int, feature: Feature) extends Feature