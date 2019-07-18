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
import epic.inference.Factor
import collection.mutable.ArrayBuffer
import breeze.util._
import epic.util.CacheBroker
import breeze.stats.distributions.Rand

/**
 *
 * @author dlwh
 */
class EPModel[Datum, Augment <: AnyRef](maxEPIter: Int, initFeatureValue: Feature => Option[Double] = {(_:Feature) => None}, epInGold: Boolean = false, dropOutFraction: Double = 0.0)(
                              _models: EPModel.CompatibleModel[Datum, Augment]*)(implicit aIsFactor: Augment <:< Factor[Augment]) extends Model[Datum] with SerializableLogging {
  def models = _models
  type ExpectedCounts = EPExpectedCounts
  type Inference = EPInference[Datum, Augment]
  type Marginal = EPMarginal[Augment, ProjectableInference[Datum, Augment]#Marginal]
  type Scorer = EPScorer[ProjectableInference[Datum, Augment]#Scorer]

  private val offsets = models.map(_.numFeatures).unfold(0)(_ + _)
  for(i <- 0 until models.length) { println(models(i) + " " + models(i).featureIndex.size)}

  def emptyCounts = {
    val counts = for (m <- models) yield m.emptyCounts
    EPExpectedCounts(0.0, counts.toIndexedSeq)
  }

  def accumulateCounts(inf: Inference, s: Scorer, datum: Datum, marg: Marginal, accum: ExpectedCounts, scale: Double):Unit = {
    import marg._
    for ( (model, i) <- models.zipWithIndex) {
      val marg = marginals(i)
      if (marg != null)
        model.accumulateCounts(inf.inferences(i).asInstanceOf[model.Inference], s.scorers(i).asInstanceOf[model.Scorer], datum, marg.asInstanceOf[model.Marginal], accum.counts(i).asInstanceOf[model.ExpectedCounts], scale)
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
      case ComponentFeature(m, ff) => models(m).initialValueForFeature(ff.asInstanceOf[Feature])
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
      if (mySlice.valuesIterator.exists(_ == 0)) {
        for(cw <- models(i).readCachedFeatureWeights(suffix+"-"+i)) {
          any = true
          var j = 0
          while (j < cw.length) {
            if (mySlice(j) == 0.0) {
              mySlice(j) = cw(j)
            }
            j += 1
          }
        }
      }
    }
    if (any)
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

  def inferenceFromWeights(weights: DenseVector[Double]):EPInference[Datum, Augment] = inferenceFromWeights(weights, 0.0)

  def inferenceFromWeights(weights: DenseVector[Double], dropOutFraction: Double) = {
    val allWeights = partitionWeights(weights)
    val toUse = new ArrayBuffer[Int]()
    var inferences = ArrayBuffer.tabulate(models.length) { i => 
      // hack, for now.
      if (dropOutFraction > 0 && Rand.uniform.get < dropOutFraction)
        null:ProjectableInference[Datum, Augment]
      else {
        toUse += i
        models(i).inferenceFromWeights(allWeights(i))
      }
    }

    if (!inferences.exists(_ ne null)) {
      toUse.clear()
      inferences = ArrayBuffer.tabulate(models.length) { i =>
        toUse += i
        models(i).inferenceFromWeights(allWeights(i))
      }
    }

    if (dropOutFraction != 0.0) 
      logger.info("Using inferences for models " + toUse.mkString(", "))

    new EPInference(inferences, maxEPIter, epInGold = epInGold)
  }

  private def partitionWeights(weights: DenseVector[Double]): Array[DenseVector[Double]] = {
    Array.tabulate(models.length)(m => projectWeights(weights, m))
  }

  private def projectWeights(weights: DenseVector[Double], modelIndex: Int) = {
    weights(offsets(modelIndex) until offsets(modelIndex + 1)).copy
  }

}

object EPModel {
  type CompatibleModel[Datum, Augment] = Model[Datum] { type Inference <: ProjectableInference[Datum, Augment]}
}

// null for dropout!
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

case class ComponentFeature[T](index: Int, feature: T) extends Feature

case class EPScorer[Scorer](scorers: IndexedSeq[Scorer])
