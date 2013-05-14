package epic.features

import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import breeze.util.CachedHashCode
import scala.runtime.ScalaRunTime

case class OrientedNGramFeature(offset: Int, features: IndexedSeq[Feature]) extends Feature with CachedHashCode {
  override def equals(other: Any):Boolean = other match {
    case x: OrientedNGramFeature => x.hashCode == hashCode && ScalaRunTime._equals(this, x)
    case _ => false
  }
}

/**
 *
 * @author dlwh
 */
class NGramSurfaceFeaturizer[W](base: SurfaceFeaturizer[W],
                                wordNgramOrder: Int = 2,
                                spanNGramOrder: Int = 0) extends SurfaceFeaturizer[W] {
  def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = {
    new SurfaceFeatureAnchoring[W] {
      val baseAnch = base.anchor(w)
      def words: IndexedSeq[W] = w


      def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Feature] = {
        if(level == FeaturizationLevel.MinimalFeatures || wordNgramOrder <= 1) baseAnch.featuresForWord(pos, level)
        else {
          val result = ArrayBuffer[Feature]() ++= baseAnch.featuresForWord(pos, level)
          for(order <- 2 to wordNgramOrder)
            addNgramFeatures(result, pos, order, FeaturizationLevel.MinimalFeatures)
          result.toArray
        }
      }

      def featuresForSpan(begin: Int, end: Int, level: FeaturizationLevel): Array[Feature] = {
        val forSpan = baseAnch.featuresForSpan(begin, end, level)


        forSpan
      }

      def addNgramFeatures(buffer: ArrayBuffer[Feature], pos: Int, order: Int, level: FeaturizationLevel) {
        for (offset <- (-order+1) to 0) {
          val features = for( pos2 <- (pos + offset) to (pos + offset + order) ) yield baseAnch.featuresForWord(pos, if (pos == pos2) level else FeaturizationLevel.MinimalFeatures)
          val configs = allConfigurations(features).map(OrientedNGramFeature(offset, _))
          buffer ++= configs
        }
      }
    }


  }

  private def allConfigurations(seqOfSeqs: TraversableOnce[Array[Feature]]): IndexedSeq[IndexedSeq[Feature]] = {
    seqOfSeqs.foldLeft(IndexedSeq(IndexedSeq.empty[Feature]))((acc,currentFeatures) => {for(a <- acc; b <- currentFeatures) yield a :+ b})
  }


}
