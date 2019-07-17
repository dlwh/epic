package epic.features

import epic.framework.Feature

import scala.collection.mutable.ArrayBuffer
import breeze.util.CachedHashCode
import epic.util.TwoTwelveSupport

import scala.runtime.ScalaRunTime

case class OrientedNGramFeature(offset: Int, features: IndexedSeq[Feature]) extends Feature with CachedHashCode {
  override def equals(other: Any): Boolean = other match {
    case x: OrientedNGramFeature => x.hashCode == hashCode && TwoTwelveSupport._equals(this, x)
    case _ => false
  }
}

/**
 *
 * @author dlwh
 */
class NGramWordFeaturizer[W](base: WordFeaturizer[W], wordNgramOrder: Int = 2) extends WordFeaturizer[W] {
  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = {
    new WordFeatureAnchoring[W] {
      val baseAnch = base.anchor(w)
      def words: IndexedSeq[W] = w

      def featuresForWord(pos: Int): Array[Feature] = {
        val result = ArrayBuffer[Feature]() ++= baseAnch.featuresForWord(pos)
        for(order <- 2 to wordNgramOrder)
          addNgramFeatures(result, pos, order)
        result.toArray
      }

      def addNgramFeatures(buffer: ArrayBuffer[Feature], pos: Int, order: Int) {
        for (offset <- (-order+1) to 0) {
          val features = for( pos2 <- (pos + offset) to (pos + offset + order) ) yield baseAnch.featuresForWord(pos)
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
