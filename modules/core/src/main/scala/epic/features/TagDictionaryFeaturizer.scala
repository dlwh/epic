package epic.features

import epic.framework.Feature
import breeze.linalg._
import breeze.util.{Encoder, Index}
import epic.features.TagDictionaryFeaturizer._
import scala.collection.mutable

/**
 * TODO
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class TagDictionaryFeaturizer[L](counts: Counter2[L, String, Double], commonWordThreshold: Int = 80) extends WordFeaturizer[String] with Serializable {
  private val wordIndex = Index(counts.keysIterator.map(_._2))
  private val labelIndices = counts.keysIterator.map(_._1).map(l => l -> MostCommonTagFeature(l)).toMap
  private val emptyArray = Array.empty[Feature]
  private val argmaxes = Encoder.fromIndex(wordIndex).tabulateArray{w =>
    val totalCount = sum(counts(::, w))
    if (totalCount >= commonWordThreshold) {
      emptyArray
    } else if (totalCount <= 2)  {
      emptyArray
    } else {
      val feats1 = counts(::, w).iterator.filter(_._2 == totalCount).map(_._1).map(labelIndices).toArray[Feature]
      feats1
    }
  }
  private val variants = Encoder.fromIndex(wordIndex).tabulateArray{w =>
    val totalCount = sum(counts(::, w))
    if (totalCount < commonWordThreshold) {
      variantFeatures(w)
    } else emptyArray
  }

  private def variantFeatures(w: String) = {
    val arr = mutable.ArrayBuilder.make[Feature]
    if (w(0).isUpper) {
      val lowerCount = sum(counts(::, w.toLowerCase))
      if (lowerCount != 0.0) {
        arr += HasKnownLowerCaseVariant(counts(::, w.toLowerCase).argmax)
      }
    }

    val dashIndex = w.lastIndexOf('-')
    if (dashIndex >= 0) {
      val afterDash = w.substring(dashIndex)
      val undashedCount = sum(counts(::, afterDash))
      if (undashedCount != 0.0) {
        arr += HasKnownAfterDashSuffix(counts(::, afterDash).argmax)
      }
    }
    arr.result()
  }

  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    val indices = w.map(wordIndex)
    val myArgmaxes = indices.map{i =>
      if (i < 0) {
        emptyArray
      } else argmaxes(i)
    }

    val variants: IndexedSeq[Array[Feature]] = indices.zipWithIndex.map{ case(i, pos) =>
      if (i < 0) {
        variantFeatures(w(pos))
      } else {
        TagDictionaryFeaturizer.this.variants(i)
      }
    }

    def featuresForWord(pos: Int): Array[Feature] = {
      if (pos < 0 || pos >= w.length) {
        Array(IndicatorWSFeature('OutOfBounds))
      } else {
        val am = myArgmaxes(pos)
        if (variants(pos).length != 0) {
          am ++ variants(pos)
        } else {
          am
        }
      }
    }

    def words: IndexedSeq[String] = w
  }
}

object TagDictionaryFeaturizer {
  case class MostCommonTagFeature[L](l: L) extends Feature
  case class HasKnownLowerCaseVariant[L](l: L) extends Feature
  case class HasKnownAfterDashSuffix[L](l: L) extends Feature
}
