package epic.features

import breeze.linalg._
import epic.features.SurfaceFeaturizer.{MarkedWordFeaturizer, MarkerPos}
import epic.framework.Feature

/**
 *
 * @author dlwh
 */
trait WordFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):WordFeatureAnchoring[W]

  def +(other: WordFeaturizer[W]) = (this,other) match {
    case (x:ZeroFeaturizer[_],_) => other
    case (_, x:ZeroFeaturizer[_]) => this
    case (MultiWordFeaturizer(feats),MultiWordFeaturizer(feats2)) => new MultiWordFeaturizer(feats ++ feats2)
    case (MultiWordFeaturizer(feats),_) => new MultiWordFeaturizer(feats :+ other)
    case (_,MultiWordFeaturizer(feats2)) => new MultiWordFeaturizer(this +: feats2)
    case _ => new MultiWordFeaturizer(this, other)
  }

  def *(other:WordFeaturizer[W]) = new ProductWordFeaturizer(this, other)
  def offset(i: Int) = new OffsetWordFeaturizer(this, i)
}

object WordFeaturizer {

  def goodPOSTagFeaturizer[L](counts: Counter2[L, String, Double]) = {
    val dsl = new WordFeaturizer.DSL[L](counts)
    import dsl._

    (
      unigrams(word + clss, 1)
        + bigrams(clss, 2)
        + bigrams(tagDict, 2)
        + suffixes()
        + prefixes()
        + props
      )
  }

  def goodPOSTagTransitionFeaturizer[L](counts: Counter2[L, String, Double]) = {
    val dsl = new WordFeaturizer.DSL[L](counts)
    import dsl._

    word + clss

  }

  case class DSL[L](counts: Counter2[L, String, Double],
                    commonWordThreshold: Int = 100,
                    unknownWordThreshold: Int = 2) {
    private val summedCounts = sum(counts, Axis._0)
    val word = new IdentityWordFeaturizer[String](summedCounts, unknownWordThreshold)
    val shape = new WordShapeFeaturizer(summedCounts, commonWordThreshold)
    val clss = new WordClassFeaturizer(summedCounts, commonWordThreshold)
    val tagDict = new TagDictionaryFeaturizer[L](counts, commonWordThreshold)
    val props = new WordPropertyFeaturizer(summedCounts)

    def suffixes(order: Int = 5) = new WordSuffixFeaturizer(summedCounts, suffixOrder = order, commonWordThreshold = commonWordThreshold)
    def prefixes(order: Int = 5) = new WordPrefixFeaturizer(summedCounts, prefixOrder = order, commonWordThreshold = commonWordThreshold)

    val zero = new ZeroFeaturizer[String]

    def bigrams(f: WordFeaturizer[String], offsetOrder:Int = 1) = new MultiWordFeaturizer[String]({
      for(i <- -offsetOrder until offsetOrder) yield {
        f(i) * f(i+1)
      }
    })

    def unigrams(f: WordFeaturizer[String], offsetOrder:Int = 1) = new MultiWordFeaturizer[String]({
      for(i <- -offsetOrder to offsetOrder) yield {
        if(i == 0) f else f(i)
      }
    })

    implicit class RichFeaturizer[String](f: WordFeaturizer[String]) {
      def apply[T, R](i: T)(implicit wfChanger: WordFeaturizer.Modifier[String, T, R]):R = wfChanger(f, i)
      def apply(mp: MarkerPos) = new MarkedWordFeaturizer(f, mp)
    }
  }

  /** Used in the DSL for turning a WordFeaturizer into something else */
  trait Modifier[W, T, R] {
    def apply(f: WordFeaturizer[W], t: T):R
  }

  implicit def offsetModifier[W]: Modifier[W, Int, WordFeaturizer[W]] = new Modifier[W, Int, WordFeaturizer[W]] {
    def apply(f: WordFeaturizer[W], t: Int): WordFeaturizer[W] = f offset t
  }

  implicit def rangeModifier[W]: Modifier[W, Range, WordFeaturizer[W]] = new Modifier[W, Range, WordFeaturizer[W]] {
    def apply(f: WordFeaturizer[W], r: Range): WordFeaturizer[W] = r.map(i => f.offset(i):WordFeaturizer[W]).reduceLeft(_ * _)
  }
}

class ZeroFeaturizer[W] extends WordFeaturizer[W] with SurfaceFeaturizer[W] {
  private val emptyArray = Array[Feature]()
  def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] with WordFeatureAnchoring[W] = {
    val w = words
    new SurfaceFeatureAnchoring[W] with WordFeatureAnchoring[W] {
      def words: IndexedSeq[W] = w

      def featuresForWord(pos: Int): Array[Feature] = emptyArray

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
    }
  }
}

