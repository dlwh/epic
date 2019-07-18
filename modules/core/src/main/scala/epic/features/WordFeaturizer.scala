package epic.features

import breeze.linalg._
import epic.features.SurfaceFeaturizer.{MarkedWordFeaturizer, MarkerPos}
import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

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
        + bigrams(tagDict, 2)
        + suffixes(4)
        + prefixes(4)
        + props
      )
  }

  def goodPOSTagTransitionFeaturizer[L](counts: Counter2[L, String, Double]) = {
    val dsl = new WordFeaturizer.DSL[L](counts)
    import dsl._
    clss
  }

  def apply[W](f: W=>Array[Feature]) = new TabulatedWordFeaturizer(f)

  case class DSL[L](counts: Counter2[L, String, Double],
                    commonWordThreshold: Int = 100,
                    unknownWordThreshold: Int = 2) {
    val summedCounts = sum(counts, Axis._0)
    val word = new IdentityWordFeaturizer[String](summedCounts, unknownWordThreshold)
    val shape = new WordShapeFeaturizer(summedCounts, commonWordThreshold)
    val clss = new WordClassFeaturizer(summedCounts, commonWordThreshold)
    val tagDict = new TagDictionaryFeaturizer[L](counts, commonWordThreshold)
    val props = new WordPropertyFeaturizer(summedCounts)
    val lfsuf = LongestFrequentSuffixFeaturizer(summedCounts, commonWordThreshold)

    def suffixes(order: Int = 5) = new WordSuffixFeaturizer(summedCounts, suffixOrder = order, commonWordThreshold = commonWordThreshold)
    def prefixes(order: Int = 5) = new WordPrefixFeaturizer(summedCounts, prefixOrder = order, commonWordThreshold = commonWordThreshold)

    def nextWordToRight(f: WordFeaturizer[String]): NextActualWordFeaturizer = new NextActualWordFeaturizer(f, lookRight = true)
    def nextWordToLeft(f: WordFeaturizer[String]): NextActualWordFeaturizer = new NextActualWordFeaturizer(f, lookRight = false)

    val zero = new ZeroFeaturizer[String]

    def bigrams(f: WordFeaturizer[String], offsetOrder:Int = 1) = new MultiWordFeaturizer[String]({
      for(i <- -offsetOrder until offsetOrder) yield {
        f(i) * f(i+1)
      }
    })

    def unigrams(f: WordFeaturizer[String], offsetOrder:Int = 1) = new MultiWordFeaturizer[String]({
      for(i <- -offsetOrder to offsetOrder) yield {
        if (i == 0) f else f(i)
      }
    })

    def context(f: WordFeaturizer[String], order: Int = 4) = new ContextFeaturizer[String](f, order)

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

  class TabulatedWordFeaturizer[W](f: W=>Array[Feature]) extends WordFeaturizer[W] with Serializable {
    override def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {

      override def words: IndexedSeq[W] = w

      val feats = words.map(f)

      override def featuresForWord(pos: Int): Array[Feature] = if (pos < 0 || pos >= words.length) Array() else feats(pos)
    }
  }
}

class ZeroFeaturizer[W] extends WordFeaturizer[W] with SurfaceFeaturizer[W] with Serializable {
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

class NextActualWordFeaturizer(f: WordFeaturizer[String], lookRight: Boolean, isPunct: (String=>Boolean) = _.forall(!_.isLetterOrDigit)) extends WordFeaturizer[String] with Serializable {
  val dir = if (lookRight) 'Right else 'Left
  def anchor(words: IndexedSeq[String]): WordFeatureAnchoring[String] = {
    val w = words
    new WordFeatureAnchoring[String] {
      val base = f.anchor(w)
      // one for each position
      val features: immutable.IndexedSeq[Array[Feature]] = w.indices.map { _pos =>

        var pos = _pos
        val delta = if (lookRight) 1 else -1

        val feats = new ArrayBuffer[Feature]()

        var done = false
        while (!done && pos >= 0 && pos < w.length) {
          if (isPunct(w(pos)))  {
            feats ++= base.featuresForWord(pos).map(PunctuationFeature(_, dir))
          } else {
            feats ++= base.featuresForWord(pos).map(ActualWordFeature(_, dir))
            done = true
          }
          pos += delta
        }

        if (pos < 0 || pos >= w.length)  feats ++= base.featuresForWord(pos)

        feats.toArray
      }
      def words: IndexedSeq[String] = w

      def featuresForWord(pos: Int): Array[Feature] = {
        if (pos < 0 || pos >= w.length)  base.featuresForWord(pos)
        else features(pos)
      }
    }
  }

}

case class PunctuationFeature(f: Feature, dir: Symbol) extends Feature
case class ActualWordFeature(f: Feature, dir: Symbol) extends Feature
