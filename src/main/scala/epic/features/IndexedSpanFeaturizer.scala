package epic
package features

import epic.sequences.Gazetteer
import breeze.util.Index
import epic.framework.Feature
import breeze.linalg._
import scala.collection.mutable.ArrayBuffer
import breeze.collection.mutable.{OpenAddressHashArray, TriangularArray}
import epic.constraints.{SpanConstraints, LabeledSpanConstraints}
import epic.lexicon.Lexicon
import epic.util.{Has2, Has}

/**
 *
 * @author dlwh
 */
class IndexedSpanFeaturizer[-Datum:Has[SpanConstraints]#R:Has[IndexedSeq[String]]#R](val featurizer: BasicSpanFeaturizer,
                                   val lexicon: Lexicon[_, String],
                                   val wordFeatureIndex: Index[Feature],
                                   val spanFeatureIndex: Index[Feature],
                                   prevCurFeature: (Int,Int)=>Int,
                                   curNextFeature: (Int,Int)=>Int,
                                   prevNextFeature: (Int,Int)=>Int,
                                   asLeftFeature: Array[Int],
                                   asRightFeature: Array[Int]) {

  def anchor(words: Datum) = new Localization(words)
  val getWords = iCanHas[IndexedSeq[String]]
  val spanConstraints = iCanHas[SpanConstraints]

  private val emptyArray = Array.empty[Int]

  class Localization(datum: Datum) extends SurfaceFeatureAnchoring[String] {
    val words = getWords(datum)
    val validSpan = spanConstraints(datum)

    def wordFeatureIndex = IndexedSpanFeaturizer.this.wordFeatureIndex
    def spanFeatureIndex = IndexedSpanFeaturizer.this.spanFeatureIndex

    def basicFeatures(pos: Int): Array[Int] = spanFeaturizer.basicFeaturesForWord(pos)
    def featuresForWord(pos: Int): Array[Int] = _featuresForWord(pos)
    def featuresForSpan(beg: Int, end: Int): Array[Int] = _featuresForSpan(beg, end)
    val spanFeaturizer = featurizer.anchor(words)
    val lexAnch = lexicon.anchor(words)

    private val _featuresForWord: IndexedSeq[Array[Int]] = 0 until words.length map { pos =>
      val basic = basicFeatures(pos)
      if(lexAnch.allowedTags(pos).size == 1) {
        basic
      } else {
        val feats = new ArrayBuffer[Int]()
        val basicLeft = basicFeatures(pos - 1)
        val basicRight = basicFeatures(pos + 1)
        feats.sizeHint((basic.length + 1) * (basicLeft.length + basicRight.length + 1) + basicLeft.length * basicRight.length)
        feats ++= spanFeaturizer.fullFeaturesForWord(pos)
        feats ++= basicLeft.map(asLeftFeature)
        feats ++= basicRight.map(asRightFeature)
        //        feats ++= inner.featuresFor(words, pos)
        for (a <- basicLeft; b <- basic) {
          val fi = prevCurFeature(a,b)
          if(fi >= 0)
            feats += fi
        }
        for (a <- basic; b <- basicRight) {
          val fi = curNextFeature(a,b)
          if(fi >= 0)
            feats += fi
        }
        for (a <- basicLeft; b <- basicRight) {
          val fi = prevNextFeature(a,b)
          if(fi >= 0)
            feats += fi
        }
        //          feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
        //          if (pos > 0 && pos < words.length - 1) {
        //            feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
        //            feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
        //          }
        feats.toArray
      }
    }

    private val _featuresForSpan = TriangularArray.tabulate(words.length+1) { (beg,end) =>
      if(beg == end || !validSpan(beg,end)) emptyArray
      else {
        spanFeaturizer.featuresForSpan(beg, end).map(spanFeatureIndex(_)).filter(_ != -1)
      }
    }

  }
}

object IndexedSpanFeaturizer {
  def forTrainingSet[Datum:Has[SpanConstraints]#R:Has[IndexedSeq[String]]#R, L](corpus: Iterable[Datum],
                        lexicon: Lexicon[L, String],
                        gazetteer: Gazetteer[Any, String] = Gazetteer.empty,
                        noShapeThreshold: Int = 100):IndexedSpanFeaturizer[Datum] = {
    val getWords = iCanHas[IndexedSeq[String]]
    val spanConstraints = iCanHas[SpanConstraints]

    val wordCounts = Counter.countTraversable(corpus.iterator.flatMap(getWords(_))).mapValues(_.toDouble)
    val featureIndex = Index[Feature]()
    val spanFeatureIndex = Index[Feature]()

    val feat = new BasicSpanFeaturizer(new BasicWordFeaturizer(wordCounts, gazetteer, noShapeThreshold))
    val basicFeatureIndex = feat.wordFeatureIndex
    basicFeatureIndex.foreach(featureIndex.index _)

    // for left and right
    val asLeftFeatures = new Array[Int](basicFeatureIndex.size)
    val asRightFeatures = new Array[Int](basicFeatureIndex.size)

    for( (f, fi) <- basicFeatureIndex.pairs) {
      asLeftFeatures(fi) = featureIndex.index(PrevWordFeature(f))
      asRightFeatures(fi) = featureIndex.index(NextWordFeature(f))
    }

    val prevCurBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))
    val curNextBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))
    val prevNextBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))

    for( datum <- corpus) {
      val words = getWords(datum)
      val validSpan = spanConstraints(datum)
      val anch = feat.anchor(words)
      val lexAnch = lexicon.anchor(words)

      def bf(pos: Int) =  anch.basicFeaturesForWord(pos)

      // words
      for(pos <- 0 until words.length) {
        val basic = bf(pos)
        if(lexAnch.allowedTags(pos) == 1) {
          basic
        } else {
          val basicLeft = bf(pos - 1)
          val basicRight = bf(pos + 1)
          for (a <- basicLeft; b <- basic)  prevCurBigramFeatures(a)(b) = featureIndex.index(BigramFeature(featureIndex.get(asLeftFeatures(a)),featureIndex.get(b)))
          for (a <- basic; b <- basicRight) curNextBigramFeatures(a)(b) = featureIndex.index(BigramFeature(featureIndex.get(a),featureIndex.get(asRightFeatures(b))))
          for (a <- basicLeft; b <- basicRight) prevNextBigramFeatures(a)(b) = featureIndex.index(BigramFeature(featureIndex.get(asLeftFeatures(a)), featureIndex.get(asRightFeatures(b))))
          //          feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
          //          if (pos > 0 && pos < words.length - 1) {
          //            feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
          //            feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
          //          }
        }
      }

      // spans
      for( begin <- 0 until words.length; end <- (begin+1) to words.length if validSpan.isAllowedSpan(begin, end)) {
        anch.featuresForSpan(begin, end).foreach(spanFeatureIndex.index(_))
      }

    }

    new IndexedSpanFeaturizer(feat,
    lexicon,
    featureIndex, spanFeatureIndex,
    {(p,c) =>prevCurBigramFeatures(p)(c)},
    {(c,r) =>curNextBigramFeatures(c)(r)},
    {(p,r) =>prevNextBigramFeatures(p)(r)},
    asLeftFeatures, asRightFeatures)

  }
}
