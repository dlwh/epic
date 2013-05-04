package epic.features

import breeze.util.{Encoder, Interner, Index}
import epic.framework.Feature
import breeze.linalg.Counter
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import epic.sequences.Gazetteer
import scala.collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class ContextualSurfaceFeaturizer private (val wordIndex: Index[String],
                                           val featureIndex: Index[Feature],
                                           wordCounts: Counter[String, Int],
                                           basicFeatures: Array[Array[Int]],
                                           prevCurFeature: (Int,Int)=>Int,
                                           curNextFeature: (Int,Int)=>Int,
                                           prevNextFeature: (Int,Int)=>Int,
                                           boundaryFeatures: Array[Int],
                                           gazetteer: Gazetteer[Any, String] = Gazetteer.empty,
                                           needsContextFeatures: (String,Double)=>Boolean = {(w,c) => true}) extends Serializable {

  def anchor(words: Seq[String]) = new Localization(words)


  class Localization(words: Seq[String]) {
    def basicFeatures(pos: Int): Array[Int] = if(pos < 0 || pos >= words.length) boundaryFeatures else _basicFeatures(pos)
    def featuresForWord(pos: Int): Array[Int] = _featuresForWord(pos)

    val indices = words.map(wordIndex(_))
    val _basicFeatures = indices.zipWithIndex.map{ case (wi, pos) =>
      if(wi >= 0) ContextualSurfaceFeaturizer.this.basicFeatures(wi)
      else {
        val result = ArrayBuffer[Int]()
        val ci = featureIndex(WordFeature(EnglishWordClassGenerator(words(pos)), 'Basic))
        if(ci >= 0)
          result += ci

        val si = featureIndex(WordFeature(WordShapeGenerator(words(pos)), 'Basic))
        if(si >= 0)
          result += si

        result.toArray
      }
    }

    val _featuresForWord: IndexedSeq[Array[Int]] = 0 until words.length map { pos =>
      val wc = wordCounts(words(pos))
      val basic = basicFeatures(pos)
      if(!needsContextFeatures(words(pos), wc)) {
        basic
      } else {
        val feats = new ArrayBuffer[Int]()
        val basicLeft = basicFeatures(pos - 1)
        val basicRight = basicFeatures(pos + 1)
        feats.sizeHint((basic.length + 1) * (basicLeft.length + basicRight.length + 1) + basicLeft.length * basicRight.length)
        feats ++= basic
        feats ++= basicLeft
        feats ++= basicRight
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
        feats ++= gazetteer.lookupWord(words(pos)).map(f =>featureIndex(WordFeature(f, 'WordSeenInSegment))).filter(_ != -1)
        feats.toArray
      }
    }

  }
}

object ContextualSurfaceFeaturizer {
  def forTrainingSet(corpus: Iterable[IndexedSeq[String]],
                     gazetteer: Gazetteer[Any, String] = Gazetteer.empty,
                     noShapeThreshold: Int = 100,
                     needsContextFeatures: (String,Double)=>Boolean = {(w,c) => true}):ContextualSurfaceFeaturizer = {
    val wordCounts = Counter.countTraversable(corpus.iterator.flatten)
    val wordIndex = Index[String](wordCounts.keySet)
    val featureIndex, basicFeatureIndex = Index[Feature]()
    val interner = new Interner[String]
    val classes = Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > noShapeThreshold) w else interner(EnglishWordClassGenerator(w)))
    val shapes = Encoder.fromIndex(wordIndex).tabulateArray(w => if (wordCounts(w) > noShapeThreshold) w else interner(WordShapeGenerator(w)))

    val basicFeatures: Array[Array[Int]] = Array.tabulate(wordIndex.size) { i =>
      val w = wordIndex.get(i)
      val wc = wordCounts(w)
      if (wc > noShapeThreshold) IndexedSeq(w)
      else if (wc > 5) IndexedSeq(w, classes(i), shapes(i))
      else IndexedSeq(classes(i), shapes(i))
    } map {_.map(f => basicFeatureIndex.index(WordFeature(f, 'Basic))).toArray}

    val boundaryFeatures = Array(basicFeatureIndex.index(WordFeature("###", 'Basic)))

    basicFeatureIndex foreach {featureIndex.index(_)}

    // for left and right
    val initialLeftFeature = basicFeatureIndex.size
    for(WordFeature(f,'Basic) <- basicFeatureIndex) {
      featureIndex.index(WordFeature(f,'Left))
    }
    def asLeftFeatures(f: Array[Int]) = f.map(_ + initialLeftFeature)

    val initialRightFeature = featureIndex.size
    for( fi <- 0 until initialLeftFeature) {
       featureIndex.get(fi) match {
         case WordFeature(f, 'Basic) => featureIndex.index(WordFeature(f,'Right))
         case _ =>
       }
    }
    def asRightFeatures(f: Array[Int]) = f.map(_ + initialRightFeature)

    val prevCurBigramFeatures = Array.fill(basicFeatureIndex.size, basicFeatureIndex.size)(-1)
    val curNextBigramFeatures = Array.fill(basicFeatureIndex.size, basicFeatureIndex.size)(-1)
    val prevNextBigramFeatures = Array.fill(basicFeatureIndex.size, basicFeatureIndex.size)(-1)

    for(words <- corpus) {
      val indices = words.map(wordIndex(_))
      val _basicFeatures = indices.map{ case wi =>
         basicFeatures(wi)
      }

      def bf(pos: Int) =  {
        if (pos < 0 || pos >= words.length) boundaryFeatures
        else _basicFeatures(pos)
      }

      for(pos <- 0 until words.length) {
        val wc = wordCounts(words(pos))
        val basic = bf(pos)
        if(!needsContextFeatures(words(pos), wc)) {
          basic
        } else {
          val basicLeft = asLeftFeatures(bf(pos - 1))
          val basicRight = asRightFeatures(bf(pos + 1))
          for (a <- basicLeft; b <- basic)  prevCurBigramFeatures(a-initialLeftFeature)(b) = featureIndex.index(BigramFeature(featureIndex.get(a),featureIndex.get(b)))
          for (a <- basic; b <- basicRight) curNextBigramFeatures(a)(b-initialRightFeature) = featureIndex.index(BigramFeature(featureIndex.get(a),featureIndex.get(b)))
          for (a <- basicLeft; b <- basicRight) prevNextBigramFeatures(a-initialLeftFeature)(b-initialRightFeature) = featureIndex.index(BigramFeature(featureIndex.get(a), featureIndex.get(b)))
          //          feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
          //          if (pos > 0 && pos < words.length - 1) {
          //            feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
          //            feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
          //          }
          gazetteer.lookupWord(words(pos)).foreach(f => featureIndex.index(WordFeature(f, 'WordSeenInSegment)))
        }
      }

    }

    new ContextualSurfaceFeaturizer(wordIndex, featureIndex, wordCounts,
      basicFeatures,
    {(p,c) =>prevCurBigramFeatures(p-initialLeftFeature)(c)},
    {(c,r) =>curNextBigramFeatures(c)(r-initialRightFeature)},
    {(p,r) =>prevNextBigramFeatures(p-initialLeftFeature)(r-initialRightFeature)},
    boundaryFeatures,
    gazetteer, needsContextFeatures)

  }
}
