package epic.features

import breeze.util.{Encoder, Interner, Index}
import epic.framework.Feature
import breeze.linalg._
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import epic.sequences.Gazetteer
import scala.collection.mutable.ArrayBuffer
import breeze.io.TextWriter.PrintStreamWriter
import java.io.{FileWriter, PrintWriter}
import com.sun.xml.internal.ws.wsdl.writer.document.OpenAtts
import breeze.collection.mutable.OpenAddressHashArray

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class IndexedWordFeaturizer private (val featurizer: BasicWordFeaturizer,
                                     val featureIndex: Index[Feature],
                                     wordCounts: Counter[String, Double],
                                     prevCurFeature: (Int,Int)=>Int,
                                     curNextFeature: (Int,Int)=>Int,
                                     prevNextFeature: (Int,Int)=>Int,
                                     asLeftFeature: Array[Int],
                                     asRightFeature: Array[Int],
                                     needsContextFeatures: (String,Double)=>Boolean = {(w,c) => true}) extends Serializable {

  def anchor(words: IndexedSeq[String]) = new Localization(words)

  class Localization(words: IndexedSeq[String]) {
    def basicFeatures(pos: Int): Array[Int] = wordFeaturizer.basicFeatures(pos)
    def featuresForWord(pos: Int): Array[Int] = _featuresForWord(pos)
    val wordFeaturizer = featurizer.anchor(words)

    private val _featuresForWord: IndexedSeq[Array[Int]] = 0 until words.length map { pos =>
      val wc = wordCounts(words(pos))
      val basic = basicFeatures(pos)
      if(!needsContextFeatures(words(pos), wc)) {
        basic
      } else {
        val feats = new ArrayBuffer[Int]()
        val basicLeft = basicFeatures(pos - 1)
        val basicRight = basicFeatures(pos + 1)
        feats.sizeHint((basic.length + 1) * (basicLeft.length + basicRight.length + 1) + basicLeft.length * basicRight.length)
        feats ++= wordFeaturizer.fullFeatures(pos)
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

  }
}

object IndexedWordFeaturizer {
  def forTrainingSet[L](corpus: Iterable[IndexedSeq[(L, String)]],
                     gazetteer: Gazetteer[Any, String] = Gazetteer.empty,
                     noShapeThreshold: Int = 100,
                     needsContextFeatures: (String,Double)=>Boolean = {(w,c) => true}):IndexedWordFeaturizer = {
    val tagWordCounts: Counter2[L, String, Double] = Counter2.count(corpus.iterator.flatten).mapValues(_.toDouble)
    val wordCounts = sum(tagWordCounts, Axis._0)
    val featureIndex = Index[Feature]()

    val feat = new BasicWordFeaturizer(tagWordCounts, wordCounts, gazetteer)
    val basicFeatureIndex = feat.featureIndex
    val out = new PrintWriter(new FileWriter("index.txt"))
    basicFeatureIndex foreach {out.println _}
    out.close()

    // for left and right
    val asLeftFeatures = new Array[Int](basicFeatureIndex.size)
    val asRightFeatures = new Array[Int](basicFeatureIndex.size)

    for( (f, fi) <- basicFeatureIndex.pairs) {
      asLeftFeatures(fi) = featureIndex.index(PrevWordFeature(f))
      asRightFeatures(fi) = featureIndex.index(NextWordFeature(f))
    }

    println(basicFeatureIndex.size -> (basicFeatureIndex.size * basicFeatureIndex.size))
    val prevCurBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))
    val curNextBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))
    val prevNextBigramFeatures = Array.fill(basicFeatureIndex.size)(new OpenAddressHashArray[Int](basicFeatureIndex.size, default= -1))

    for(words <- corpus) {
      val anch = feat.anchor(words.map(_._2))

      def bf(pos: Int) =  anch.basicFeatures(pos)

      for(pos <- 0 until words.length) {
        val wc = wordCounts(words(pos)._2)
        val basic = bf(pos)
        if(!needsContextFeatures(words(pos)._2, wc)) {
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

    }

    new IndexedWordFeaturizer(feat,
    featureIndex, wordCounts,
    {(p,c) =>prevCurBigramFeatures(p)(c)},
    {(c,r) =>curNextBigramFeatures(c)(r)},
    {(p,r) =>prevNextBigramFeatures(p)(r)},
    asLeftFeatures, asRightFeatures,
    needsContextFeatures)

  }
}
