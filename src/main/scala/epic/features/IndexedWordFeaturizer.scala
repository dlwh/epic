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
import epic.lexicon.Lexicon

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class IndexedWordFeaturizer private (val featurizer: BasicWordFeaturizer,
                                     val featureIndex: Index[Feature],
                                     lexicon: Lexicon[_, String],
                                     prevCurFeature: (Int,Int)=>Int,
                                     curNextFeature: (Int,Int)=>Int,
                                     prevNextFeature: (Int,Int)=>Int,
                                     asLeftFeature: Array[Int],
                                     asRightFeature: Array[Int]) extends Serializable {

  def anchor(words: IndexedSeq[String]) = new Localization(words)

  class Localization(words: IndexedSeq[String]) {
    def basicFeatures(pos: Int): Array[Int] = wordFeaturizer.basicFeatures(pos)
    def featuresForWord(pos: Int): Array[Int] = _featuresForWord(pos)
    private val wordFeaturizer = featurizer.anchor(words)
    private val lexAnch = lexicon.anchor(words)

    private val _featuresForWord: IndexedSeq[Array[Int]] = 0 until words.length map { pos =>
      val basic = basicFeatures(pos)
      if(lexAnch.allowedTags(pos).size == 1) {
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
  def forTrainingSet[L](corpus: Iterable[IndexedSeq[String]],
                        lexicon: Lexicon[L, String],
                        gazetteer: Gazetteer[Any, String] = Gazetteer.empty,
                        noShapeThreshold: Int = 100) = {
    val wordCounts = Counter.countTraversable(corpus.iterator.flatten).mapValues(_.toDouble)
    val featureIndex = Index[Feature]()

    val feat = new BasicWordFeaturizer(wordCounts, gazetteer, noShapeThreshold)
    val basicFeatureIndex = feat.featureIndex
    basicFeatureIndex.foreach(featureIndex.index)

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

    def isAmbiguous(pos: Int, anch: lexicon.Localization) = anch.allowedTags(pos).size != 1

    for(words <- corpus) {
      val anch = feat.anchor(words)
      val lexAnch = lexicon.anchor(words)

      def bf(pos: Int) =  anch.basicFeatures(pos)

      for(pos <- 0 until words.length) {
        val basic = bf(pos)
        if(isAmbiguous(pos, lexAnch)) {
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
    featureIndex, lexicon,
    {(p,c) =>prevCurBigramFeatures(p)(c)},
    {(c,r) =>curNextBigramFeatures(c)(r)},
    {(p,r) =>prevNextBigramFeatures(p)(r)},
    asLeftFeatures, asRightFeatures)

  }
}
