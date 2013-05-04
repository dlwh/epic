package epic.features

import breeze.linalg.{Counter, Counter2}
import breeze.util.{Encoder, Index, Interner}
import epic.framework.Feature
import scala.collection.mutable.ArrayBuffer
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import scala.collection.Set


/**
 *
 * @author dlwh
 */
class BasicWordFeaturizer(tagWordCounts: Counter2[_, String, Double],
                          wordCounts: Counter[String, Double],
                          noShapeThreshold: Int = 100,
                          minCountThreshold: Int = 2)  {
  def anchor(words: IndexedSeq[String]): Anchoring = new Anchoring(words)
  def featureIndex:Index[Feature] = _featureIndex

  private val _featureIndex = Index[Feature]()
  private val wordIndex = Index(wordCounts.keySet)


  private val LowCountFeature = addToIndex("=LOWCOUNTWORD=", 'LowCount)
  private val boundaryFeatures = Array[Int](_featureIndex.index(BoundaryFeature))
  private def addToIndex(s: String, kind: Symbol) = _featureIndex.index(WordFeature(s, kind))

  private val wordFeatures = Encoder.fromIndex(wordIndex).tabulateArray(addToIndex(_, 'Word))
  private val classes = Encoder.fromIndex(wordIndex).tabulateArray(w => if(wordCounts(w) > noShapeThreshold) wordFeatures(wordIndex(w)) else addToIndex(EnglishWordClassGenerator(w), 'Class))
  private val shapes =  Encoder.fromIndex(wordIndex).tabulateArray(w => if(wordCounts(w) > noShapeThreshold) wordFeatures(wordIndex(w)) else addToIndex(WordShapeGenerator(w), 'Shape))

  // caches
  private val basicFeatures = Array.tabulate(wordIndex.size){ i =>
    val shape =  shapes(i)
    val classe =  classes(i)
    val wc = wordCounts(wordIndex.get(i))
    val w = wordFeatures(i)
    if(wc > noShapeThreshold) Array(w)
    else if (wc > minCountThreshold) Array(w, shape, classe, addToIndex("T-" + tagWordCounts(::, wordIndex.get(i)).argmax, 'MaxTag))
    else Array(shape, classe, LowCountFeature)
  }

  private val inner = new WordShapeFeaturizer(wordCounts, minCountThreshold)
  private val fullFeatures = Array.tabulate(wordIndex.size) {i =>
    inner.apply(wordIndex.get(i)).map(featureIndex(_))
  }

  class Anchoring(words: IndexedSeq[String]) {
    val indices = words.map(wordIndex(_))
    val counts = words.map(wordCounts(_))

    def basicFeatures(pos: Int): Array[Int] = {
      if(pos < 0 || pos >= words.length) boundaryFeatures
      else _basicFeatures(pos)
    }

    def fullFeatures(pos: Int): Array[Int] = {
      if(pos < 0 || pos >= words.length) basicFeatures(pos)
      else _fullFeatures(pos)
    }


    private val _basicFeatures = (0 until words.length) map { i =>
      val index = indices(i)
      if(index >= 0) {
        BasicWordFeaturizer.this.basicFeatures(index)
      } else {
        val ww = words(i)
        val classe =  featureIndex(WordFeature(EnglishWordClassGenerator(ww), 'Class))
        val shape =  featureIndex(WordFeature(WordShapeGenerator(ww), 'Shape))
        val out = ArrayBuffer[Int]()
        out += LowCountFeature
        if(classe >= 0) out += classe
        if(shape >= 0) out += shape
        out.toArray
      }
    }

    private val _fullFeatures:IndexedSeq[Array[Int]] = (0 until words.length) map {  i =>
      val index = indices(i)
      if(index >= 0) {
         BasicWordFeaturizer.this.fullFeatures(index).toArray
      } else {
        inner.apply(wordIndex.get(i)).map(featureIndex(_)).toArray
      }
    }

  }


}

case object BoundaryFeature extends Feature
