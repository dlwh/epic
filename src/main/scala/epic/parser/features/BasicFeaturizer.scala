package epic.parser.features

import epic.framework.Feature
import epic.trees.Span

import StandardSpanFeatures._
import collection.mutable.ArrayBuffer
import breeze.linalg.{Counter, Counter2}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import breeze.util.Interner


/**
 * Makes features for Spans and Words without attaching label semantics.
 * Designed to be fairly portable across tasks
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class BasicFeaturizer(tagWordCounts: Counter2[_, String, Double], wordCounts: Counter[String, Double], noShapeThreshold: Int = 100, minCountThreshold: Int = 5) extends Serializable {
  def anchor(words: IndexedSeq[String]): Anchoring = new Anchoring(words)

  val interner = new Interner[Feature]
  val inner = new WordShapeFeaturizer(wordCounts)

  class Anchoring(words: IndexedSeq[String]) {

    def featuresForWord(pos: Int) = _featuresForWord(pos)

    def featuresForSpan(start: Int, end: Int): Array[Feature] = {
      val feats = ArrayBuffer[Feature]()
      if (start < end - 1) {
        feats += WordEdges('Inside, basicFeatures(start)(0), basicFeatures(end-1)(0))
        feats += WordEdges('Outside, basicFeatures(start-1)(0), basicFeatures(end)(0))
        feats += WordEdges('Begin, basicFeatures(start-1)(0), basicFeatures(start)(0))
        feats += WordEdges('End, basicFeatures(end-1)(0), basicFeatures(end)(0))
        feats += SpanShapeFeature(SpanShapeGenerator.apply(words, Span(start,end)))
      }

      if (start == 0)
        feats += BeginSentFeature
      if(end == words.length)
        feats += EndSentFeature
      if (start == 0 && end == words.length)
        feats += WholeSentFeature

      feats += SpanLengthFeature(binDistance(end - start))

      feats.toArray
    }

    def basicFeatures(pos: Int) = {
      if(pos < 0 || pos >= words.length) IndexedSeq("###")
      else _basicFeatures(pos)
    }

    private val classes = words.map(w => if(wordCounts(w) > noShapeThreshold) w else EnglishWordClassGenerator(w))
    private val shapes = words.map(w => if(wordCounts(w) > noShapeThreshold) w else WordShapeGenerator(w))

    private val _basicFeatures = (0 until words.length) map { i =>
      val w = words(i)
      if(wordCounts(w) > 10) IndexedSeq(w)
      else if (wordCounts(w) > minCountThreshold) IndexedSeq(w, classes(i), shapes(i), ("T-" + tagWordCounts(::, w).argmax).intern)
      else if (wordCounts(w) > 1) IndexedSeq(("T-" + tagWordCounts(::, w).argmax).intern, classes(i), shapes(i))
      else IndexedSeq(shapes(i), classes(i))
    } map {_.map(_.intern)}


    val _featuresForWord: immutable.IndexedSeq[Array[Feature]] = 0 until words.length map { pos =>
      val feats = new ArrayBuffer[Feature]()
      val basic = basicFeatures(pos).map(WordFeature(_, 'Cur))
      val basicLeft = basicFeatures(pos - 1).map(WordFeature(_, 'Prev))
      val basicRight = basicFeatures(pos + 1).map(WordFeature(_, 'Next))
      feats ++= basicLeft
      feats ++= basicRight
      feats ++= inner.featuresFor(words, pos)
      for (a <- basicLeft; b <- basic) feats += BigramFeature(a,b)
      for (a <- basic; b <- basicRight) feats += BigramFeature(a,b)
      //        for (a <- basicLeft; b <- basicRight) feats += PairFeature(a,b)
      feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
      if(pos > 0 && pos < words.length - 1) {
        feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
        feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
      }
      feats.map(interner.intern _).toArray
    }
  }

  private def binDistance(dist2:Int) = {
    val dist = dist2.abs - 1
    if (dist >= 20) 7
    else if (dist >= 10) 6
    else if (dist >= 5) 5
    else dist
  }
}
