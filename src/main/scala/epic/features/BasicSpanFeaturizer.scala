package epic.features

import epic.framework.Feature
import epic.parser.features.StandardSpanFeatures._
import epic.trees.Span
import breeze.util.Index
import scala.collection.mutable.ArrayBuffer
import epic.newfeatures.SpanShapeGenerator

/**
 *
 * @author dlwh
 */
class BasicSpanFeaturizer(val wordFeaturizer: BasicWordFeaturizer) {
  def wordFeatureIndex = wordFeaturizer.featureIndex

  def anchor(words: IndexedSeq[String]) = new Anchoring(words)

  class Anchoring(words: IndexedSeq[String]) {
    private val wordAnch = wordFeaturizer.anchor(words)
    import wordAnch._

    def basicFeaturesForWord(pos: Int): Array[Int] = wordAnch.basicFeatures(pos)

    def fullFeaturesForWord(pos: Int): Array[Int] =  wordAnch.fullFeatures(pos)

    def featuresForSpan(start: Int, end: Int): Array[Feature] = {
       val feats = ArrayBuffer[Feature]()
       if (start < end - 1) {
         feats += WordEdges('Inside, wordFeatureIndex.get(basicFeatures(start)(0)), wordFeatureIndex.get(basicFeatures(end-1)(0)))
         feats += WordEdges('Outside, wordFeatureIndex.get(basicFeatures(start-1)(0)), wordFeatureIndex.get(basicFeatures(end)(0)))
         feats += SpanShapeFeature(SpanShapeGenerator.apply(words, start,end))
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


  }

  @inline
  private def binDistance(dist2:Int) = {
    val dist = dist2.abs - 1
    if (dist >= 20) 7
    else if (dist >= 10) 6
    else if (dist >= 5) 5
    else dist
  }
}
