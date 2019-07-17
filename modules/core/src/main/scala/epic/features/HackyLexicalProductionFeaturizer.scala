package epic.features

import epic.parser.RefinedFeaturizer
import epic.framework.Feature
import breeze.util.Index
import epic.trees.Rule
import epic.parser.RuleTopology
import epic.trees.TreeInstance
import epic.trees.AnnotatedLabel
import scala.collection.mutable.HashMap
import breeze.linalg.Counter2
import breeze.linalg.Counter
import breeze.linalg._
import epic.util.Arrays

class HackyLexicalProductionFeaturizer(wordTagCounts: Counter2[String, String, Double],
                                       topology: RuleTopology[AnnotatedLabel],
                                       featsDesc: String,
                                       hackyHeadFinder: HackyHeadFinder[String,String] = new RuleBasedHackyHeadFinder,
                                       db: DistanceBinner = DistanceBinner(),
                                       wordThreshold: Int = 5,
                                       commonWordThreshold: Int = 100) extends RuleAndSpansFeaturizer[String] {
  
  private val wordCounts = Counter[String,Double]
  private val wordToTagMap = new HashMap[String,String]
  for (word <- wordTagCounts.keysIterator.map(_._2).toSeq.distinct) {
    wordCounts(word) = sum(wordTagCounts(::, word))
    if (!wordToTagMap.contains(word)) {
      val tagCounts = wordTagCounts(::, word).iterator
      var bestTag = HackyLexicalProductionFeaturizer.UnkTag
      var bestTagCount = 0.0
      for ((tag, count) <- tagCounts) {
        if (count > bestTagCount) {
          bestTag = tag
          bestTagCount = count
        }
      }
      wordToTagMap.put(word, bestTag)
    }
  }
  
  def tag(word: String) = if (wordToTagMap.contains(word)) wordToTagMap(word) else HackyLexicalProductionFeaturizer.UnkTag
  
  val emptyArray = Array[Feature]()
  
  def anchor(w: IndexedSeq[String]) = new Anchoring {
    
    def words: IndexedSeq[String] = w

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Array[Feature] = {
      
      val preterminals = new Array[String](end - begin)
      for (i <- begin until end) {
        preterminals(i - begin) = tag(words(i))
      }
      
      val lc = topology.labelIndex.get(topology.leftChild(rule)).baseLabel
      val rc = topology.labelIndex.get(topology.rightChild(rule)).baseLabel
      
      val lcHeadIdx = begin + hackyHeadFinder.findHead(lc, preterminals.slice(0, split - begin))
      val rcHeadIdx = split + hackyHeadFinder.findHead(rc, preterminals.slice(split - begin, end - begin))
      val lcHeadWord = words(lcHeadIdx)
      val lcHeadTag = tag(words(lcHeadIdx))
      val rcHeadWord = words(rcHeadIdx)
      val rcHeadTag = tag(words(rcHeadIdx))
      
      val distance = db.binnedDistance(lcHeadIdx, rcHeadIdx)
      
      // It doesn't really make sense to back off to tag features here since the tags
      // will fail when the words are rare...
      val otherFeats: Array[Feature] = if (featsDesc.contains("lexical")) {
        Array(HeadPairDistanceRuleFeature(rule, lcHeadTag, rcHeadTag, distance),
              HeadPairDistanceRuleFeature(rule, lcHeadTag, if (wordCounts(rcHeadWord) >= commonWordThreshold) rcHeadWord else rcHeadTag, distance),
              HeadPairDistanceRuleFeature(rule, if (wordCounts(lcHeadWord) >= commonWordThreshold) lcHeadWord else lcHeadTag, rcHeadTag, distance))
      } else if (featsDesc.contains("ultralexical")) {
        Array(HeadPairDistanceRuleFeature(rule, lcHeadTag, rcHeadTag, distance),
              HeadPairDistanceRuleFeature(rule, lcHeadTag, if (wordCounts(rcHeadWord) >= commonWordThreshold) rcHeadWord else rcHeadTag, distance),
              HeadPairDistanceRuleFeature(rule, if (wordCounts(lcHeadWord) >= commonWordThreshold) lcHeadWord else lcHeadTag, rcHeadTag, distance),
              HeadPairDistanceRuleFeature(rule, if (wordCounts(lcHeadWord) >= commonWordThreshold) lcHeadWord else lcHeadTag, if (wordCounts(rcHeadWord) >= commonWordThreshold) rcHeadWord else rcHeadTag, distance))
      } else {
        Array[Feature]()
      }
      Arrays.concatenate(otherFeats,
                         Array(LeftTagDistanceRuleFeature(rule, lcHeadTag, distance),
                               LeftHeadDistanceRuleFeature(rule, if (wordCounts(lcHeadWord) >= wordThreshold) lcHeadWord else HackyLexicalProductionFeaturizer.RareToken, distance),
                               RightTagDistanceRuleFeature(rule, rcHeadTag, distance),
                               RightHeadDistanceRuleFeature(rule, if (wordCounts(rcHeadWord) >= wordThreshold) rcHeadWord else HackyLexicalProductionFeaturizer.RareToken, distance)))
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Array[Feature] = emptyArray
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int):Array[Feature] = emptyArray
  }

}

case class LeftTagDistanceRuleFeature(rule: Int, ltag: String, distance: Int) extends Feature
case class LeftHeadDistanceRuleFeature(rule: Int, lsuff: String, distance: Int) extends Feature
case class RightTagDistanceRuleFeature(rule: Int, rtag: String, distance: Int) extends Feature
case class RightHeadDistanceRuleFeature(rule: Int, rsuff: String, distance: Int) extends Feature
case class HeadPairDistanceRuleFeature(rule: Int, lsuff: String, rsuff: String, distance: Int) extends Feature

object HackyLexicalProductionFeaturizer {
  val UnkTag = "NN"
  val RareToken = "<RARE>"
}
