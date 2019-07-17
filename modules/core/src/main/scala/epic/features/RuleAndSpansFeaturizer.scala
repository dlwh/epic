package epic.features

import epic.framework.Feature
import breeze.util.Index
import breeze.util.MutableIndex

trait RuleAndSpansFeaturizer[W] extends Serializable {
  
  def anchor(words: IndexedSeq[W]):Anchoring
  
  trait Anchoring {
    def words: IndexedSeq[W]

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int):Array[Feature]
    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int):Array[Feature]
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int):Array[Feature]
  }
}

class ZeroRuleAndSpansFeaturizer[W]() extends RuleAndSpansFeaturizer[W] {
  val emptyArray = Array[Feature]()
  
  def anchor(w: IndexedSeq[W]) = new Anchoring {
    def words = w
    
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = emptyArray
    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = emptyArray
    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = emptyArray
  }
}

object RuleAndSpansFeaturizer {
  
  def indexAndOffset(index: Index[Feature], feats: Array[Feature], offset: Int): Array[Int] = {
    val indexedArr = new Array[Int](feats.size);
    var i = 0;
    while (i < feats.size) {
      indexedArr(i) = index(feats(i)) + offset;
      i += 1;
    }
    indexedArr;
  }
  
  def addToIndex(index: MutableIndex[Feature], feats: Array[Feature]) {
    var i = 0
    while (i < feats.length) {
      index.index(feats(i))
      i += 1
    }
  }
  
}