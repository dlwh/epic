package epic.features

import epic.framework.Feature
import breeze.linalg.Counter2
import breeze.util.Index
import breeze.linalg._
import scala.collection.mutable.ArrayBuffer
import epic.trees.TreeInstance
import epic.trees.AnnotatedLabel

@SerialVersionUID(1L)
class TagSpanShapeFeaturizer[L](wordTagCounts: Counter2[L, String, Double], commonWordThreshold: Int = 3) extends SurfaceFeaturizer[String] with Serializable {
  
  private val tagIndex = Index(wordTagCounts.keysIterator.map(_._1));
  
  def anchor(words: IndexedSeq[String]): SurfaceFeatureAnchoring[String] = {
    new SurfaceFeatureAnchoring[String] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val b11 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 1, 1, 0, 0);
        val b21 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 2, 1, 0, 0);
        val b12 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 1, 2, 0, 0);
        val e11 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 0, 0, 1, 1);
        val e12 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 0, 0, 1, 2);
        val e21 = TagSpanShapeGenerator.featureFor(words, begin, end, wordTagCounts, commonWordThreshold, tagIndex, 0, 0, 2, 1);
        Array(b11, b21, b12, e11, e12, e21);
      }
    }
  }
}

/**
 *
 * @author dlwh
 */
object TagSpanShapeGenerator extends Serializable {
  
  def makeFeatType(beginContextAmount: Int, beginContentAmount: Int, endContentAmount: Int, endContextAmount: Int) = {
    beginContextAmount * 1000 + beginContentAmount * 100 + endContentAmount * 10 + endContextAmount;
  }
  
  def canonicalize(annLabel: AnnotatedLabel) = annLabel.baseLabel.substring(0, 1);
  
  def makeBaseLexicon(trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = makeLexicon(trees, canonicalize);
  
  def makeStandardLexicon(trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = makeLexicon(trees, _.baseLabel);
  
  def makeLexicon(trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], collapser: AnnotatedLabel => String) = {
    val strLexicon = Counter2[String,String,Double];
    for(ti <- trees) {
      val TreeInstance(_, tree, words) = ti;
      for ((l, w) <- tree.leaves.map(leaf => (leaf, words(leaf.span.begin)))) {
        strLexicon(collapser(l.label), w) += 1.0;
      }
    }
    strLexicon;
  }
  
  def labelIndexFor[L](word: String, wordTagCounts: Counter2[L, String, Double], commonWordThreshold: Int, tagIndex: Index[L]) = {
    val totalCount = sum(wordTagCounts(::, word))
    if (totalCount < commonWordThreshold) {
      -1;
    }
    val tagCounts = wordTagCounts(::, word).iterator;
    var bestTagIdx = -1;
    var bestTagCount = 0.0;
    for ((tag, count) <- tagCounts) {
      if (count > bestTagCount) {
        bestTagIdx = tagIndex(tag);
        bestTagCount = count;
      }
    }
    bestTagIdx
  }

  def featureFor[L](words: IndexedSeq[String],
                    begin: Int,
                    end: Int,
                    wordTagCounts: Counter2[L, String, Double],
                    commonWordThreshold: Int,
                    tagIndex: Index[L],
                    beginContextAmount: Int,
                    beginContentAmount: Int,
                    endContentAmount: Int,
                    endContextAmount: Int) = {
    var result = new StringBuilder();
    for (i <- begin - beginContextAmount until end + endContextAmount) {
      if (i < begin + beginContentAmount || i >= end - endContentAmount) {
        if (i < 0 || i >= words.size) {
          result = result.append("-1,");
        } else {
          result = result.append(labelIndexFor(words(i), wordTagCounts, commonWordThreshold, tagIndex)).append(",");
        }
      }
    }
    val featType = makeFeatType(beginContextAmount, beginContentAmount, endContentAmount, endContextAmount);
//    println(words.slice(begin, end) + " => " + featType + ": " + result.toString);
    TagSpanShapeFeature(featType, result.toString);
  }
}

case class TagSpanShapeFeature(featType: Int, shape: String) extends SpanFeature
