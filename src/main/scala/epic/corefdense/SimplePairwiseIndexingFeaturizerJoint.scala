package epic.corefdense

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer

class SimplePairwiseIndexingFeaturizerJoint(val indexer: Indexer[String],
                                            val featsToUse: Set[String]) extends PairwiseIndexingFeaturizer {
  
  def getIndexer(): Indexer[String] = indexer
  
  def getQueryCountsBundle: Option[QueryCountsBundle] = None

  private def maybeAddFeat(indexedFeats: ArrayBuffer[Int], feat: String, addToIndexer: Boolean) {
    if (addToIndexer) {
      indexedFeats += indexer.getIndex(feat)
    } else {
      val idx = indexer.indexOf(feat)
      if (idx != -1) indexedFeats += idx;
    }
  }
  
  def featurizeIndex(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, addToFeaturizer: Boolean): Array[Int] = {
    val feats = new ArrayBuffer[Int]
    def add(feat: String) = maybeAddFeat(feats, feat, addToFeaturizer)
    
    val currMent = docGraph.getMention(currMentIdx)
    val antecedentMent = docGraph.getMention(antecedentIdx)
    
    //val snStr = ;
    val sn = antecedentIdx == currMentIdx
    add("SN=" + sn)
    add("Dist=" + Math.min(currMentIdx - antecedentIdx, 10));
    add("SentDist=" + Math.min(currMent.sentIdx - antecedentMent.sentIdx, 10));
    add("iWi=" + currMent.iWi(antecedentMent));
    val exactStrMatch = (currMent.spanToString.toLowerCase.equals(antecedentMent.spanToString.toLowerCase));
    add("ExactStrMatch=" + exactStrMatch);
    add("ThisContained=" + (antecedentMent.spanToString.contains(currMent.spanToString)));
    add("AntContained=" + (currMent.spanToString.contains(antecedentMent.spanToString)));
    val headMatch = currMent.headStringLc.equals(antecedentMent.headStringLc);
    add("ThisHeadContained=" + (antecedentMent.spanToString.contains(currMent.headString)));
    add("AntHeadContained=" + (currMent.spanToString.contains(antecedentMent.headString)));
    
    // TODO: featurize + index
    feats.toArray
  }
}