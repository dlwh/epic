package epic.corefdense

import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer
import edu.berkeley.nlp.entity.coref.Mention

class SimplePairwiseIndexingFeaturizerJoint(val typeIndexer: Indexer[String]) {
  
  val predefinedFeatureDomains = Array(6, // ment len
                                       6, // prev ment len
                                       10, // dist
                                       10, // sent dist
                                       2, // iWi
                                       2, // exact match
                                       2, // exact contained
                                       2, // exact reverse contained
                                       2, // head match
                                       2, // head contained
                                       2, // head reverse contained
                                       typeIndexer.size, // curr mention type
                                       typeIndexer.size) // ant mention type
  
  def getPredefinedFeatureDomains: Array[Int] = predefinedFeatureDomains
  
  def featurizePredefined(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int): Array[Int] = {
    val predefinedFeatureDomains = getPredefinedFeatureDomains
    val totalSize = predefinedFeatureDomains.foldLeft(0)(_ + _)
    val feats = Array.tabulate(totalSize)(i => 0)
    val currMent = docGraph.getMention(currMentIdx)
    val antMent = docGraph.getMention(antecedentIdx)
    var featCounter = 0
    var featOffset = 0
    def addFeat(idx: Int) {
      feats(featOffset + idx) = 1
      featOffset += predefinedFeatureDomains(featCounter)
      featCounter += 1
    }
    addFeat(Math.min(currMent.words.size, 6) - 1) // ment len
    addFeat(Math.min(antMent.words.size, 6) - 1) // prev ment len
    addFeat(Math.min(currMentIdx - antecedentIdx, 10) - 1) // dist
    addFeat(Math.min(currMent.sentIdx - antMent.sentIdx, 10) - 1) // sent dist
    addFeat(if (currMent.iWi(antMent)) 1 else 0) // iWi
    addFeat(if (currMent.spanToString.toLowerCase.equals(antMent.spanToString.toLowerCase)) 1 else 0) // exact match
    addFeat(if (antMent.spanToString.contains(currMent.spanToString)) 1 else 0); // exact contained
    addFeat(if (currMent.spanToString.contains(antMent.spanToString)) 1 else 0); // exact reverse contained
    addFeat(if (currMent.headStringLc.equals(antMent.headStringLc)) 1 else 0); // head match
    addFeat(if (antMent.spanToString.contains(currMent.headString)) 1 else 0); // head contained
    addFeat(if (currMent.spanToString.contains(antMent.headString)) 1 else 0); // head reverse contained
    val currType = SimplePairwiseIndexingFeaturizerJoint.mentType(currMent)
    // Use the first type by convention for unks (should be extremely rare)
    addFeat(if (typeIndexer.contains(currType)) typeIndexer.indexOf(currType) else 0) // curr mention type
    val antType = SimplePairwiseIndexingFeaturizerJoint.mentType(antMent)
    addFeat(if (typeIndexer.contains(antType)) typeIndexer.indexOf(antType) else 0) // ant mention type
    feats
  }

//  private def maybeAddFeat(indexedFeats: ArrayBuffer[Int], feat: String, addToIndexer: Boolean) {
//    if (addToIndexer) {
//      indexedFeats += indexer.getIndex(feat)
//    } else {
//      val idx = indexer.indexOf(feat)
//      if (idx != -1) indexedFeats += idx;
//    }
//  }
//  
//  def featurizeIndex(docGraph: DocumentGraph, currMentIdx: Int, antecedentIdx: Int, addToFeaturizer: Boolean): Array[Int] = {
//    val feats = new ArrayBuffer[Int]
//    def add(feat: String) = maybeAddFeat(feats, feat, addToFeaturizer)
//    
//    val currMent = docGraph.getMention(currMentIdx)
//    val antecedentMent = docGraph.getMention(antecedentIdx)
//    
//    //val snStr = ;
//    val sn = "SN=" + antecedentIdx == currMentIdx
//    add("SN=" + sn)
//    add("SNMentLen=" + currMent.words.size + sn)
//    add("Dist=" + Math.min(currMentIdx - antecedentIdx, 10));
//    add("SentDist=" + Math.min(currMent.sentIdx - antecedentMent.sentIdx, 10));
//    add("iWi=" + currMent.iWi(antecedentMent));
//    val exactStrMatch = (currMent.spanToString.toLowerCase.equals(antecedentMent.spanToString.toLowerCase));
//    add("ExactStrMatch=" + exactStrMatch);
//    add("ThisContained=" + (antecedentMent.spanToString.contains(currMent.spanToString)));
//    add("AntContained=" + (currMent.spanToString.contains(antecedentMent.spanToString)));
//    val headMatch = currMent.headStringLc.equals(antecedentMent.headStringLc);
//    add("HeadMatch=" + headMatch);
//    add("ThisHeadContained=" + (antecedentMent.spanToString.contains(currMent.headString)));
//    add("AntHeadContained=" + (currMent.spanToString.contains(antecedentMent.headString))));
//    
//    
//    
//    // TODO: featurize + index
//    feats.toArray
//  }
}

object SimplePairwiseIndexingFeaturizerJoint {
  
  def mentType(ment: Mention) = if (ment.mentionType.isClosedClass) ment.headStringLc else ment.mentionType.toString
  
  def makeTypeIndexer(docGraphs: Seq[DocumentGraph]) = {
    val indexer = new Indexer[String]
    for (docGraph <- docGraphs) {
      for (ment <- docGraph.getMentions) {
        val currTypePron = mentType(ment)
        indexer.getIndex(currTypePron)
      }
    }
    indexer
  }
}