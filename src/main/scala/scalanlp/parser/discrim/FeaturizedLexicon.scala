package scalanlp.parser
package discrim

import scalala.tensor.dense._;
import scalala.tensor.counters.LogCounters._;

/**
 * 
 * @author dlwh
 *
 */
class FeaturizedLexicon[L,W](val openTagSet: Set[L], val weights: DenseVector,
                             val featureIndexer: FeatureIndexer[L,W]) extends Lexicon[L,W] {
  def wordScore(label: L, w: W): Double = {
    tagScores(w)(label);
  }



  override def tagScores(w: W): LogDoubleCounter[L] = {
    if(wordScores.contains(w)) wordScores(w)
    else aggregate(openTagSet.iterator.map ( k => (k,scoreUnknown(k,w))));
  }

  private def scoreUnknown(label: L, w: W):Double = {
    assert(!openTagSet.isEmpty);
    if(!openTagSet.contains(label)) Double.NegativeInfinity
    else {
      val feats = featureIndexer.featuresFor(featureIndexer.labelIndex(label),w);
      val score = feats dot weights;
      score;
    }
  }

  def tags: Iterator[L] = tagSet.iterator;

  private val wordScores = LogPairedDoubleCounter[W,L]();
  private val tagSet = collection.mutable.Set[L]();
  for( (wordMap, tagIndex) <- featureIndexer.lexicalCache.iterator.zipWithIndex;
       (word,feats) <- wordMap) {
    wordScores(word,featureIndexer.labelIndex.get(tagIndex)) = feats dot weights;
    tagSet += featureIndexer.labelIndex.get(tagIndex);
  }

  def knownTagWords = wordScores.activeKeys.map(_.swap);

}