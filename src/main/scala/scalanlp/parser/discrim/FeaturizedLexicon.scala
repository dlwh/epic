package scalanlp.parser
package discrim

import scalala.tensor.dense._
import scalala.tensor.{Counter2,Counter,::}

/**
 * 
 * @author dlwh
 *
 */
class FeaturizedLexicon[L,W](val openTagSet: Set[L], val closedWords: Set[W], val weights: DenseVector[Double],
                             val featureIndexer: FeatureIndexer[L,W]) extends Lexicon[L,W] {
  def wordScore(label: L, w: W): Double = {
    tagScores(w)(label);
  }



  override def tagScores(w: W): Counter[L,Double] = {
    if(closedWords(w) && wordScores.contains(w)) wordScores(w, ::)
    else {
      val res = Counter(openTagSet.iterator.map ( k => (k,scoreUnknown(k,w))));
      for( (k,v) <- wordScores(w, ::).nonzero.pairs) {
        res(k) = v;
      }
      res
    }
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

  private val wordScores = Counter2[W,L,Double]();
  private val tagSet = collection.mutable.Set[L]();
  for( (wordMap, tagIndex) <- featureIndexer.lexicalCache.iterator.zipWithIndex;
       (word,feats) <- wordMap) {
    val score = feats dot weights;
    if(score.isNaN) {
      sys.error("Score for " + word + "is NaN!" + feats.nonzero.keys.map { k => (featureIndexer.index.get(k),weights(k))}.toIndexedSeq);
    }
    wordScores(word,featureIndexer.labelIndex.get(tagIndex)) = score;
    assert(wordScores(word,featureIndexer.labelIndex.get(tagIndex)) != Double.NegativeInfinity, (word,featureIndexer.labelIndex.get(tagIndex)).toString + "\n" +
        featureIndexer.decode(feats) + " " + featureIndexer.decode(weights));
    tagSet += featureIndexer.labelIndex.get(tagIndex);
  }

  def knownTagWords = wordScores.nonzero.keys.iterator.map(_.swap);

}