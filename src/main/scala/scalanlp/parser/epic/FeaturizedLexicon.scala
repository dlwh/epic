package scalanlp.parser.epic

import scalala.tensor.dense._
import scalala.tensor.{Counter2, Counter, ::}
import scalanlp.parser.Lexicon

/**
 * 
 * @author dlwh
 *
 */
class FeaturizedLexicon[L, L2, W](val openTagSet: Set[L2], val closedWords: Set[W], val weights: DenseVector[Double],
                                  val featureIndexer: FeatureIndexer[L, L2, W]) extends Lexicon[L2, W] {
  def wordScore(label: L2, w: W): Double = {
    tagScores(w)(label);
  }


  def wordScore(words: Seq[W], label: L2, pos: Int) = wordScore(label, words(pos))

  override def tagScores(w: W): Counter[L2, Double] = {
    if(closedWords(w) && wordScores.contains(w)) wordScores(w, ::)
    else {
      val res = Counter(openTagSet.iterator.map ( k => (k, scoreUnknown(k, w))));
      for( (k, v) <- wordScores(w, ::).nonzero.pairs) {
        res(k) = v;
      }
      res
    }
  }

  private def scoreUnknown(label: L2, w: W):Double = {
    assert(!openTagSet.isEmpty);
    if(!openTagSet.contains(label)) Double.NegativeInfinity
    else {
      val feats = featureIndexer.featuresFor(featureIndexer.labelIndex(label), w);
      val score = feats dot weights;
      score;
    }
  }

  def tags: Iterator[L2] = tagSet.iterator;

  private val wordScores = Counter2[W, L2,Double]();
  private val tagSet = collection.mutable.Set[L2]();
  for( (wordMap, tagIndex) <- featureIndexer.lexicalCache.iterator.zipWithIndex;
       (word, feats) <- wordMap) {
    val score = feats dot weights;
    if(score.isNaN) {
      sys.error("Score for " + word + "is NaN!" + feats.nonzero.keys.map { k => (featureIndexer.index.get(k), weights(k))}.toIndexedSeq);
    }
    wordScores(word, featureIndexer.labelIndex.get(tagIndex)) = score;
    assert(wordScores(word, featureIndexer.labelIndex.get(tagIndex)) != Double.NegativeInfinity, (word, featureIndexer.labelIndex.get(tagIndex)).toString + "\n" +
        featureIndexer.decode(feats) + " " + featureIndexer.decode(weights));
    tagSet += featureIndexer.labelIndex.get(tagIndex);
  }

  def knownTagWords = wordScores.nonzero.keys.iterator.map(_.swap);

}