package scalanlp.parser.models

import scalala.tensor.dense._
import scalala.tensor.{Counter2, Counter, ::}
import scalanlp.parser.{TagScorer, Lexicon}

/**
 *
 * @author dlwh
 *
 */
class FeaturizedLexicon[L, L2, W](val weights: DenseVector[Double],
                                  val featureIndexer: FeatureIndexer[L, L2, W]) extends TagScorer[L2, W] {


  def scoreTag(l: L2, words: Seq[W], pos: Int) = {
    val w = words(pos)
    if (wordScores.contains(w, l))
      wordScores(w, l)
    else scoreUnknown(l, w)


  }

  private def scoreUnknown(label: L2, w: W): Double = {
    val feats = featureIndexer.featuresFor(featureIndexer.labelIndex(label), w);
    val score = feats dot weights;
    score;
  }


  private val wordScores = Counter2[W, L2, Double]();
  for ((wordMap, tagIndex) <- featureIndexer.lexicalCache.iterator.zipWithIndex;
       (word, feats) <- wordMap) {
    val score = feats dot weights;
    if (score.isNaN) {
      sys.error("Score for " + word + "is NaN!" + feats.nonzero.keys.map {
        k => (featureIndexer.index.get(k), weights(k))
      }.toIndexedSeq);
    }
    wordScores(word, featureIndexer.labelIndex.get(tagIndex)) = score;
    assert(wordScores(word, featureIndexer.labelIndex.get(tagIndex)) != Double.NegativeInfinity, (word, featureIndexer.labelIndex.get(tagIndex)).toString + "\n" +
      featureIndexer.decode(feats) + " " + featureIndexer.decode(weights));
  }


}