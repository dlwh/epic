package epic.parser.models

import breeze.linalg._
import epic.parser.{TagScorer, Lexicon}

/**
 *
 * @author dlwh
 *
 */
class FeaturizedLexicon[L, L2, W](val weights: DenseVector[Double],
                                  val featureIndexer: IndexedFeaturizer[L, L2, W]) extends TagScorer[L2, W] {


  def scoreTag(l: L2, words: Seq[W], pos: Int) = {
    val w = words(pos)
    if (wordScores.contains(w, l))
      wordScores(w, l)
    else scoreUnknown(l, w)


  }

  private def scoreUnknown(label: L2, w: W): Double = {
    featureIndexer.computeWeight(featureIndexer.labelIndex(label), w, weights)
  }


  private val wordScores = Counter2[W, L2, Double]()
  for ((wordMap, tagIndex) <- featureIndexer.lexicalCache.iterator.zipWithIndex;
       (word, feats) <- wordMap) {
    val score = featureIndexer.computeWeight(tagIndex, word, weights)
    assert(!score.isNaN, tagIndex + " " + word + weights.valuesIterator.exists(_.isNaN))
    wordScores(word, featureIndexer.labelIndex.get(tagIndex)) = score
    assert(wordScores(word, featureIndexer.labelIndex.get(tagIndex)) != Double.NegativeInfinity, (word, featureIndexer.labelIndex.get(tagIndex)).toString + "\n" +
      featureIndexer.decode(feats) + " " + featureIndexer.decode(weights))
  }


}