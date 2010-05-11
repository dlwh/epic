package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.counters.LogCounters
import LogisticBitVector.Feature


class BitVectorLexicon[L,W](featurizer: Featurizer[L,W],
                            precomputedScores: LogPairedDoubleCounter[W,(L,Int)],
                            tagFeatureWeights: PairedDoubleCounter[(L,Int),Feature[L,W]],
                            logNormalizers: LogDoubleCounter[(L,Int)]) extends Lexicon[(L,Int),W] {
  def wordScore(label: (L,Int), w: W): Double = {
    if(precomputedScores(w).size > 0) precomputedScores(w)(label)
    else scoreUnknown(label,w);
  }
  
  override def tagScores(w: W): LogDoubleCounter[(L,Int)] = {
    if(precomputedScores(w).size > 0) precomputedScores(w);
    else LogCounters.aggregate(tags.map { l => (l,scoreUnknown(l,w))});
  }

  private def scoreUnknown(label: (L,Int), w: W) = {
    var score = 0.0;
    val labelScores = tagFeatureWeights(label);
    for(f <- featurizer.features(LogisticBitVector.WordDecision(w), label)) {
      score += labelScores(f);
    }
    score - logNormalizers(label);
  }
  
  def tags: Iterator[(L,Int)] = tagFeatureWeights.rows.map(_._1);
}
