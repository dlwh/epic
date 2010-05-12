package scalanlp.parser
package bitvector

import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.counters.LogCounters.LogPairedDoubleCounter
import scalala.tensor.counters.LogCounters
import scala.util.control.Breaks._;
import LogisticBitVector.Feature


class BitVectorLexicon[L,W](featurizer: Featurizer[L,W],
                            precomputedScores: LogPairedDoubleCounter[W,(L,Int)],
                            featureWeights: DoubleCounter[Feature[L,W]],
                            logNormalizers: LogDoubleCounter[(L,Int)],
                            tagSet: Set[(L,Int)],
                            openTagSet: Set[(L,Int)]
                            ) extends Lexicon[(L,Int),W] {
  def wordScore(label: (L,Int), w: W): Double = {
    val score = if(precomputedScores(w).size > 0) precomputedScores(w)(label)
    else scoreUnknown(label,w);
    println(label,w,score);
    score;
  }
  
  override def tagScores(w: W): LogDoubleCounter[(L,Int)] = {
    val scores = if(precomputedScores(w).size > 0) precomputedScores(w);
    else LogCounters.aggregate(tags.map { l => (l,scoreUnknown(l,w))}.filterNot(_._2.isInfinite));

    println(w,scores);
    scores;
  }

  private def scoreUnknown(label: (L,Int), w: W):Double = {
    if(!openTagSet.contains(label)) Double.NegativeInfinity
    else {
      var score = 0.0;
      val feats = featurizer.features(LogisticBitVector.WordDecision(w), label);
      var featScores = collection.mutable.Map[Feature[L,W],Double]();
      for(f <- feats) {
        var w = featureWeights(f);
        if(w == 0) {
          w = featurizer.priorForFeature(f) getOrElse 0.0;
        }
        featScores(f) = w;
        score += w;
      }
      println(label,w,score-logNormalizers(label),logNormalizers(label),featScores);
      score - logNormalizers(label);
    }
  }
  
  def tags: Iterator[(L,Int)] = tagSet.iterator;
}
