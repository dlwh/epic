package epic.sequences

import epic.framework.EvaluationResult
import com.typesafe.scalalogging.slf4j.LazyLogging


/**
 * Object for evaluating [[epic.sequences.Segmentation]]s. Returned metrics
 * are precision, recall, and f1
 *
 * @author dlwh
 */
object SegmentationEval extends LazyLogging {
  def eval[L ,W](crf: SemiCRF[L, W], examples: IndexedSeq[Segmentation[L, W]], logOnlyErrors: Boolean = true):Stats = {
    examples.par.aggregate(new Stats(0,0,0)) ({ (stats, gold )=>
      val guess = crf.bestSequence(gold.words, gold.id +"-guess")
      try {
        if(guess.label != gold.label)
          logger.trace(s"gold = $gold guess = $guess " +
            s"guess logPartition = ${crf.goldMarginal(guess.segments, guess.words).logPartition} " +
            s"gold logPartition =${crf.goldMarginal(gold.segments, gold.words).logPartition}")
      } catch {
        case ex: Exception => logger.debug("Can't recover gold for " + gold)
      }
      val myStats = evaluateExample(Set(), guess, gold)
      if(!logOnlyErrors || myStats.f1 < 1.0)
        logger.info("Guess:\n" + guess.render + "\n Gold:\n" + gold.render+ "\n" + myStats)
      stats + myStats
    }, {_ + _})

  }

  def evaluateExample[W, L](outsideLabel: Set[L], guess: Segmentation[L, W], gold: Segmentation[L, W]): SegmentationEval.Stats = {
    val guessSet = guess.segments.filter(a => !outsideLabel(a._1)).toSet
    val goldSet = gold.segments.filter(a => !outsideLabel(a._1)).toSet
    val nRight = (guessSet & goldSet).size
    val myStats: Stats = new Stats(nRight, guessSet.size, goldSet.size)
    myStats
  }

  class Stats(val nRight: Int = 0, val nGuess: Int = 0, val nGold: Int = 0) extends EvaluationResult[Stats] {
    def precision = nRight * 1.0 / nGuess
    def recall = nRight * 1.0 / nGold
    def f1 = 2 * precision * recall / (precision + recall)

    def +(stats: Stats) = {
      new Stats(nRight + stats.nRight, nGuess + stats.nGuess, nGold + stats.nGold)
    }

    override def toString = "Evaluation Result: P=%.4f R=%.4f F=%.4f".format(precision,recall,f1)

  }

}
