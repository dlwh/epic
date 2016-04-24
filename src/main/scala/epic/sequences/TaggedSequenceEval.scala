package epic.sequences

import epic.framework.EvaluationResult
import breeze.linalg.Counter2


/**
 * Object for evaluating [[epic.sequences.TaggedSequence]]s. Returned metrics
 * are accuracy and exact match.
 *
 * @author dlwh
 */
object TaggedSequenceEval {
  def eval[L ,W](crf: CRF[L, W], examples: IndexedSeq[TaggedSequence[L, W]], printBadExamples: Boolean = false):Stats[L] = {
    examples.par.aggregate(new Stats[L]()) ({ (stats, gold )=>
      val guess = crf.bestSequence(gold.words, gold.id +"-guess")
      val myStats = evaluateExample(guess, gold)

      val sent = for( ((p,g),w) <- guess.label zip gold.label zip guess.words) yield if (g == p) s"$w/$g" else s"$w/[G:$g,P:$p]"
      if (myStats.exact != 1)
        println(sent.mkString(" ") + "\n" + myStats)
      stats + myStats
    }, {_ + _})

  }

  def evaluateExample[W, L](guess: TaggedSequence[L, W], gold: TaggedSequence[L, W]): TaggedSequenceEval.Stats[L] = {
    val confusion = Counter2({for( (p, g) <- guess.tags zip gold.tags if p != g) yield (p,g,1)}:_*)
    val nRight = guess.length - confusion.size
    val nTotal = guess.length
    val myStats = new Stats(nRight, nTotal, if (nRight == nTotal) 1 else 0, 1, confusion)
    myStats
  }

  case class Stats[L](nRight: Int = 0, nTotal: Int = 0, exact: Int = 0, nSent: Int = 0, confusion: Counter2[L, L, Int] = Counter2[L, L, Int]()) extends EvaluationResult[Stats[L]] {
    def accuracy = nRight * 1.0 / nTotal
    def exactMatch = exact * 1.0 / nSent

    def +(stats: Stats[L]) = {
      new Stats[L](nRight + stats.nRight, nTotal + stats.nTotal, exact + stats.exact, nSent + stats.nSent, confusion + stats.confusion)
    }

    override def toString = f"Evaluation Result: Accuracy=$accuracy%.4f Exact=$exactMatch%.4f"



  }

}
