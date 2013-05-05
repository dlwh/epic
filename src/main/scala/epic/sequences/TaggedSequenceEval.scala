package epic.sequences

import epic.framework.EvaluationResult


object TaggedSequenceEval {
  def eval[L ,W](crf: CRF[L, W], examples: IndexedSeq[TaggedSequence[L, W]]):Stats = {
    examples.par.aggregate(new Stats()) ({ (stats, gold )=>
      val guess = crf.bestSequence(gold.words, gold.id +"-guess")
      val myStats = evaluateExample(guess, gold)
      println("Guess:\n" + guess.render + "\n Gold:\n" + gold.render+ "\n" + myStats)
      stats + myStats
    }, {_ + _})

  }

  def evaluateExample[W, L](guess: TaggedSequence[L, W], gold: TaggedSequence[L, W]): TaggedSequenceEval.Stats = {
    val nRight = (guess.tags zip gold.tags).count{ case (k,v) => k == v}
    val nTotal = guess.length
    val myStats: Stats = new Stats(nRight, nTotal, if(nRight == nTotal) 1 else 0, 1)
    myStats
  }

  case class Stats(nRight: Int = 0, nTotal: Int = 0, exact: Int = 0, nSent: Int = 0) extends EvaluationResult[Stats] {
    def accuracy = nRight * 1.0 / nTotal
    def exactMatch = exact * 1.0 / nSent

    def +(stats: Stats) = {
      new Stats(nRight + stats.nRight, nTotal + stats.nTotal, exact + stats.exact, nSent + stats.nSent)
    }

    override def toString = f"Evaluation Result: Accuracy=$accuracy%.4f Exact=$exactMatch%.4f"

  }

}
