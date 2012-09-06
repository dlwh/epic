package epic.framework

import collection.mutable.ArrayBuffer
import breeze.inference.{ExpectationPropagation, Factor}

case class FullEPInference[Datum, Augment](inferences: IndexedSeq[FullProjectableInference[Datum, Augment]],
                                           maxEPIter: Int)(implicit aIsFactor: Augment <:< Factor[Augment]) extends AugmentableInference[Datum, Augment] {
  type ExpectedCounts = EPExpectedCounts

  def baseAugment(v: Datum) = inferences(0).baseAugment(v)

  // ugh code duplication...
  def getGoldMarginals(datum: Datum, augment: Augment) = {
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
      val (marg, contributionToLikelihood) = inf.goldMarginal(datum, q)
      val newAugment = inf.projectGold(datum, marg, q)
      marginals(i) = marg
      newAugment -> contributionToLikelihood
    }
    val ep = new ExpectationPropagation(project _)

    var state: ep.State = null
    val iterates = ep.inference(augment, 0 until inferences.length, IndexedSeq.fill[Augment](inferences.length)(null.asInstanceOf[Augment]))
    var iter = 0
    var converged = false
    while (!converged && iter < maxEPIter && iterates.hasNext) {
      val s = iterates.next()
      if (state != null) {
        converged = (s.logPartition - state.logPartition).abs / math.max(s.logPartition, state.logPartition) < 1E-4
      }
      iter += 1
      state = s
    }
    print(iter + " ")

    (state.logPartition, state.q, marginals, state.f_~)
  }


  def getMarginals(datum: Datum, augment: Augment) = {
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
      val (marg, contributionToLikelihood) = inf.marginal(datum, q)
      val newAugment = inf.project(datum, marg, q)
      marginals(i) = marg
      newAugment -> contributionToLikelihood
    }
    val ep = new ExpectationPropagation(project _)

    var state: ep.State = null
    val iterates = ep.inference(augment, 0 until inferences.length, IndexedSeq.fill[Augment](inferences.length)(null.asInstanceOf[Augment]))
    var iter = 0
    var converged = false
    while (!converged && iter < maxEPIter && iterates.hasNext) {
      val s = iterates.next()
      if (state != null) {
        converged = (s.logPartition - state.logPartition).abs / math.max(s.logPartition, state.logPartition) < 1E-4
      }
      iter += 1
      state = s
    }
    print(iter + " ")

    (state.logPartition, state.q, marginals, state.f_~)
  }

  def guessCounts(datum: Datum, augment: Augment) = {
    val (partition, finalAugment, marginals, f_~) = getMarginals(datum, augment)

    val finalCounts = jointCounts(datum, marginals, finalAugment, f_~)

    EPExpectedCounts(partition, finalCounts)
  }

  def goldCounts(datum: Datum, augment: Augment) = {
     val (partition, finalAugment, marginals, f_~) = getGoldMarginals(datum, augment)

     val finalCounts = jointCounts(datum, marginals, finalAugment, f_~)

     EPExpectedCounts(partition, finalCounts)
   }

  def jointCounts(datum: Datum, marginals: ArrayBuffer[ProjectableInference[Datum, Augment]#Marginal], finalAugment: Augment, f_~ : IndexedSeq[Augment]) = {
    for (((inf, f_~), i) <- (inferences zip f_~).zipWithIndex) yield {
      val marg = marginals(i)
      val augment = finalAugment / f_~
      inf.countsFromMarginal(datum, marg.asInstanceOf[inf.Marginal], augment)
    }
  }
}
