package epic.framework

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import collection.mutable.ArrayBuffer
import breeze.inference.{ExpectationPropagation, Factor}

case class EPInference[Datum, Augment](inferences: IndexedSeq[ProjectableInference[Datum, Augment]],
                                       maxEPIter: Int)(implicit aIsFactor: Augment <:< Factor[Augment]) extends AugmentableInference[Datum, Augment] {
  type ExpectedCounts = EPExpectedCounts

  def baseAugment(v: Datum) = inferences(0).baseAugment(v)

  // assume we don't need gold  to do EP, at least for now
  def goldCounts(value: Datum, augment: Augment) = {
    val counts = inferences.map(_.goldCounts(value, augment))
    EPExpectedCounts(counts.foldLeft(0.0) {
      _ + _.loss
    }, counts)
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

    val finalCounts = for (((inf, f_~), i) <- (inferences zip f_~).zipWithIndex) yield {
      val marg = marginals(i)
      val augment = f_~
      inf.guessCountsFromMarginals(datum, marg.asInstanceOf[inf.Marginal], augment)
    }

    EPExpectedCounts(partition, finalCounts)
  }
}
