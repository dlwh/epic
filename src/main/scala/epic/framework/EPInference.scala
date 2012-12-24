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

class EPInference[Datum, Augment](inferences: IndexedSeq[ProjectableInference[Datum, Augment]],
                                  val maxEPIter: Int,
                                  val epInGold: Boolean = false)(implicit aIsFactor: Augment <:< Factor[Augment]) extends ProjectableInference[Datum, Augment] with Serializable {
  type Marginal = EPMarginal[Augment, ProjectableInference[Datum, Augment]#Marginal]
  type ExpectedCounts = EPExpectedCounts

  def emptyCounts = {
    val counts = for (m <- inferences) yield m.emptyCounts
    EPExpectedCounts(0.0, counts)
  }

  def baseAugment(v: Datum) = inferences(0).baseAugment(v)

  def project(v: Datum, m: Marginal, oldAugment: Augment): Augment = m.q

  // assume we don't need gold  to do EP, at least for now
  override def goldCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double) = {
    if(epInGold) {
      val counts = for( (inf, acc) <- inferences zip accum.counts) yield inf.goldCounts(value, augment, acc.asInstanceOf[inf.ExpectedCounts], scale)
      EPExpectedCounts(counts.foldLeft(0.0) {
        _ + _.loss
      }, counts)
    } else {
      super.goldCounts(value, augment, accum, scale)
    }
  }

  // ugh code duplication...
  def goldMarginal(datum: Datum, augment: Augment) = {
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
      val (marg, contributionToLikelihood) = inf.goldMarginal(datum, q)
      val newAugment = inf.project(datum, marg, q)
      marginals(i) = marg
      println("gold??? " + marginals(i).logPartition + " " + inf.goldMarginal(datum)._1.logPartition)
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
    print(iter + " gold(" + state.logPartition+") ")

    EPMarginal(state.logPartition, state.q, marginals, state.f_~) -> state.logPartition
  }


  def marginal(datum: Datum, augment: Augment) = {
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
      val (marg, contributionToLikelihood) = inf.marginal(datum, q)
      val newAugment = inf.project(datum, marg, q)
      marginals(i) = marg
      println("guess... " + marginals(i).logPartition + " " + inf.marginal(datum)._1.logPartition)
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
    print(iter + " guess(" + state.logPartition+") ")

    EPMarginal(state.logPartition, state.q, marginals, state.f_~) -> state.logPartition
  }


  def countsFromMarginal(datum: Datum, marg: Marginal, accum: EPExpectedCounts, scale: Double) = {
    import marg._
    for (((inf, f_~), i) <- (inferences zip messages).zipWithIndex) yield {
      val marg = marginals(i)
      inf.countsFromMarginal(datum, marg.asInstanceOf[inf.Marginal], accum.counts(i).asInstanceOf[inf.ExpectedCounts], scale)
    }
    accum.loss += scale * marg.logPartition
    accum
  }
}


case class EPMarginal[Augment, Marginal](logPartition: Double, q: Augment, marginals: IndexedSeq[Marginal], messages: IndexedSeq[Augment]) extends epic.framework.Marginal

