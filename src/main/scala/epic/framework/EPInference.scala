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
import epic.util.SafeLogging
import nak.inference.{ExpectationPropagation, Factor}

class EPInference[Datum, Augment](val inferences: IndexedSeq[ProjectableInference[Datum, Augment]],
                                  val maxEPIter: Int,
                                  val epInGold: Boolean = false)(implicit aIsFactor: Augment <:< Factor[Augment]) extends ProjectableInference[Datum, Augment] with SafeLogging with Serializable {
  type Marginal = EPMarginal[Augment, ProjectableInference[Datum, Augment]#Marginal]
  type ExpectedCounts = EPExpectedCounts



  def baseAugment(v: Datum) = inferences.filter(_ ne null).head.baseAugment(v)

  def project(v: Datum, m: Marginal, oldAugment: Augment): Augment = m.q

  // ugh code duplication...
  def goldMarginal(datum: Datum, augment: Augment) = {
    if(!epInGold) {
      val marginals = for(inf <- inferences) yield {
        if(inf eq null)
          null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
        else 
          inf.goldMarginal(datum)
      }
      val (inf, m) = (inferences zip marginals).filter(_._2 != null).head
      EPMarginal(marginals.filter(_ ne null).map(_.logPartition).sum, inf.project(datum, m.asInstanceOf[inf.Marginal], augment), marginals)
    } else {

      val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])
      var iter = 0
      def project(q: Augment, i: Int) = {
        val inf = inferences(i)
        assert(inf != null)
        marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
        val marg = inf.goldMarginal(datum, q)
        val contributionToLikelihood = marg.logPartition
        assert(!contributionToLikelihood.isInfinite, s"Model $i is misbehaving ($contributionToLikelihood) on iter $iter! Datum: " + datum )
        assert(!contributionToLikelihood.isNaN, s"Model $i is misbehaving (NaN) on iter $iter!")
        val newAugment = inf.project(datum, marg, q)
        marginals(i) = marg
        newAugment -> contributionToLikelihood
      }

      val ep = new ExpectationPropagation(project _, 1E-3)

      val inferencesToUse = (0 until inferences.length).filter(inferences(_) ne null)

      var state: ep.State = null
      val iterates = ep.inference(augment, inferencesToUse, IndexedSeq.fill[Augment](inferencesToUse.length)(null.asInstanceOf[Augment]))
      var converged = false
      while (!converged && iter < maxEPIter && iterates.hasNext) {
        val s = iterates.next()
        if (state != null) {
          converged = (s.logPartition - state.logPartition).abs / math.max(s.logPartition, state.logPartition) < 1E-4
          //        if (s.q.isInstanceOf[SentenceBeliefs]) {
          //          println(iter + " gold " + s.q.asInstanceOf[SentenceBeliefs].maxChange(state.q.asInstanceOf[SentenceBeliefs]) + " " + s.logPartition + " " + state.logPartition)
          //        }
        }
        iter += 1
        state = s
      }
      //    print(f"gold($iter%d:${state.logPartition%.1f})")

      EPMarginal(state.logPartition, state.q, marginals)
    }
  }


  def marginal(datum: Datum, augment: Augment) = {
    var iter = 0
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal])

    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
      var marg = inf.marginal(datum, q)
      var contributionToLikelihood = marg.logPartition
      if (contributionToLikelihood.isInfinite || contributionToLikelihood.isNaN) {
        logger.error(s"Model $i is misbehaving ($contributionToLikelihood) on iter $iter! Datum: $datum" )
        throw new RuntimeException("EP is being sad!")
        /*
        marg = inf.marginal(datum)
        contributionToLikelihood = marg.logPartition
        if (contributionToLikelihood.isInfinite || contributionToLikelihood.isNaN) {
          throw new RuntimeException(s"Model $i is misbehaving ($contributionToLikelihood) on iter $iter! Datum: " + datum )
        }
        */
      }
      val newAugment = inf.project(datum, marg, q)
      marginals(i) = marg
//      println("Leaving " + i)
      newAugment -> contributionToLikelihood
    }

    val ep = new ExpectationPropagation(project _, 1E-5)
    val inferencesToUse = (0 until inferences.length).filter(inferences(_) ne null)

    var state: ep.State = null
    val iterates = ep.inference(augment, inferencesToUse, inferencesToUse.map(i => inferences(i).baseAugment(datum)))
    var converged = false
    while (!converged && iter < maxEPIter && iterates.hasNext) {
      val s = iterates.next()
      if (state != null) {
        converged = (s.logPartition - state.logPartition).abs / math.max(s.logPartition, state.logPartition) < 1E-5
        logger.trace {
          import epic.everything._
          if (s.q.isInstanceOf[SentenceBeliefs]) {
            (iter + " guess " + s.q.asInstanceOf[SentenceBeliefs].maxChange(state.q.asInstanceOf[SentenceBeliefs]) + " " + s.logPartition + " " + state.logPartition)
          } else {
            (iter + " guess " + s.q.isConvergedTo(state.q) + " " + s.logPartition + " " + state.logPartition)
          }
        }
      }

      iter += 1
      state = s
    }
    logger.debug(f"guess($iter%d:${state.logPartition%.1f})")

    EPMarginal(state.logPartition, state.q, marginals)
  }



}


case class EPMarginal[Augment, Marginal](logPartition: Double, q: Augment, marginals: IndexedSeq[Marginal]) extends epic.framework.Marginal

