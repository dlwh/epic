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

import epic.inference.{ExpectationPropagation, Factor}

import collection.mutable.ArrayBuffer
import breeze.util.SerializableLogging
import java.util.concurrent.atomic.AtomicLong
import epic.parser.ParseMarginal

class EPInference[Datum, Augment <: AnyRef](val inferences: IndexedSeq[ProjectableInference[Datum, Augment]],
                                  val maxEPIter: Int,
                                  val epInGold: Boolean = false)(implicit aIsFactor: Augment <:< Factor[Augment]) extends ProjectableInference[Datum, Augment] with SerializableLogging with Serializable {
  type Marginal = EPMarginal[Augment, ProjectableInference[Datum, Augment]#Marginal]
  type ExpectedCounts = EPExpectedCounts
  type Scorer = EPScorer[ProjectableInference[Datum, Augment]#Scorer]

  def baseAugment(v: Datum) = inferences.filter(_ ne null).head.baseAugment(v)

  def project(v: Datum, s: Scorer, m: Marginal, oldAugment: Augment): Augment = m.q

  def scorer(v: Datum): Scorer = EPScorer(inferences.map(_.scorer(v)))

  override def forTesting = new EPInference(inferences.map(_.forTesting), maxEPIter, epInGold)

  // ugh code duplication...
  def goldMarginal(scorer: Scorer, datum: Datum, augment: Augment): Marginal = {
    if (!epInGold) {
      val marginals = inferences.indices.map { i =>
        val inf = inferences(i)
        if (inf eq null)
          null.asInstanceOf[ProjectableInference[Datum, Augment]#Marginal]
        else 
          inf.goldMarginal(scorer.scorers(i).asInstanceOf[inf.Scorer], datum)
      }
      val ((inf, m), iScorer) = (inferences zip marginals zip scorer.scorers).filter(_._1._2 != null).head
      EPMarginal(marginals.filter(_ ne null).map(_.logPartition).sum, inf.project(datum, iScorer.asInstanceOf[inf.Scorer], m.asInstanceOf[inf.Marginal], augment), marginals)
    } else {
      EPInference.doInference(datum, augment, inferences, scorer, (inf:ProjectableInference[Datum, Augment], scorer: ProjectableInference[Datum, Augment]#Scorer, q: Augment) => inf.goldMarginal(scorer.asInstanceOf[inf.Scorer], datum, q), maxEPIter)
    }
  }

  def marginal(scorer: Scorer, datum: Datum, augment: Augment): Marginal = {
    EPInference.doInference(datum, augment, inferences, scorer, (inf:ProjectableInference[Datum, Augment], scorer: ProjectableInference[Datum, Augment]#Scorer, q: Augment) => inf.marginal(scorer.asInstanceOf[inf.Scorer], datum, q), maxEPIter)
  }

}

case class EPMarginal[Augment, Marginal](logPartition: Double, q: Augment, marginals: IndexedSeq[Marginal]) extends epic.framework.Marginal

object EPInference extends SerializableLogging {
  val iters, calls = new AtomicLong(0)

  def doInference[Datum, Augment <: AnyRef,
                  Marginal <: ProjectableInference[Datum, Augment]#Marginal,
                  Scorer](datum: Datum,
                          augment: Augment, inferences: IndexedSeq[ProjectableInference[Datum, Augment]],
                          scorer: EPScorer[Scorer],
                          infType: (ProjectableInference[Datum, Augment],Scorer, Augment)=>Marginal,
                          maxEPIter: Int = 5,
                          convergenceThreshold: Double = 1E-4)
                         (implicit aIsFactor: Augment <:< Factor[Augment]):EPMarginal[Augment, Marginal] = {
    var iter = 0
    val marginals = ArrayBuffer.fill(inferences.length)(null.asInstanceOf[Marginal])

    def project(q: Augment, i: Int) = {
      val inf = inferences(i)
      marginals(i) = null.asInstanceOf[Marginal]
      val iScorer = scorer.scorers(i)
      var marg = infType(inf, iScorer, q)
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
      val newAugment = inf.project(datum, iScorer.asInstanceOf[inf.Scorer], marg.asInstanceOf[inf.Marginal], q)
      marginals(i) = marg
      // println("Leaving " + i)
      newAugment -> contributionToLikelihood
    }
    val ep = new ExpectationPropagation(project _, convergenceThreshold)
    val inferencesToUse = inferences.indices.filter(inferences(_) ne null)

    var state: ep.State = null
    val iterates = ep.inference(augment, inferencesToUse, inferencesToUse.map(i => inferences(i).baseAugment(datum)))
    while (iter < maxEPIter && iterates.hasNext) {
      val s = iterates.next()
      iter += 1
      state = s
    }
    EPInference.iters.addAndGet(iter)
    if (EPInference.calls.incrementAndGet % 1000 == 0) {
      val calls = EPInference.calls.get()
      val iters = EPInference.iters.get()
      logger.info(s"EP Stats $iters $calls ${iters * 1.0 / calls} $maxEPIter")
      EPInference.calls.set(0)
      EPInference.iters.set(0)
    }
    logger.debug(f"guess($iter%d:${state.logPartition}%.1f)")

    EPMarginal(state.logPartition, state.q, marginals)

  }

}
