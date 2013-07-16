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

/**
 * Inference is the core interface in Epic. It produces instances of [[epic.framework.Marginal]]
 * which are then turned into [[epic.framework.ExpectedCounts]].
 *
 * There are two kinds of marginals produced by an inference object: gold and guess.
 * Gold marginals are the marginals conditioned on the output label/structure itself (e.g. the parse tree).
 * Guess marginals are the marginals conditioned on only the input data (e.g. the words in the sentence)
 *
 * In structured prediction, the objective usually takes the form:
 * \minimize \sum_{all structures} score(structure) - \sum_{all structures compatible with output label} score(structure)
 *
 * with the derivative being:
 * E_{all structures} features(structure) - E_{all structures compatible with output label} features(structure)
 *
 * replacing sum with max for max marginals.
 *
 *
 * @tparam Datum the kind of thing to do inference on
 * @author dlwh
 */
trait Inference[Datum] extends Serializable {
  type Marginal <: epic.framework.Marginal
  type ExpectedCounts <: epic.framework.ExpectedCounts[ExpectedCounts]
  def emptyCounts: ExpectedCounts

  /**
   * Produces the "gold marginal" which is the marginal conditioned on the output label/structure itself.
   * @param v the example
   * @return gold marginal
   */
  def goldMarginal(v: Datum):Marginal

  /**
   * Produces the "guess marginal" which is the marginal conditioned on only the input data
   * @param v the example
   * @return gold marginal
   */
  def marginal(v: Datum):Marginal

  /**
   * Accumulates expected counts from the marginal into accum, scaling all counts by the scale factor.
   * Typically scale is ±1.0. +1 is for "guess" portions of the objective, and -1 for gold parts.
   *
   * This operation is destructive to prevent thrashing in the GC. (ExpectedCounts is usually quite big!)
   *
   * @param v
   * @param marg
   * @param accum
   * @param scale
   * @return
   */
  def countsFromMarginal(v: Datum, marg: Marginal, accum: ExpectedCounts, scale: Double):ExpectedCounts


  /**
   * Compute the marginal and tally guess expected counts
   * @return
   */
  def guessCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts = {
    val marg = marginal(value)
    countsFromMarginal(value, marg, accum, scale)
  }

  /**
   * Compute the gold marginal and tally gold expected counts
   * @return
   */
  def goldCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts = {
    val marg = goldMarginal(value)
    countsFromMarginal(value, marg, accum, scale)
  }

  /**
   * compute both marginals and tally guess counts - gold counts
   * @param datum
   * @param accum
   * @param scale
   * @return
   */
  def expectedCounts(datum: Datum, accum: ExpectedCounts, scale: Double) = {
    val result1 = guessCounts(datum, accum, scale)
    goldCounts(datum, result1, -scale)
  }
}


/**
 * AugmentableInference is an [[epic.framework.Inference]] that can support injecting
 * additional information into the structure computation. This can include
 * prior information over the structure (useful for EP or other Bayesian inference)
 * or loss-augmentation.
 *
 * @tparam Datum the kind of thing to do inference on
 * @tparam Augment the extra piece of information we can use to do inference
 *
 */
trait AugmentableInference[Datum,Augment] extends Inference[Datum] {
  /**
   * The "no prior information" augment. Used if nothing is passed in.
   */
  def baseAugment(v: Datum):Augment

  /**
   * ```marginal(v, baseAugment(v))```
   * @param v the example
   * @return gold marginal
   */
  def marginal(v: Datum):Marginal = marginal(v, baseAugment(v))
  def marginal(v: Datum, aug: Augment):Marginal

  def goldMarginal(v: Datum):Marginal  = goldMarginal(v, baseAugment(v))
  def goldMarginal(v: Datum, aug: Augment):Marginal

  def guessCounts(datum: Datum,  augment: Augment, accum: ExpectedCounts, scale: Double) = {
    val m = marginal(datum,augment)
    countsFromMarginal(datum, m, accum, scale)
  }

  def goldCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    val m = goldMarginal(value, augment)
    countsFromMarginal(value, m, accum, scale)
  }

  override def goldCounts(value: Datum, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    goldCounts(value, baseAugment(value), accum, scale)
  }

  override def guessCounts(value: Datum, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    guessCounts(value, baseAugment(value), accum, scale)
  }
}

/**
 * A ProjectableInference is an [[epic.framework.AugmentableInference]] that
 * can also create a new Augment from the marginal and the old augment. This is
 * mostly for EP/BP-type setups where you iteratively improve the prior
 * until convergence.
 * @tparam Datum the kind of thing to do inference on
 * @tparam Augment the extra piece of information we can use to do inference
 *
 */
trait ProjectableInference[Datum,Augment] extends AugmentableInference[Datum,Augment] {
  def project(v: Datum, m: Marginal, oldAugment: Augment):Augment
}
