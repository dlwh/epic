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
 *
 * @author dlwh
 */
trait Inference[Datum] extends Serializable {
  type ExpectedCounts <: AnyRef
  def emptyCounts: ExpectedCounts

  def expectedCounts(datum: Datum, accum: ExpectedCounts, scale: Double): ExpectedCounts
}

trait GoldGuessInference[Datum] extends Inference[Datum] {
  type ExpectedCounts <: epic.framework.ExpectedCounts[ExpectedCounts]

  def guessCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts
  def goldCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts

  def expectedCounts(datum: Datum, accum: ExpectedCounts, scale: Double) = {
    guessCounts(datum, accum, scale)
    goldCounts(datum, accum, -scale)
    accum
  }
}

/**
 * Used for loss-augmented inference or EP inference
 */
trait AugmentableInference[Datum, Augment] extends GoldGuessInference[Datum] {
  def baseAugment(v: Datum):Augment

  override def guessCounts(value: Datum, accum: ExpectedCounts, scale: Double) =  guessCounts(value, baseAugment(value), accum, scale)
  def guessCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double):ExpectedCounts

  override def goldCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts = goldCounts(value, baseAugment(value), accum, scale)
  def goldCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double):ExpectedCounts
}

trait MarginalInference[Datum,Augment] extends AugmentableInference[Datum,Augment] {
  type Marginal <: AnyRef
  def marginal(v: Datum, aug: Augment):(Marginal,Double)
  def countsFromMarginal(v: Datum, marg: Marginal, aug: Augment, accum: ExpectedCounts, scale: Double):ExpectedCounts
  override def guessCounts(datum: Datum,  augment: Augment, accum: ExpectedCounts, scale: Double) = {
    val m = marginal(datum,augment)
    countsFromMarginal(datum,m._1,augment, accum, scale)
  }
}

trait ProjectableInference[Datum,Augment] extends MarginalInference[Datum,Augment] {
  def project(v: Datum, m: Marginal, oldAugment: Augment):Augment
}

trait FullProjectableInference[Datum, Augment] extends ProjectableInference[Datum, Augment] {
  def projectGold(v: Datum, m: Marginal, oldAugment: Augment):Augment
  def goldMarginal(v: Datum, aug: Augment):(Marginal,Double)

  override def goldCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    val m = goldMarginal(value, augment)
    countsFromMarginal(value, m._1, augment, accum, scale)
  }
}