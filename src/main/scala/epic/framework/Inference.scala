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
  type ExpectedCounts

  def expectedCounts(datum: Datum): ExpectedCounts
}

trait GoldGuessInference[Datum] extends Inference[Datum] {
  type ExpectedCounts <: epic.framework.ExpectedCounts[ExpectedCounts]

  def guessCounts(value: Datum):ExpectedCounts
  def goldCounts(value: Datum):ExpectedCounts

  def expectedCounts(datum: Datum):ExpectedCounts = {
    val guess = guessCounts(datum)
    val gold = goldCounts(datum)
    if(guess.loss * 1.0001 < gold.loss ) {
      println("Odd... guess ll is smaller than gold score?" + guess.loss + " " + gold.loss)
    }
    guess -= gold
    guess
  }
}

/**
 * Used for loss-augmented inference or EP inference
 */
trait AugmentableInference[Datum, Augment] extends GoldGuessInference[Datum] {
  def baseAugment(v: Datum):Augment

  def guessCounts(value: Datum):ExpectedCounts = guessCounts(value,baseAugment(value))
  def guessCounts(value: Datum, augment: Augment):ExpectedCounts

  def goldCounts(value: Datum):ExpectedCounts = goldCounts(value,baseAugment(value))
  def goldCounts(value: Datum, augment: Augment):ExpectedCounts
}

trait MarginalInference[Datum,Augment] extends AugmentableInference[Datum,Augment] {
  type Marginal <: AnyRef
  def marginal(v: Datum, aug: Augment):(Marginal,Double)
  def countsFromMarginal(v: Datum, marg: Marginal, aug: Augment):ExpectedCounts
  def guessCounts(datum: Datum, augment: Augment) = {
    val m = marginal(datum,augment)
    countsFromMarginal(datum,m._1,augment)
  }
}

trait ProjectableInference[Datum,Augment] extends MarginalInference[Datum,Augment] {
  def project(v: Datum, m: Marginal, oldAugment: Augment):Augment
}

trait FullProjectableInference[Datum, Augment] extends ProjectableInference[Datum, Augment] {
  def projectGold(v: Datum, m: Marginal, oldAugment: Augment):Augment
  def goldMarginal(v: Datum, aug: Augment):(Marginal,Double)

  def goldCounts(value: Datum, augment: Augment): ExpectedCounts = {
    val m = goldMarginal(value, augment)
    countsFromMarginal(value, m._1, augment)
  }
}