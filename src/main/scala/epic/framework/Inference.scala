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
  type ExpectedCounts <: epic.framework.ExpectedCounts[ExpectedCounts]
  type Marginal <: epic.framework.Marginal
  def emptyCounts: ExpectedCounts

  def goldMarginal(v: Datum):(Marginal,Double)
  def marginal(v: Datum):(Marginal,Double)
  def countsFromMarginal(v: Datum, marg: Marginal, accum: ExpectedCounts, scale: Double):ExpectedCounts


  def guessCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts = {
    val marg = marginal(value)
    countsFromMarginal(value, marg._1, accum, scale)
  }

  def goldCounts(value: Datum, accum: ExpectedCounts, scale: Double):ExpectedCounts = {
    val marg = goldMarginal(value)
    countsFromMarginal(value, marg._1, accum, scale)
  }

  def expectedCounts(datum: Datum, accum: ExpectedCounts, scale: Double) = {
    guessCounts(datum, accum, scale)
    goldCounts(datum, accum, -scale)
    accum
  }
}


trait AugmentableInference[Datum,Augment] extends Inference[Datum] {
  def baseAugment(v: Datum):Augment

  def marginal(v: Datum):(Marginal,Double) = marginal(v, baseAugment(v))
  def marginal(v: Datum, aug: Augment):(Marginal,Double)

  def goldMarginal(v: Datum):(Marginal,Double)  = goldMarginal(v, baseAugment(v))
  def goldMarginal(v: Datum, aug: Augment):(Marginal,Double)

  def guessCounts(datum: Datum,  augment: Augment, accum: ExpectedCounts, scale: Double) = {
    val m = marginal(datum,augment)
    countsFromMarginal(datum,m._1, accum, scale)
  }

  def goldCounts(value: Datum, augment: Augment, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    val m = goldMarginal(value, augment)
    countsFromMarginal(value, m._1, accum, scale)
  }

}

trait ProjectableInference[Datum,Augment] extends AugmentableInference[Datum,Augment] {
  def project(v: Datum, m: Marginal, oldAugment: Augment):Augment
}
