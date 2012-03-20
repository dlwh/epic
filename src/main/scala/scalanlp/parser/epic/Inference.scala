package scalanlp.parser.epic

/**
 * 
 * @author dlwh
 */
trait Inference[Datum] extends Serializable {
  type ExpectedCounts;
  def expectedCounts(datum: Datum):ExpectedCounts
}

trait GoldGuessInference[Datum] extends Inference[Datum] {
  type ExpectedCounts <: scalanlp.parser.epic.ExpectedCounts[ExpectedCounts]

  def guessCounts(value: Datum):ExpectedCounts
  def goldCounts(value: Datum):ExpectedCounts

  def expectedCounts(datum: Datum):ExpectedCounts = {
//    guessCounts(datum)
    guessCounts(datum) -= goldCounts(datum)
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
  def guessCountsFromMarginals(v: Datum, marg: Marginal, aug: Augment):ExpectedCounts
  def guessCounts(datum: Datum, augment: Augment) = {
    val m = marginal(datum,augment)
    guessCountsFromMarginals(datum,m._1,augment)
  }
}

trait ProjectableInference[Datum,Augment] extends MarginalInference[Datum,Augment] {
  def project(v: Datum, m: Marginal, oldAugment: Augment):Augment

}

