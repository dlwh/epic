package epic.framework

/**
 * A class that returns an augment that gives higher
 * scores to spans that are wrong. Used for training mostly.
 *
 * @author dlwh
 **/
trait LossAugmentation[Datum, Augment] extends (Datum=>Augment) {
  def lossAugmentation(datum: Datum):Augment
  def apply(datum: Datum): Augment = lossAugmentation(datum)
}
