package epic.framework

/**
 * AnnotatingInference adds the ability to take
 * a Marginal and attach (or replace) new information
 * to the Datum.
 * @author dlwh
 */
trait AnnotatingInference[Datum] extends Inference[Datum] {
 def annotate(datum: Datum, m: Marginal):Datum
}
