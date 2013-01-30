package epic.framework

/**
 * 
 * @author dlwh
 */
trait AnnotatingInference[Datum] extends Inference[Datum] {
 def annotate(datum: Datum, m: Marginal):Datum
}
