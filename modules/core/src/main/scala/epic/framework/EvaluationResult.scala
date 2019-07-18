package epic.framework

/**
 * Marker for the output of an evaluation routine.
 * @tparam R self type
 *
 * @author dlwh
 */
trait EvaluationResult[R<:EvaluationResult[R]] { this: R =>
  def +(other: R):R
}
