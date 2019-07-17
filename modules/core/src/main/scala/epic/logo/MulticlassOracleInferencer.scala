package epic.logo

import breeze.util.Index
import breeze.math.MutableInnerProductModule

case class MulticlassOracleInferencer[L, F, W](
  validLabels: IndexedSeq[L], labelConjoiner: (L, F) => W)(implicit space: MutableInnerProductModule[W, Double])
    extends OracleInferencer[LabeledDatum[L, F], W, Unit] {
  override type Y = L

  def oracle(weights : Weights[W], instance : LabeledDatum[L, F]) : (L, W, Double, Unit) = {
    (instance.label, labelConjoiner(instance.label, instance.features), 0.0, ())

  }

  def initialState = ()
  def reduceStates(a: Unit, b: Unit) = ()

}
