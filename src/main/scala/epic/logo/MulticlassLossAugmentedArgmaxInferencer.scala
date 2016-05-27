package epic.logo
import breeze.util.Index
import breeze.math.MutableInnerProductModule

class MulticlassLossAugmentedArgmaxInferencer[L, F, W](validLabels: IndexedSeq[L], labelConjoiner: (L, F) => W)
    extends LossAugmentedArgmaxInferencer[LabeledDatum[L, F], W, Unit] {
  type Y = L

  def lossAugmentedArgmax(weights: Weights[W], instance: LabeledDatum[L, F],
                          weightsWeight: Double, lossWeight: Double): (L, W, Double, Unit) = {
    validLabels.map(label => {
      val loss = if (instance.label == null || label.equals(instance.label)) 0.0 else 1.0
      val labeledFeatureVector = labelConjoiner(label, instance.features)
      (label,labeledFeatureVector, loss, ())
    }).maxBy { case (label, features, loss, _) => weightsWeight * (weights * features) + lossWeight * loss }
  }

  def initialState: Unit = ()
  def reduceStates(a: Unit, b: Unit): Unit = ()
}
