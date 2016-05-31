package epic.logo

import breeze.math.MutableInnerProductModule

class MulticlassOneSlackLossAugmentedArgmaxInferencer[L, F, W](
  inferencer: MulticlassLossAugmentedArgmaxInferencer[L, F, W],
  emptyFeatureVector: W)(implicit space: MutableInnerProductModule[W, Double])
    extends LossAugmentedArgmaxInferencer[Seq[LabeledDatum[L, F]], W, Unit] {
  override type Y = Seq[L]
  import space._

  def lossAugmentedArgmax(weights: Weights[W], instance: Seq[LabeledDatum[L, F]], weightsWeight: Double,
                          lossWeight: Double): (Seq[L], W, Double, Unit) = {
   val (labels, fvs, losses) =
     instance.map(i => inferencer.lossAugmentedArgmax(weights, i, weightsWeight, lossWeight))
      .foldLeft((Seq.empty[L], emptyFeatureVector, 0.0)) { (acc, curr) =>
        (acc._1 :+ curr._1, acc._2 + curr._2, acc._3 + curr._3)
      }
   (labels, fvs, losses, ())
  }

  def initialState: Unit = ()

  def reduceStates(s1: Unit, s2: Unit): Unit = ()

}
