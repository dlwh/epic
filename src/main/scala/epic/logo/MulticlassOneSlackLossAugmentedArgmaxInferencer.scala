package epic.logo

class MulticlassOneSlackLossAugmentedArgmaxInferencer[L, F, W](
    inferencer: MulticlassLossAugmentedArgmaxInferencer[L, F, W],
    emptyFeatureVector: FeatureVector[W]) extends LossAugmentedArgmaxInferencer[Seq[LabeledDatum[L, F]], Seq[L], W] {

  def lossAugmentedArgmax(weights: Weights[W], instance: Seq[LabeledDatum[L, F]], weightsWeight: Double,
                          lossWeight: Double): (Seq[L], FeatureVector[W], Double) = {
    instance.map(i => inferencer.lossAugmentedArgmax(weights, i, weightsWeight, lossWeight))
      .foldLeft((Seq.empty[L], emptyFeatureVector, 0.0)) { (acc, curr) =>
        (acc._1 :+ curr._1, acc._2 + curr._2, acc._3 + curr._3)
      }
  }

}
