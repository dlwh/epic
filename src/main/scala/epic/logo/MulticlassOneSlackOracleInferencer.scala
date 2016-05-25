package epic.logo

import breeze.math.MutableInnerProductModule

class MulticlassOneSlackOracleInferencer[L, F, W](
  val inferencer: MulticlassOracleInferencer[L, F, W])(implicit space: MutableInnerProductModule[W, Double])
    extends OracleInferencer[Seq[LabeledDatum[L, F]], Seq[L], W] {

  def oracle(weights : Weights[W], instance : Seq[LabeledDatum[L, F]]) : (Seq[L], FeatureVector[W], Double) = {
    val zeroVector =
      FeatureVector(space.zeroLike(inferencer.labelConjoiner(instance.head.label, instance.head.features)))
    instance.map(i => inferencer.oracle(weights, i))
    .foldLeft((Seq.empty[L], zeroVector, 0.0)) { (acc, curr) =>
      (acc._1 :+ curr._1, acc._2 + curr._2, acc._3 + curr._3)
    }
  }

}
