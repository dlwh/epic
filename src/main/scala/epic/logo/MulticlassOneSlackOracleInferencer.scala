package epic.logo

import breeze.math.MutableInnerProductModule

class MulticlassOneSlackOracleInferencer[L, F, W](
  val inferencer: MulticlassOracleInferencer[L, F, W])(implicit space: MutableInnerProductModule[W, Double])
    extends OracleInferencer[Seq[LabeledDatum[L, F]], W, Unit] {
  import space._
  type Y = Seq[L]

  def oracle(weights: Weights[W], instance: Seq[LabeledDatum[L, F]]): (Seq[L], W, Double, Unit) = {
    val zeroVector = space.zeroLike(inferencer.labelConjoiner(instance.head.label, instance.head.features))
    val (labels, fvs, losses) = instance.map(i => inferencer.oracle(weights, i))
      .foldLeft((Seq.empty[L], zeroVector, 0.0)) { (acc, curr) =>
        (acc._1 :+ curr._1, acc._2 + curr._2, acc._3 + curr._3)
      }
    (labels, fvs, losses, ())
  }

  def initialState: Unit = ()

  def reduceStates(state1: Unit, state2: Unit): Unit = ()

}
