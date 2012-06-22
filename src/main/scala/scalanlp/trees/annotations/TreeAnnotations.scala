package scalanlp.trees
package annotations

object TreeAnnotations {


  // KM Annotations
  trait KMAnnotation extends Annotation

  trait Aux extends KMAnnotation
  case object AuxBe extends Aux
  case object Aux extends Aux
  case object AuxHave extends Aux

  case object VPisVBF extends KMAnnotation
  case class VPisX(x: String) extends KMAnnotation

  sealed trait INKind extends KMAnnotation
  case object IN_N extends INKind
  case object IN_Q extends INKind
  case object IN_SCC extends INKind
  case object IN_SC extends INKind

  case object NP_Possessive extends KMAnnotation
  case object BaseNP extends KMAnnotation
  case object RRNP extends KMAnnotation

  case object RealUnary extends KMAnnotation
  case object ExternalUnary extends KMAnnotation

  case class Dom(str: String) extends KMAnnotation
}
