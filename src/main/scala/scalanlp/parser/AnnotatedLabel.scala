package scalanlp.parser

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1L)
trait Annotation extends Serializable

/**
 * Useful for Klein-and-Manning style annotated labels and other explicit-annotation strategies
 * @author dlwh
 */
case class AnnotatedLabel(label: String,
                          parents: Seq[String] = Seq.empty,
                          siblings: Seq[Either[String,String]] = Seq.empty,
                          features: Set[Annotation] = Set.empty) {
  def annotate(sym: Annotation) = copy(features = features + sym)
  def isIntermediate = label.nonEmpty && label.charAt(0) == '@'
  def baseLabel = label.dropWhile(_ == '@')

  override lazy val hashCode = {
    scala.runtime.ScalaRunTime._hashCode(this)
  }
}