package scalanlp.trees

import scalanlp.util.Lens
import collection.mutable.ArrayBuffer

/**
 * Something we can throw in an AnnotatedLabel
 * @author dlwh
 */
@SerialVersionUID(1L)
trait Annotation extends Serializable

case class FunctionalTag(tag: String) extends Annotation

/**
 * The standard label used in the parser (used to be String).
 *
 * Useful for Klein-and-Manning style annotated labels and other explicit-annotation strategies
 * @author dlwh
 */
@SerialVersionUID(1L)
case class AnnotatedLabel(label: String,
                          parents: Seq[String] = Seq.empty,
                          siblings: Seq[Either[String, String]] = Seq.empty,
                          unaryChain: Seq[String] = Seq.empty,
                          features: Set[Annotation] = Set.empty) {

  def annotate(sym: Annotation) = copy(features = features + sym)

  def isIntermediate = label.nonEmpty && label.charAt(0) == '@'

  def baseLabel = label.dropWhile(_ == '@')

  def baseAnnotatedLabel = AnnotatedLabel(label,unaryChain=unaryChain)
  def clearFeatures = copy(features=Set.empty)

  override def toString = {
    val components = new ArrayBuffer[String]()
    if(unaryChain.nonEmpty || parents.nonEmpty) {
      var s = ""
      if(unaryChain.nonEmpty)
        s = unaryChain.mkString("(","^",")")
      components += parents.mkString(s,"^","")
    }
    if(siblings.nonEmpty) {
      val b = new StringBuilder()
      siblings foreach {
        case Left(sib) =>
          b ++= "\\"
          b ++= sib
        case Right(sib) =>
          b ++= "/"
          b ++= sib
      }
      components += b.toString
    }
    if(features.nonEmpty)
      components += features.toString

    if(components.nonEmpty) components.mkString(label+"[", ", ", "]")
    else label
  }
}

object AnnotatedLabel {
  val TOP = AnnotatedLabel("TOP")

  implicit val stringLens:Lens[AnnotatedLabel, String] = new Lens[AnnotatedLabel, String] with Serializable {
    def get(t: AnnotatedLabel) = t.label
    def set(t: AnnotatedLabel, u: String) = t.copy(u)
  }
}