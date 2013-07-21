package chalk.slab

import scala.reflect.ClassTag

trait Slab[ContentType, BaseAnnotationType, +AnnotationTypes <: BaseAnnotationType] {

  val content: ContentType

  def ++[A <: BaseAnnotationType](annotations: Iterator[A]): Slab[ContentType, BaseAnnotationType, AnnotationTypes with A]

  def iterator[A >: AnnotationTypes <: BaseAnnotationType: ClassTag]: Iterator[A]

  def covered[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]

  def preceding[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]

  def following[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]

  def stringRep[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = {
    iterator[A].mkString("\n")
  }
  
}

abstract class SlabAnnotationOps[ContentType, BaseAnnotationType, AnnotationType >: AnnotationTypes <: BaseAnnotationType: ClassTag, AnnotationTypes <: BaseAnnotationType](
  val annotation: AnnotationType,
  val slab: Slab[ContentType, BaseAnnotationType, AnnotationTypes]) {

  def content: ContentType

  def covered[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.covered[A](this.annotation)

  def preceding[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.preceding[A](this.annotation)

  def following[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.following[A](this.annotation)
}

// =========================
// Annotation infrastructure
// =========================
trait StringAnnotation {
  val begin: Int
  val end: Int
  def in[AnnotationTypes <: StringAnnotation](slab: Slab[String, StringAnnotation, AnnotationTypes]) =
    new SlabAnnotationOps(this, slab) {
      def content = this.slab.content.substring(this.annotation.begin, this.annotation.end)
    }
}

object StringAnnotation {
  implicit object StringAnnotationHasBounds extends Slab.HasBounds[StringAnnotation] {
    def covers(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation1.begin <= annotation2.begin && annotation2.end <= annotation1.end
    def follows(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation2.end <= annotation1.begin
    def precedes(annotation1: StringAnnotation, annotation2: StringAnnotation): Boolean =
      annotation1.end <= annotation2.begin
  }
}

// ===========
// Annotations
// ===========
case class Sentence(val begin: Int, val end: Int) extends StringAnnotation
case class Token(val begin: Int, val end: Int) extends StringAnnotation


object Slab {
  def apply[ContentType, BaseAnnotationType: HasBounds](content: ContentType): Slab[ContentType, BaseAnnotationType, BaseAnnotationType] =
    new HorribleInefficientSlab(content)

  /**
   * This trait has the minimum necessary for the implementation below.
   *
   * An efficient implementation will probably need some other set of operations.
   */
  trait HasBounds[AnnotationType] {
    def covers(annotation1: AnnotationType, annotation2: AnnotationType): Boolean
    def precedes(annotation1: AnnotationType, annotation2: AnnotationType): Boolean
    def follows(annotation1: AnnotationType, annotation2: AnnotationType): Boolean
  }

  private[slab] class HorribleInefficientSlab[ContentType, BaseAnnotationType, AnnotationTypes <: BaseAnnotationType](
    val content: ContentType,
    val _annotations: Seq[Any] = Seq.empty)(
      implicit hasBounds: HasBounds[BaseAnnotationType])
    extends Slab[ContentType, BaseAnnotationType, AnnotationTypes] {

    def ++[AnnotationType](annotations: Iterator[AnnotationType]): Slab[ContentType, BaseAnnotationType, AnnotationTypes with AnnotationType] =
      new HorribleInefficientSlab(this.content, this._annotations ++ annotations)

    def iterator[A >: AnnotationTypes <: BaseAnnotationType: ClassTag]: Iterator[A] =
      this._annotations.iterator.collect {
        case annotation: A => annotation
      }

    def covered[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A] =
      this.iterator[A].filter(a => hasBounds.covers(annotation, a))

    def following[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A] =
      this.iterator[A].filter(a => hasBounds.follows(a, annotation))

    def preceding[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A] =
      this.iterator[A].filter(a => hasBounds.precedes(a, annotation)).toSeq.reverseIterator

  }
}
