package chalk.slab

import scala.reflect.ClassTag

trait SLAB[ContentType, BaseAnnotationType, +AnnotationTypes <: BaseAnnotationType] {

  val content: ContentType

  def ++[A <: BaseAnnotationType](annotations: Iterator[A]): SLAB[ContentType, BaseAnnotationType, AnnotationTypes with A]

  def iterator[A >: AnnotationTypes <: BaseAnnotationType: ClassTag]: Iterator[A]

  def covered[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]

  def preceding[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]

  def following[A >: AnnotationTypes <: BaseAnnotationType: ClassTag](annotation: BaseAnnotationType): Iterator[A]
}

abstract class SLABAnnotationOps[ContentType, BaseAnnotationType, AnnotationType >: AnnotationTypes <: BaseAnnotationType: ClassTag, AnnotationTypes <: BaseAnnotationType](
  val annotation: AnnotationType,
  val slab: SLAB[ContentType, BaseAnnotationType, AnnotationTypes]) {

  def content: ContentType

  def covered[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.covered[A](this.annotation)

  def preceding[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.preceding[A](this.annotation)

  def following[A >: AnnotationTypes <: BaseAnnotationType: ClassTag] = this.slab.following[A](this.annotation)
}

object SLAB {
  def apply[ContentType, BaseAnnotationType: HasBounds](content: ContentType): SLAB[ContentType, BaseAnnotationType, BaseAnnotationType] =
    new HorribleInefficientSLAB(content)

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

  private[slab] class HorribleInefficientSLAB[ContentType, BaseAnnotationType, AnnotationTypes <: BaseAnnotationType](
    val content: ContentType,
    val _annotations: Seq[Any] = Seq.empty)(
      implicit hasBounds: HasBounds[BaseAnnotationType])
    extends SLAB[ContentType, BaseAnnotationType, AnnotationTypes] {

    def ++[AnnotationType](annotations: Iterator[AnnotationType]): SLAB[ContentType, BaseAnnotationType, AnnotationTypes with AnnotationType] =
      new HorribleInefficientSLAB(this.content, this._annotations ++ annotations)

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
