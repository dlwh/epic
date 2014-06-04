package epic.slab

import scala.reflect.ClassTag
import java.net.URL
import epic.util.BinarySearch
import epic.slab.AnnotatedSpan.{EndFirstSpanOrdering, SpanOrdering}

trait Slab[ContentType, BaseAnnotationType, +AnnotationTypes <: BaseAnnotationType] {

  val content: ContentType

  def +[A <: BaseAnnotationType](annotation: A): Slab[ContentType, BaseAnnotationType, AnnotationTypes with A] = {
    ++[A](Iterator(annotation))
  }

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
trait AnnotatedSpan {
  def begin: Int
  def end: Int
}


object AnnotatedSpan {

  implicit object SpanOrdering extends Ordering[AnnotatedSpan] {
    override def compare(x: AnnotatedSpan, y: AnnotatedSpan): Int = {
      if      (x.begin < y.begin) -1
      else if (x.begin > y.begin)  1
      else if (x.end  < y.end)    -1
      else if (x.end > y.end)      1
      else                         0
    }
  }

  implicit object EndFirstSpanOrdering extends Ordering[AnnotatedSpan] {
    override def compare(x: AnnotatedSpan, y: AnnotatedSpan): Int = {
      if (x.end  < y.end)    -1
      else if (x.end > y.end)      1
      else if (x.begin < y.begin) -1
      else if (x.begin > y.begin)  1
      else                         0
    }
  }

  implicit class SpanInStringSlab(val span: AnnotatedSpan) extends AnyVal {
    def in[AnnotationTypes <: AnnotatedSpan](slab: StringSlab[AnnotationTypes]) =
      new StringSpanAnnotationOps(this.span, slab)
  }

  class StringSpanAnnotationOps[AnnotationType >: AnnotationTypes <: AnnotatedSpan: ClassTag, AnnotationTypes <: AnnotatedSpan](
    annotation: AnnotationType,
    slab: StringSlab[AnnotationTypes])
    extends SlabAnnotationOps[String, AnnotatedSpan, AnnotationType, AnnotationTypes](annotation, slab) {
    def content = this.slab.content.substring(this.annotation.begin, this.annotation.end)
  }
  
  implicit object StringAnnotationHasBounds extends Slab.HasBounds[AnnotatedSpan] {
    def covers(annotation1: AnnotatedSpan, annotation2: AnnotatedSpan): Boolean =
      annotation1.begin <= annotation2.begin && annotation2.end <= annotation1.end
    def follows(annotation1: AnnotatedSpan, annotation2: AnnotatedSpan): Boolean =
      annotation2.end <= annotation1.begin
    def precedes(annotation1: AnnotatedSpan, annotation2: AnnotatedSpan): Boolean =
      annotation1.end <= annotation2.begin
  }
}

// ===========
// Annotations
// ===========
case class Source(begin: Int, end: Int, url: URL) extends AnnotatedSpan
case class Sentence(begin: Int, end: Int, id: Option[String] = None) extends AnnotatedSpan
case class Segment(begin: Int, end: Int, id: Option[String] = None) extends AnnotatedSpan
case class Token(begin: Int, end: Int, token: String) extends AnnotatedSpan
case class PartOfSpeech(begin: Int, end: Int, tag: String, id: Option[String] = None) extends AnnotatedSpan
case class EntityMention(begin: Int, end: Int, entityType: String, id: Option[String] = None) extends AnnotatedSpan


object Slab {

  def apply[BaseAnnotationType <: AnnotatedSpan](content: String):StringSlab[BaseAnnotationType] = {
    new SortedSequenceSlab(content, Map.empty, Map.empty)
  }
  
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
      // FIXME: this should keep the annotations sorted by offset
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

  /**
   * This slab should be more efficient, especially for longer documents. It maintains the annotations in sorted order.
   *
   * @param content
   * @param annotations
   * @tparam ContentType
   * @tparam BaseAnnotationType
   * @tparam AnnotationType
   */
  private[slab] class SortedSequenceSlab[ContentType,
  BaseAnnotationType <: AnnotatedSpan,
  AnnotationType <: BaseAnnotationType](val content: ContentType,
                                        val annotations: Map[Class[_], Vector[BaseAnnotationType]] = Map.empty,
                                        val reverseAnnotations: Map[Class[_], Vector[BaseAnnotationType]] = Map.empty) extends Slab[ContentType, BaseAnnotationType, AnnotationType] {
    override def ++[A <: BaseAnnotationType](annotations: Iterator[A]): Slab[ContentType, BaseAnnotationType, AnnotationType with A] = {
      var newAnnotations = this.annotations
      val grouped = annotations.toIndexedSeq.groupBy(_.getClass)
      for( (clss, group) <- grouped) {
        newAnnotations = newAnnotations + (clss -> (newAnnotations.getOrElse(clss, Vector.empty) ++ group).sorted(SpanOrdering))
      }

      var reverseAnnotations = this.reverseAnnotations
      for( (clss, group) <- grouped) {
        reverseAnnotations = reverseAnnotations + (clss -> (reverseAnnotations.getOrElse(clss, Vector.empty) ++ group).sorted(EndFirstSpanOrdering))
      }
      new SortedSequenceSlab(content, newAnnotations, reverseAnnotations)
    }

    override def following[A >: AnnotationType <: BaseAnnotationType : ClassTag](annotation: BaseAnnotationType): Iterator[A] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var pos = BinarySearch.interpolationSearch(annotations, (_:AnnotatedSpan).begin, annotation.end)
        if(pos < 0) pos = ~pos
        annotations.view(pos, annotations.length)
      }.asInstanceOf[Iterator[A]]
    }

    override def preceding[A >: AnnotationType <: BaseAnnotationType : ClassTag](annotation: BaseAnnotationType): Iterator[A] = {
      reverseAnnotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var pos = BinarySearch.interpolationSearch(annotations, (_:AnnotatedSpan).end, annotation.begin + 1)
        if(pos < 0) pos = ~pos
        annotations.view(0, pos).reverseIterator
      }.asInstanceOf[Iterator[A]]
    }

    override def covered[A >: AnnotationType <: BaseAnnotationType : ClassTag](annotation: BaseAnnotationType): Iterator[A] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var begin = BinarySearch.interpolationSearch(annotations, (_:AnnotatedSpan).begin, annotation.begin)
        if(begin < 0) begin = ~begin
        annotations.view(begin, annotations.length).takeWhile(_.end <= annotation.end)
      }.asInstanceOf[Iterator[A]]
    }

    override def iterator[A >: AnnotationType <: BaseAnnotationType : ClassTag]: Iterator[A] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatten.asInstanceOf[Iterator[A]]
    }

  }
}
