package epic.slab

import scala.reflect.ClassTag
import java.net.URL
import epic.util.BinarySearch
import epic.slab.AnnotatedSpan.{EndFirstSpanOrdering, SpanOrdering}
import epic.trees.Span

/**
 * A Slab is the core "document" type in Epic. It represents a document and a set of annotations on that document,
 * such as sentence boundaries, tokens, named entity spans, etc. The ContentType is the type of the document--typically
 * a string--and the AnnotationTypes parameter is an encoding of the kinds of annotations that are present. [[epic.slab.AnalysisFunction]]s
 * can be used to add new annotations to a Slab that have the prerequisite annotations.
 * @tparam ContentType
 * @tparam RegionType
 * @tparam AnnotationTypes
 */
trait Slab[ContentType, RegionType, +AnnotationTypes] {

  val content: ContentType

  def spanned(region: RegionType):ContentType

  def append[A](region: RegionType, annotation: A): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    this.+[A](region -> annotation)
  }

  def +[A](pair: (RegionType, A)): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    ++[A](Iterator(pair))
  }

  def ++[A](annotations: TraversableOnce[(RegionType, A)]): Slab[ContentType, RegionType, AnnotationTypes with A]

  def iterator[A >: AnnotationTypes: ClassTag]: Iterator[(RegionType, A)]

  def covered[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)]

  def preceding[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)]

  def following[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)]

  def stringRep[A >: AnnotationTypes: ClassTag] = {
    iterator[A].mkString("\n")
  }
  
}

/*
abstract class SlabAnnotationOps[ContentType, RegionType, AnnotationTypes](
  val region: RegionType,
  val slab: Slab[ContentType, RegionType, AnnotationTypes]) {

  def content: ContentType

  def covered[A >: AnnotationTypes : ClassTag] = this.slab.covered[A](this.annotation)

  def preceding[A >: AnnotationTypes: ClassTag] = this.slab.preceding[A](this.annotation)

  def following[A >: AnnotationTypes: ClassTag] = this.slab.following[A](this.annotation)
}
*/

// =========================
// Annotation infrastructure
// =========================
trait AnnotatedSpan {
  def begin: Int
  def end: Int
}


object AnnotatedSpan {

  implicit object SpanOrdering extends Ordering[Span] {
    override def compare(x: Span, y: Span): Int = {
      if      (x.begin < y.begin) -1
      else if (x.begin > y.begin)  1
      else if (x.end  < y.end)    -1
      else if (x.end > y.end)      1
      else                         0
    }
  }

  implicit object EndFirstSpanOrdering extends Ordering[Span] {
    override def compare(x: Span, y: Span): Int = {
      if      (x.end < y.end)    -1
      else if (x.end > y.end)      1
      else if (x.begin < y.begin) -1
      else if (x.begin > y.begin)  1
      else                         0
    }
  }

  /*
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
  */
  
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
case class Source(url: URL)
case class Sentence(id: Option[String] = None)
case class Segment(id: Option[String] = None)
case class Token(token: String)
case class PartOfSpeech(tag: String, id: Option[String] = None)
case class EntityMention(entityType: String, id: Option[String] = None)


object Slab {

  trait ExtractRegion[Region, T] {
    def apply(region: Region, t: T):T
  }

  implicit object SpanStringExtractRegion extends ExtractRegion[Span, String] {
    def apply(region: Span, t: String) = t.substring(region.begin, region.end)
  }

  def apply(content: String):StringSlab[Any] = {
    new SortedSequenceSlab(content, Map.empty, Map.empty)
  }
  
  def apply[ContentType, RegionType](content: ContentType)
                                    (implicit hasBounds: HasBounds[RegionType],
                                     extract: ExtractRegion[RegionType, ContentType]): Slab[ContentType, RegionType, Any] =
    new HorribleInefficientSlab[ContentType, RegionType, Any](content)

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

  private[slab] class HorribleInefficientSlab[ContentType, RegionType, AnnotationTypes](
    val content: ContentType,
    val _annotations: Seq[(RegionType, Any)] = Seq.empty)(
      implicit hasBounds: HasBounds[RegionType], extract: ExtractRegion[RegionType, ContentType])
    extends Slab[ContentType, RegionType, AnnotationTypes] {


    override def spanned(region: RegionType): ContentType = extract(region, content)

    def ++[AnnotationType](annotations: TraversableOnce[(RegionType, AnnotationType)]): Slab[ContentType, RegionType, AnnotationTypes with AnnotationType] =
      // FIXME: this should keep the annotations sorted by offset
      new HorribleInefficientSlab(this.content, this._annotations ++ annotations)

    def iterator[A >: AnnotationTypes: ClassTag]: Iterator[(RegionType, A)] =
      this._annotations.iterator.collect {
        case pair@(region, annotation: A) => pair.asInstanceOf[(RegionType, A)]
      }

    def covered[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)] =
      this.iterator[A].filter(a => hasBounds.covers(region, a._1))

    def following[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)] =
      this.iterator[A].filter(a => hasBounds.follows(a._1, region))

    def preceding[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)] =
      this.iterator[A].filter(a => hasBounds.precedes(a._1, region)).toSeq.reverseIterator

  }

  /**
   * This slab should be more efficient, especially for longer documents. It maintains the annotations in sorted order.
   *
   * @param content
   * @param annotations
   * @tparam ContentType
   * @tparam AnnotationType
   */
  private[slab] class SortedSequenceSlab[ContentType,
                                         AnnotationType](val content: ContentType,
                                                         val annotations: Map[Class[_], Vector[(Span, Any)]] = Map.empty,
                                                         val reverseAnnotations: Map[Class[_], Vector[(Span, Any)]] = Map.empty)(implicit extract: ExtractRegion[Span, ContentType]) extends Slab[ContentType, Span, AnnotationType] {


    override def spanned(region: Span): ContentType = extract(region, content)

    override def ++[A](annotations: TraversableOnce[(Span, A)]): Slab[ContentType, Span, AnnotationType with A] = {
      var newAnnotations = this.annotations
      val grouped = annotations.toIndexedSeq.groupBy(_._2.getClass)
      for( (clss, group) <- grouped) {
        newAnnotations = newAnnotations + (clss -> (newAnnotations.getOrElse(clss, Vector.empty) ++ group).sortBy(_._1)(SpanOrdering))
      }

      var reverseAnnotations = this.reverseAnnotations
      for( (clss, group) <- grouped) {
        reverseAnnotations = reverseAnnotations + (clss -> (reverseAnnotations.getOrElse(clss, Vector.empty) ++ group).sortBy(_._1)(EndFirstSpanOrdering))
      }
      new SortedSequenceSlab(content, newAnnotations, reverseAnnotations)
    }

    override def following[A >: AnnotationType: ClassTag](region: Span): Iterator[(Span, A)] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.end)
        if(pos < 0) pos = ~pos
        annotations.view(pos, annotations.length)
      }.asInstanceOf[Iterator[(Span, A)]]
    }

    override def preceding[A >: AnnotationType : ClassTag](region: Span): Iterator[(Span, A)] = {
      reverseAnnotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.end, region.begin + 1)
        if(pos < 0) pos = ~pos
        annotations.view(0, pos).reverseIterator
      }.asInstanceOf[Iterator[(Span, A)]]
    }

    override def covered[A >: AnnotationType : ClassTag](region: Span): Iterator[(Span, A)] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
        var begin = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.begin)
        if(begin < 0) begin = ~begin
        annotations.view(begin, annotations.length).takeWhile(_._1.end <= region.end)
      }.asInstanceOf[Iterator[(Span, A)]]
    }

    override def iterator[A >: AnnotationType : ClassTag]: Iterator[(Span, A)] = {
      annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatten.asInstanceOf[Iterator[(Span, A)]]
    }

  }



}
