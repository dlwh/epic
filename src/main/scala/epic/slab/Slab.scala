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

  def append[A:ClassTag](region: RegionType, annotation: A): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    this.+[A](region -> annotation)
  }

  def +[A:ClassTag](pair: (RegionType, A)): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    addLayer[A](pair)
  }

  @deprecated("use addLayer", "0.3.1")
  def ++[A:ClassTag](annotations: TraversableOnce[(RegionType, A)]): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    addLayer[A](annotations)
  }

  def addLayer[A:ClassTag](annotations: TraversableOnce[(RegionType, A)]): Slab[ContentType, RegionType, AnnotationTypes with A]

  def addLayer[A:ClassTag](annotations: (RegionType, A)*): Slab[ContentType, RegionType, AnnotationTypes with A] = {
    addLayer[A](annotations)
  }

  /** Can't remove the type, but you can upcast */
  def removeLayer[A >: AnnotationTypes:ClassTag]: Slab[ContentType, RegionType, AnnotationTypes]

  /** useful for downcasting */
  def checkedCast[A: ClassTag]:Option[Slab[ContentType, RegionType, AnnotationTypes with A]] = {
    if (!hasLayer[A]) {
      None
    } else {
      Some(this.asInstanceOf[Slab[ContentType, RegionType, AnnotationTypes with A]])
    }
  }

  /** Queries whether we have annotations of this type, even if the slab
    *  doesn't have this type. Sometimes you just have to cast... */
  def hasLayer[A :ClassTag]:Boolean

  def iterator[A >: AnnotationTypes: ClassTag]: Iterator[(RegionType, A)]

  def indexedSeq[A >: AnnotationTypes : ClassTag]: IndexedSeq[(RegionType, A)]

  /**
   * Returns annotations wholly contained in the region
   * @param region
   * @tparam A
   * @return
   */
  def covered[A >: AnnotationTypes: ClassTag](region: RegionType): IndexedSeq[(RegionType, A)]

  /**
   * Returns annotations that are entirely before the region
   * @param region
   * @tparam A
   * @return
   */
  def preceding[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)]

  def following[A >: AnnotationTypes: ClassTag](region: RegionType): Iterator[(RegionType, A)]

  def stringRep[A >: AnnotationTypes: ClassTag] = {
    iterator[A].mkString("\n")
  }

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
      if (x.end  < y.end)    -1
      else if (x.end > y.end)      1
      else if (x.begin < y.begin) -1
      else if (x.begin > y.begin)  1
      else                         0
    }
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
    def apply(region: Region, t: T): T
  }

  implicit object SpanStringExtractRegion extends ExtractRegion[Span, String] {
    def apply(region: Span, t: String) = t.substring(region.begin, region.end)
  }

  def apply(content: String):StringSlab[Any] = {
    new SortedSequenceSlab(content, Map.empty, Map.empty)
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

    override def addLayer[A:ClassTag](annotations: TraversableOnce[(Span, A)]): Slab[ContentType, Span, AnnotationType with A] = {
      val ann = annotations.toSeq
      var newAnnotations = this.annotations
      val clss = implicitly[ClassTag[A]].runtimeClass
      newAnnotations = newAnnotations + (clss -> (newAnnotations.getOrElse(clss, Vector.empty) ++ ann).sortBy(_._1)(SpanOrdering))

      val reverseAnnotations = {
        this.reverseAnnotations + (clss -> (this.reverseAnnotations.getOrElse(clss, Vector.empty) ++ ann).sortBy(_._1)(EndFirstSpanOrdering))
      }

      new SortedSequenceSlab(content, newAnnotations, reverseAnnotations)
    }

    override def removeLayer[A >: AnnotationType: ClassTag]: Slab[ContentType, Span, AnnotationType] = {
      new SortedSequenceSlab(content,
        annotations - implicitly[ClassTag[A]].runtimeClass,
        reverseAnnotations - implicitly[ClassTag[A]].runtimeClass)
    }

    /** Queries whether we have annotations of this type, even if the slab
      * doesn't have this type. Sometimes you just have to cast... */
    override def hasLayer[A: ClassTag]: Boolean = {
      annotations.contains(implicitly[ClassTag[A]].runtimeClass)
    }

    override def following[A >: AnnotationType: ClassTag](region: Span): Iterator[(Span, A)] = {
      val annotations = selectAnnotations[A]
      var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.end)
      if (pos < 0) pos = ~pos
      annotations.view(pos, annotations.length).iterator
    }

    override def preceding[A >: AnnotationType : ClassTag](region: Span): Iterator[(Span, A)] = {
      val annotations = selectReverse[A]
      var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.end, region.begin + 1)
      if (pos < 0) pos = ~pos
      annotations.view(0, pos).reverseIterator
    }

    override def covered[A >: AnnotationType : ClassTag](region: Span): IndexedSeq[(Span, A)] = {
      val annotations = selectAnnotations[A]
      var begin = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.begin)
      if (begin < 0) begin = ~begin
      var end = annotations.indexWhere(_._1.end > region.end, begin)
      if (end < 0) end = annotations.length
      annotations.slice(begin, end)
    }

    override def iterator[A >: AnnotationType : ClassTag]: Iterator[(Span, A)] = {
      selectAnnotations[A].iterator
    }

    override def indexedSeq[A >: AnnotationType : ClassTag]: IndexedSeq[(Span, A)] = {
      selectAnnotations[A]
    }

    private def selectAnnotations[A >: AnnotationType : ClassTag]: IndexedSeq[(Span, A)] = {
      annotations.getOrElse(implicitly[ClassTag[A]].runtimeClass, IndexedSeq.empty).asInstanceOf[IndexedSeq[(Span, A)]]
    }

    private def selectReverse[A >: AnnotationType : ClassTag]:  IndexedSeq[(Span, A)] = {
      reverseAnnotations.getOrElse(implicitly[ClassTag[A]].runtimeClass, IndexedSeq.empty).asInstanceOf[IndexedSeq[(Span, A)]]
    }

    override def stringRep[A >: AnnotationType: ClassTag] = {
      iterator[A].map { case (Span(begin, end), x) => s"Span($begin, $end) $x"}.mkString("\n")
    }

  }

}
