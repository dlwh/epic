package epic.slab

import shapeless._
import shapeless.syntax.typeable._
import ops.hlist._
import epic.slab.typeclasses._

/**
 * A Slab is the core "document" type. It holds the document itself as
 * well as the annotations.
 */

class Slab[Content, L <: HList](val content: Content, val annotations: L) {
  // Select a single element from an hlist. For more than one element
  // in a single AnalysisFunction, see @selectMany
  def select[T](implicit sel: SubSelector[L, Vector[T]]): Vector[T] = sel(annotations)
  // Convenience overload for slab.select[T](index)
  def select[T](index: Int)(implicit sel: SubSelector[L, Vector[T]]): T = sel(annotations)(index)
  // Select multiple elements, creating a new HList. Preferable over
  // @select because it only requires a single evidence.
  def selectMany[T <: HList](implicit sel: SelectMany.Aux[L, T, T]): T = sel(annotations)

  // Returns a new slab with the new annotations added. Preferably
  // only call once per AnalysisFunction, because the performance is
  // questionable.
  def add[A, Tmp <: HList, Result <: HList](newAnnotations: Vector[A])(implicit adder: Adder.Aux[L, A, Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, newAnnotations))
  }
  def add[A, Tmp <: HList, Result <: HList](newAnnotation: A)(implicit adder: Adder.Aux[L, A, Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, Vector(newAnnotation)))
  }
}

object Slab {
  def apply[C](content: C): Slab[C, HNil] = new Slab[C, HNil](content, HNil)
  def apply[C, L <: HList](content: C, annotations: L): Slab[C, L] = new Slab[C, L](content, annotations)

  implicit class StringSlabOps[L <: HList](slab: Slab[String, L]) {
    def substring(span: SpanAnnotation): String = span.substring(slab.content)
    def substring(span: Span): String = substring(span.begin, span.end)
    def substring(begin: Int, end: Int): String = slab.content.substring(begin, end)
  }

  implicit final class SpanIndexSlabOps[In <: HList](slab: Slab[String, In]) {
    def covered[T <: SpanAnnotation](span: Span)(implicit sel: SubSelector[In, Vector[T]]): Iterable[T] = {
      spanIndex(sel).apply(span)
    }
    def spanIndex[T <: SpanAnnotation](implicit sel: SubSelector[In, Vector[T]]) = SpanIndex(slab.select[T](sel))
    def covered[T <: SpanAnnotation](span: Span, index: SpanIndex[T]): Iterable[T] = {
      index(span)
    }
  }
}
