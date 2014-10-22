package epic.slab
import shapeless._
import ops.hlist._
import epic.trees.Span

// TODO: Get one which supports multiple types via HLists
class SpanIndex[C, T](content: C, data: Vector[T]) {
  def apply(span: Span): Vector[T] = ???
}

object SpanIndex {
  def apply[C, T](content: C, data: Vector[T]) = new SpanIndex[C, T](content, data)
}

object Indexes {
  implicit final class SpanIndexSlabOps[C, In](slab: Slab[C, In]) {
    def covered[T](span: Span)(implicit sel: Selector[In, Vector[T]]): Vector[T] = {
      spanIndex(sel).apply(span)
    }
    def spanIndex[T](implicit sel: Selector[In, Vector[T]]) = SpanIndex(slab.content, slab.select[T](sel))
    def covered[T](span: Span, index: SpanIndex[C, T]): Vector[T] = {
      index(span)
    }
  }
}
