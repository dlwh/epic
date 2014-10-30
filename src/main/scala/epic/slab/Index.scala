package epic.slab
import shapeless._
import ops.hlist._
import epic.trees.Span

// TODO: Get one which supports multiple types via HLists
class SpanIndex[T](content: String, data: Vector[T]) {
  def apply(span: Span): Vector[T] = ???
}

object SpanIndex {
  def apply[T](content: String, data: Vector[T]) = new SpanIndex[T](content, data)
}

object Indexes {
  implicit final class SpanIndexSlabOps[In <: HList](slab: Slab[String, In]) {
    def covered[T](span: Span)(implicit sel: Selector[In, Vector[T]]): Vector[T] = {
      spanIndex(sel).apply(span)
    }
    def spanIndex[T](implicit sel: Selector[In, Vector[T]]) = SpanIndex(slab.content, slab.select[T](sel))
    def covered[T](span: Span, index: SpanIndex[T]): Vector[T] = {
      index(span)
    }
  }
}
