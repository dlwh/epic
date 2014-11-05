package epic.slab
import shapeless._
import ops.hlist._
import epic.trees.Span
import scala.collection.SortedMap

// TODO: Get one which supports multiple types via HLists and CoProducts
class SpanIndex[T <: SpanAnnotation](data: Vector[T]) {
  // Indexes by `begin`.
  lazy val indexed = data.foldLeft(SortedMap[Int, Seq[T]]())({case (map, e) =>
    map + ((e.begin, (map.get(e.begin).map(_ :+ e).getOrElse((Seq[T](e))))))
  })

  def apply(span: Span): Iterable[T] = {
    indexed.range(span.begin, span.end + 1).toIterable.flatMap({ case (_, seq) =>
      seq.filter(_.end <= span.end)
    })
  }
}

object SpanIndex {
  def apply[T <: SpanAnnotation](data: Vector[T]) = new SpanIndex(data)
}


object Indexes {
  implicit final class SpanIndexSlabOps[In <: HList](slab: Slab[String, In]) {
    def covered[T <: SpanAnnotation](span: Span)(implicit sel: Selector[In, Vector[T]]): Iterable[T] = {
      spanIndex(sel).apply(span)
    }
    def spanIndex[T <: SpanAnnotation](implicit sel: Selector[In, Vector[T]]) = SpanIndex(slab.select[T](sel))
    def covered[T <: SpanAnnotation](span: Span, index: SpanIndex[T]): Iterable[T] = {
      index(span)
    }
  }
}
