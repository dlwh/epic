package epic.slab
import shapeless._
import ops.hlist._
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

  def all(): Iterable[T] = {
    indexed.values.flatten
  }
}

object SpanIndex {
  def apply[T <: SpanAnnotation](data: Vector[T]) = new SpanIndex(data)
}
