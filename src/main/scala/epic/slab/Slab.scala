package epic.slab

/**
 * A Slab is the core "document" type in Epic.
 */

import shapeless._
import shapeless.syntax.typeable._
import Utils._
import ops.hlist._
import epic.trees.Span

class Slab[Content, L <: HList](val content: Content, val annotations: L) {

  def get[T](implicit sel: Selector[L, Vector[T]]): Vector[T] = sel(annotations)
  def get[T](index: Int)(implicit sel: Selector[L, Vector[T]]): T = sel(annotations)(index)
  def getMany[T <: HList](implicit sel: SelectMany.Aux[L, T, T]): T = sel(annotations)

  def add[A, Tmp <: HList, Result <: HList](newAnnotations: Vector[A])(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, newAnnotations))
  }
  def add[A, Tmp <: HList, Result <: HList](newAnnotation: A)(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, Vector(newAnnotation)))
  }
}

object Slab {
  def apply[C](content: C): Slab[C, HNil] = new Slab[C, HNil](content, HNil)
  def apply[C, L <: HList](content: C, annotations: L): Slab[C, L] = new Slab[C, L](content, annotations)
}

class StringSlab[L <: HList](val content: String, val annotations: L) extends Slab[String, L](content, annotations) {
  def at(span: Span): String = content.substring(span.begin, span.end)
}

object StringSlab {
  def apply[L <: HList](s: Slab[String, L]): StringSlab[L] = new StringSlab(s.content, s.annotations)
}

object Implicits {
  implicit def stringSlab[L <: HList](s: Slab[String, L]) = StringSlab[L](s)
}
