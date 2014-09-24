package epic.slab

import java.net.URL

// ===========
// Annotations
// ===========

trait Annotation extends Serializable {}
trait Located

// Handles annotation over regions, any-dimensional
trait SpanAnnotation extends Annotation {}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

case class Source(url: URL) extends DocumentAnnotation
case class Sentence(id: Option[String] = None) extends SpanAnnotation
case class Segment(id: Option[String] = None) extends SpanAnnotation
case class Token(token: String) extends SpanAnnotation
case class PartOfSpeech(tag: String, id: Option[String] = None) extends SpanAnnotation
case class EntityMention(entityType: String, id: Option[String] = None) extends SpanAnnotation


/**
 * A Slab is the core "document" type in Epic.
 */

import shapeless._
import shapeless.syntax.typeable._
import Utils._
import ops.hlist._

class Slab[Content, L <: HList](val content: Content, val annotations: L) {

  def get[T](index: Int)(implicit sel: Selector[L, Vector[T]]): T = sel(annotations)(index)
  def get[T](implicit sel: Selector[L, Vector[T]]): Vector[T] = sel(annotations)

  def add[A, Tmp <: HList, Result <: HList](newAnnotations: Vector[A])(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, newAnnotations))
  }
  def add[A, Tmp <: HList, Result <: HList](newAnnotation: A)(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, Vector(newAnnotation)))
  }

  def toHList: L = this.annotations
}

object Slab {
  def apply[C](content: C): Slab[C, HNil] = new Slab[C, HNil](content, HNil)
}
