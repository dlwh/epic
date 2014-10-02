package epic.slab

import java.net.URL

trait Annotation extends Serializable {}
trait Located

// Handles annotation over regions, any-dimensional

// No real implementation yet because I have no idea what would be a
// good generalization.
trait RegionAnnotation extends Annotation {}

trait SpanAnnotation extends RegionAnnotation {
  def begin: Int
  def end: Int
}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

case class Source(url: URL) extends DocumentAnnotation
case class Sentence(begin: Int, end: Int, id: Option[String] = None) extends SpanAnnotation
case class Segment(begin: Int, end: Int, id: Option[String] = None) extends SpanAnnotation
case class Token(begin: Int, end: Int) extends SpanAnnotation
case class PartOfSpeech(begin: Int, end: Int, tag: String, id: Option[String] = None) extends SpanAnnotation
case class EntityMention(begin: Int, end: Int, entityType: String, id: Option[String] = None) extends SpanAnnotation
