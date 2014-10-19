package epic.slab

import java.net.URL

trait Annotation extends Serializable {}
trait Located

// Handles annotation over regions, any-dimensional

// No real implementation yet because I have no idea what would be a
// good generalization.
trait RegionAnnotation extends Annotation {}

case class Span(begin: Int, end: Int)

trait SpanAnnotation extends RegionAnnotation {
  def span: Span
  def begin: Int = span.begin
  def end: Int = span.end
}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

case class Source(url: URL) extends DocumentAnnotation
case class Sentence(span: Span, id: Option[String] = None) extends SpanAnnotation
case class Segment(span: Span, id: Option[String] = None) extends SpanAnnotation
case class Token(span: Span) extends SpanAnnotation
case class Tagged[Tag](span: Span, tag: Tag, id: Option[String] = None) extends SpanAnnotation
case class EntityMention(span: Span, entityType: String, id: Option[String] = None) extends SpanAnnotation
