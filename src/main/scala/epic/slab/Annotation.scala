package epic.slab

import java.net.URL
import epic.trees.Span

trait Annotation extends Serializable {}
trait Located

// Handles annotation over regions, any-dimensional

// No real implementation yet because I have no idea what would be a
// good generalization.
trait RegionAnnotation extends Annotation {}

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
case class Token(span: Span) extends SpanAnnotation {
  def offset(by: Int) = this.copy(span.offset(by))
}

case class Tagged[Tag](val span: Span, val tag: Tag) extends SpanAnnotation {
  def offset(by: Int) = this.copy(span.offset(by))
}
case class EntityMention(entityType: String, id: Option[String] = None)
case class PartOfSpeech(tag: String, id: Option[String] = None)
