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
  def substring(content: String): String = {
    // This code should be in a typeclass. But I'm not sure if it will
    // work and it's not really possible to extend typeclasses in
    // Scala (that I know of). Exists for elements that carry their
    // own content around for e.g. normalization.
    content.substring(begin, end)
  }
}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

case class Source(url: URL) extends DocumentAnnotation
case class Sentence(span: Span) extends SpanAnnotation
case class Token(span: Span) extends SpanAnnotation {
  def offset(by: Int) = this.copy(span.offset(by))
}

case class Tagged[Tag](val span: Span, val tag: Tag) extends SpanAnnotation {
  def offset(by: Int) = this.copy(span.offset(by))
}
case class EntityMention(entityType: String, id: Option[String] = None)
case class PartOfSpeech(tag: String, id: Option[String] = None)

class ContentToken(override val span: Span, val content: String) extends Token(span) {
  override def substring(c: String): String = content
  override def offset(by: Int) = this.copy(span.offset(by))
}

object ContentToken {
  def apply(span: Span, content: String): ContentToken = new ContentToken(span, content)
}

case class Segment(span: Span, id: Option[String]) extends SpanAnnotation
