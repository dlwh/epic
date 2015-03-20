package epic.slab

import java.net.URL
import scalaz.std.vector._

trait Annotation extends Serializable {}

trait Located

//** Handles annotation over regions, any-dimensional

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

object SpanAnnotation {
  implicit def ordering[A <: SpanAnnotation] = new Ordering[A] {
    override def compare(a: A, b: A) = a.span.encoded.compare(b.span.encoded)
  }
}

trait Offsetter[T <: SpanAnnotation] {
  def apply(obj: T, by: Int): T
}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

case class Source(url: URL) extends DocumentAnnotation
case class Sentence(span: Span) extends SpanAnnotation
class Token(val span: Span) extends SpanAnnotation {
  override def equals(o: Any) = o match {
    case that: Token => that.span == span
    case _ => false
  }
  override def hashCode = span.hashCode
}

object Token {
  // The implicits help avoid Tokenized[Token], which looks ugly imo.
  // Also, the Token class won't be subclassed too often, so a bit of
  // copy/pasting in the name of a nicer looking API for beginners is
  // a worthy trade-off.
  implicit val tokenOffsetter = new Offsetter[Token] {
    def apply(obj: Token, by: Int): Token = Token(obj.span.offset(by))
  }

  def apply(span: Span): Token = new Token(span)
  def apply(begin: Int, end: Int): Token = new Token(Span(begin, end))
}

// I considered to use the same offsetter pattern with the Tagged
// class. I decided against it because usually tags are aligned with
// the Tokens and the offset isn't passed to the annotating library,
// so it can be constructed in the annotator. Also, there are a lot
// more different tags than tokens, which would lead to more
// boilerplate, which would lead back to the path of UIMA.
case class Tagged[Tag](val span: Span, val tag: Tag) extends SpanAnnotation {
  def offset(by: Int) = this.copy(span.offset(by))
}
case class EntityMention(entityType: String, id: Option[String] = None)
case class PartOfSpeech(tag: String, id: Option[String] = None)

class ContentToken(override val span: Span, val content: String) extends Token(span) {
  override def substring(c: String): String = content
  override def equals(o: Any) = o match {
    case that: ContentToken => that.span == span && that.content == content
    case _ => false
  }
  override def hashCode = span.hashCode ^ content.hashCode
}

object ContentToken {
  implicit val contentTokenOffsetter = new Offsetter[ContentToken] {
    def apply(obj: ContentToken, by: Int): ContentToken = ContentToken(obj.span.offset(by), obj.content)
  }
  def apply(span: Span, content: String): ContentToken = new ContentToken(span, content)
}
