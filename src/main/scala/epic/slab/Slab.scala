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
 * A Slab is the core "document" type in Epic. It represents a
 * document and a set of annotations on that document, such as
 * sentence boundaries, tokens, named entity spans, etc. The
 * ContentType is the type of the document--typically a string--and
 * the AnnotationTypes parameter is an encoding of the kinds of
 * annotations that are present. [[epic.slab.AnalysisFunction]]s can
 * be used to add new annotations to a Slab that have the prerequisite
 * annotations.
 * @tparam ContentType
 * @tparam AnnotationTypes
 */
