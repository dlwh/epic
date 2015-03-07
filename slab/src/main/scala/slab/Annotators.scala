package epic.slab.annotators
import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import scalaz.std.vector._

/** Classes which implement common annotators. The initialize part is
  *  for implementing annotators which have a limited degree of
  *  referential transparency.
  */

trait NoInitializer extends legacyannotators.Initialized[Boolean] {
  override val initialize = () => true
}

/** Splits the input document into sentences.
  */

trait SentenceSegmenter[S <: Sentence] extends (String => Iterable[Sentence]) with AnalysisFunction01[String, S] {
  def apply(sentence: String): Iterable[S]
  def strings(document: String): Iterable[String] = {
    val sentences = apply(document)
    sentences.map(s => document.substring(s.begin, s.end))
  }
}

/** Abstract trait for tokenizers, which annotate sentence-segmented
  *  text with tokens. Tokens are usually words, but e.g. 42 is also a
  *  token. The trait offsets the returned tokens according to the
  *  Sentence. Sentences are not guaranteed to be in order.
  */

abstract class Tokenizer[S <: Sentence, T <: Token: Offsetter] extends legacyannotators.Tokenizer[S, T, Boolean] with NoInitializer with (String => Iterable[T]) {
  def apply(sentence: String): Iterable[T]
  override def apply(initialized: Boolean, sentence: String): Iterable[T] = apply(sentence)
}

object Tokenizer {
  def apply[S <: Sentence, T <: Token: Offsetter](tokenizer: (String => Iterable[T])): Tokenizer[S, T] = new Tokenizer[S, T] {
    def apply(sentence: String): Iterable[T] = tokenizer(sentence)
  }
}

object aliases {
  // Type alias to reduce the clutter in the Annotator signature.
  type input = Vector[Sentence] :: Vector[Token] :: HNil
}
import aliases._

/** Basic annotator. The function is passed the Vector of Tokens, one
  * Sentence per time. The sentences are not guaranteed to be in order.
  */

class Annotator[S <: Sentence, T <: Token, Annotated](fun: ((String, Vector[T]) => Iterable[Annotated])) extends legacyannotators.Annotator[S, T, Annotated, Boolean](() => true, ({case (f, sentence, tokens) => fun(sentence, tokens)}))

object Annotator {
  def apply[S <: Sentence, T <: Token, Annotated](fun: ((String, Vector[T]) => Iterable[Annotated])) = new Annotator[S, T, Annotated](fun)
}

/** A Tagger assigns a sequence of Tags to a Sentence. Create a new
  * Tagger by creating a new class passing a tagger as function. The
  * Tagger expects the output from the library to be the same length
  * as the Token Vector passed as input and then copies the position
  * information from the Tokens.
  */

class Tagger[S <: Sentence, T <: Token, Tag](val tagger: (Vector[String] => Iterable[Tag])) extends Annotator[S, T, Tagged[Tag]](Tagger.tag(tagger))

object Tagger {
  def tag[T <: Token, Tag](fun: Vector[String] => Iterable[Tag])(content: String, tokens: Vector[T]): Vector[Tagged[Tag]] =
    legacyannotators.Tagger.tag[T, Tag, Boolean](({case (f, tokens) => fun(tokens)}))(true, content, tokens)
  def apply[S <: Sentence, T <: Token, Tag](fun: Vector[String] => Iterable[Tag]): Tagger[S, T, Tag] = new Tagger[S, T, Tag](fun)
}

/** A Segmenter splits up a sentence into labeled segments. For
  * instance, it might find all the people, places and things (Named
  * Entity Recognition) in a document. To create a new Segmenter,
  * either inherit from it and define `apply` or create pass the
  * segmenting function to the constructor. The sentences are not
  * guaranteed to be in order.
  */

trait Segmenter[S <: Sentence, T <: Token, Tag] extends legacyannotators.Segmenter[S, T, Tag, Boolean] with NoInitializer {
  override def apply(initialized: Boolean, sentence: Vector[String]): Iterable[Tagged[Tag]] = apply(sentence)
  def apply(sentence: Vector[String]): Iterable[Tagged[Tag]]
}

object Segmenter {
  def apply[S <: Sentence, T <: Token, Tag](seg: Vector[String] => Iterable[Tagged[Tag]]) = new Segmenter[S, T, Tag] {
    def apply(sentence: Vector[String]) = seg(sentence)
  }
}

trait TokenParser[S <: Sentence, T <: Token, Label] extends legacyannotators.TokenParser[S, T, Label, Boolean] with NoInitializer {
  override def apply(initialized: Boolean, body: String, tokens: Vector[T]): Tree[Label] = apply(body, tokens)
  def apply(body: String, tokens: Vector[T]): Tree[Label]
}

object TokenParser {
  def apply[S <: Sentence, T <: Token, Label](parser: (String, Vector[T]) => Tree[Label]) = new TokenParser[S, T, Label] {
    override def apply(body: String, tokens: Vector[T]) = parser(body, tokens)
  }
}

trait StringTokenParser[S <: Sentence, T <: Token, Label] extends legacyannotators.StringTokenParser[S, T, Label, Boolean] with NoInitializer {
  def apply(tokens: Vector[String]): Tree[Label]
  override def apply(initialized: Boolean, tokens: Vector[String]): Tree[Label] = apply(tokens)
}

object StringTokenParser {
  def apply[S <: Sentence, T <: Token, Label](parser: (Vector[String]) => Tree[Label]) = new StringTokenParser[S, T, Label] {
    override def apply(tokens: Vector[String]) = parser(tokens)
  }
}
