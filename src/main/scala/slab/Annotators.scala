package epic.slab.annotators
import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import scalaz.std.list._

/** Classes which implement common annotators. The initialize part is
  *  for implementing annotators which have a limited degree of
  *  referential transparency.
  */

/** Splits the input document into sentences.
  */

trait NoInitializer extends legacyannotators.Initialized[Boolean] {
  override val initialize = () => true
}

trait SentenceSegmenter[S <: Sentence] extends legacyannotators.SentenceSegmenter[S, Boolean] with NoInitializer {
  override def apply(initialized: Boolean, sentence: String) = apply(sentence)
  def apply(sentence: String): Iterable[S]
}

/** Abstract trait for tokenizers, which annotate sentence-segmented
  *  text with tokens. Tokens are usually words, but e.g. 42 is also a
  *  token. The trait offsets the returned tokens according to the
  *  Sentence. Sentences are not guaranteed to be in order.
  */

abstract class Tokenizer[T <: Token: Offsetter] extends legacyannotators.Tokenizer[T, Boolean] with NoInitializer {
  def apply(sentence: String): Iterable[T]
  override def apply(initialized: Boolean, sentence: String): Iterable[T] = apply(sentence)
}

object Tokenizer {
  def apply[T <: Token: Offsetter](tokenizer: (String => Iterable[T])): Tokenizer[T] = new Tokenizer[T] {
    def apply(sentence: String): Iterable[T] = tokenizer(sentence)
  }
}

object aliases {
  // Type alias to reduce the clutter in the Annotator signature.
  type input = List[Sentence] :: List[Token] :: HNil
}
import aliases._

/** Basic annotator. The function is passed the List of Tokens, one
  * Sentence per time. The sentences are not guaranteed to be in order.
  */

class Annotator[T](fun: ((String, Vector[Token]) => Iterable[T])) extends legacyannotators.Annotator[T, Boolean](() => true, ({case (f, sentence, tokens) => fun(sentence, tokens)}))

object Annotator {
  def apply[T](fun: ((String, Vector[Token]) => Iterable[T])) = new Annotator(fun)
}

/** A Tagger assigns a sequence of Tags to a Sentence. Create a new
  * Tagger by creating a new class passing a tagger as function. The
  * Tagger expects the output from the library to be the same length
  * as the Token Vector passed as input and then copies the position
  * information from the Tokens.
  */

class Tagger[Tag](val tagger: (Vector[String] => Iterable[Tag])) extends Annotator[Tagged[Tag]](Tagger.tag(tagger))

object Tagger {
  def tag[Tag](fun: Vector[String] => Iterable[Tag])(content: String, tokens: Vector[Token]): List[Tagged[Tag]] =
    legacyannotators.Tagger.tag[Tag, Boolean](({case (f, tokens) => fun(tokens)}))(true, content, tokens)
  def apply[Tag](fun: Vector[String] => Iterable[Tag]): Tagger[Tag] = new Tagger[Tag](fun)
}

/** A Segmenter splits up a sentence into labeled segments. For
  * instance, it might find all the people, places and things (Named
  * Entity Recognition) in a document. To create a new Segmenter,
  * either inherit from it and define `apply` or create pass the
  * segmenting function to the constructor. The sentences are not
  * guaranteed to be in order.
  */

trait Segmenter[Tag] extends legacyannotators.Segmenter[Tag, Boolean] with NoInitializer {
  override def apply(initialized: Boolean, sentence: List[String]): Iterable[Tagged[Tag]] = apply(sentence)
  def apply(sentence: List[String]): Iterable[Tagged[Tag]]
}

object Segmenter {
  def apply[Tag](seg: List[String] => List[Tagged[Tag]]) = new Segmenter[Tag] { def apply(sentence: List[String]) = seg(sentence) }
}
