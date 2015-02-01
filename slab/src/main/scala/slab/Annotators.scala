package epic.slab.annotators
import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import scalaz.std.list._

/** Classes which implement common annotators. The initialize part is
  *  for implementing annotators which have a limited degree of
  *  referential transparency.
  */

object NoInitializer {
  val i = () => true
}

/** Splits the input document into sentences.
  */

class SentenceSegmenter[S <: Sentence](val segmenter: String => Iterable[S])
    extends legacyannotators.SentenceSegmenter[S, Boolean](NoInitializer.i, {case (i: Boolean, c: String) => segmenter(c)})

/** Abstract trait for tokenizers, which annotate sentence-segmented
  *  text with tokens. Tokens are usually words, but e.g. 42 is also a
  *  token. The trait offsets the returned tokens according to the
  *  Sentence. Sentences are not guaranteed to be in order.
  */

class Tokenizer[S <: Sentence, T <: Token: Offsetter](val tokenizer: String => Iterable[T])
    extends legacyannotators.Tokenizer[S, T, Boolean](NoInitializer.i, {case (i: Boolean, sentence: String) => tokenizer(sentence)})

object Tokenizer {
  def apply[S <: Sentence, T <: Token: Offsetter](tokenizer: (String => Iterable[T])): Tokenizer[S, T] = new Tokenizer[S, T](tokenizer)
}

object aliases {
  // Type alias to reduce the clutter in the Annotator signature.
  type input = List[Sentence] :: List[Token] :: HNil
}
import aliases._

/** Basic annotator. The function is passed the List of Tokens, one
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
  def tag[T <: Token, Tag](fun: Vector[String] => Iterable[Tag])(content: String, tokens: Vector[T]): List[Tagged[Tag]] =
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

class Segmenter[S <: Sentence, T <: Token, Tag](val segmenter: Vector[String] => Iterable[Tagged[Tag]])
    extends legacyannotators.Segmenter[S, T, Tag, Boolean](NoInitializer.i, {case (i: Boolean, tokens: Vector[String]) => segmenter(tokens)})

object Segmenter {
  def apply[S <: Sentence, T <: Token, Tag](seg: Vector[String] => Iterable[Tagged[Tag]]) = new Segmenter[S, T, Tag](seg)
}
