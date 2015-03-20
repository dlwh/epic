package epic.slab.legacyannotators
import epic.slab._
import epic.slab.typeclasses._
import shapeless._
import scalaz.std.vector._

trait Initialized[T] {
  val initialize: (() => T)
}

/** Classes which implement common annotators. The initialize part is
  *  for implementing annotators which have a limited degree of
  *  referential transparency.
  */

/** Splits the input document into sentences.
  */

trait SentenceSegmenter[S <: Sentence, I] extends (String => Iterable[Sentence]) with AnalysisFunction01[String, S] with Initialized[I] {
  override def apply(sentence: String) = apply(initialize(), sentence)
  def apply(initialized: I, sentence: String): Iterable[S]
  def strings(document: String): Iterable[String] = {
    val sentences = apply(document)
    sentences.map(_.substring(document))
  }
}

/** Abstract trait for tokenizers, which annotate sentence-segmented
  *  text with tokens. Tokens are usually words, but e.g. 42 is also a
  *  token. The trait offsets the returned tokens according to the
  *  Sentence. Sentences are not guaranteed to be in order.
  */

abstract class Tokenizer[S <: Sentence, T <: Token: Offsetter, I] extends AnalysisFunction11[String, S, T] with Initialized[I] {
  override def apply(content: String, sentences: Vector[S]): Iterable[T] = {
    val initialized = initialize()
    sentences.map({ sentence => 
      apply(initialized, sentence.substring(content))
        .map(token => implicitly[Offsetter[T]].apply(token, sentence.span.begin))
    }).flatten
  }

  def apply(initialized: I, sentence: String): Iterable[T]
}

object Tokenizer {
  def apply[S <: Sentence, T <: Token: Offsetter, I](initializer: (() => I), tokenizer: ((I, String) => Iterable[T])): Tokenizer[S, T, I] = new Tokenizer[S, T, I] {
    val initialize = initializer
    def apply(initialized: I, sentence: String): Iterable[T] = tokenizer(initialized, sentence)
  }
}

/** Basic annotator. The function is passed the Vector of Tokens, one
  * Sentence per time. The sentences are not guaranteed to be in order.
  */

class Annotator[S <: Sentence, T <: Token, Annotated, I](override val initialize: (() => I), val fun: ((I, String, Vector[T]) => Iterable[Annotated])) extends AnalysisFunctionN1[String, Vector[S] :: Vector[T] :: HNil, Annotated] with Initialized[I] {
  override def apply(content: String, in: Vector[S] :: Vector[T] :: HNil) = {
    val initialized = initialize()
    val index = SpanIndex(in.select[Vector[T]])
    val annotatedSentences = for(sentence <- in.select[Vector[S]]) yield {
      fun(initialized, content, index(sentence.span).toVector)
    }
    annotatedSentences.flatten :: HNil
  }
}

object Annotator {
  def apply[S <: Sentence, T <: Token, A, I](initialize: (() => I), fun: ((I, String, Vector[Token]) => Iterable[A])) =
    new Annotator[S, T, A, I](initialize, fun)
}

/** A Tagger assigns a sequence of Tags to a Sentence. Create a new
  * Tagger by creating a new class passing a tagger as function. The
  * Tagger expects the output from the library to be the same length
  * as the Token Vector passed as input and then copies the position
  * information from the Tokens.
  */

class Tagger[S <: Sentence, T <: Token, Tag, I](override val initialize: (() => I), val tagger: ((I, Vector[String]) => Iterable[Tag])) extends Annotator[S, T, Tagged[Tag], I](initialize, Tagger.tag[T, Tag, I](tagger))

object Tagger {
  // Merges tag information back into the Vector[Tagged[Tag]] format.
  def tag[T <: Token, Tag, I](fun: (I, Vector[String]) => Iterable[Tag])(initialized: I, content: String, tokens: Vector[T]): Vector[Tagged[Tag]] = {
    val strings = tokens.map(_.substring(content))
    val tagSeq = fun(initialized, strings)
    tokens.zip(tagSeq).map({case (token, tag) => Tagged[Tag](token.span, tag)}).toVector
  }
  def apply[S <: Sentence, T <: Token, Tag, I](initialize: (() => I), fun: (I, Vector[String]) => Iterable[Tag]): Tagger[S, T, Tag, I] =
    new Tagger(initialize, fun)
}

/** A Segmenter splits up a sentence into labeled segments. For
  * instance, it might find all the people, places and things (Named
  * Entity Recognition) in a document. To create a new Segmenter,
  * either inherit from it and define `apply` or create pass the
  * segmenting function to the constructor. The sentences are not
  * guaranteed to be in order.
  */

trait Segmenter[S <: Sentence, T <: Token, Tag, I] extends AnalysisFunctionN1[String, Vector[S] :: Vector[T] :: HNil, Tagged[Tag]] with Initialized[I] {
  override def apply(content: String, in: Vector[S] :: Vector[T] :: HNil): Vector[Tagged[Tag]] :: HNil = {
    val initialized = initialize()
    val index = SpanIndex(in.select[Vector[T]])
    val annotatedSentences = for(sent <- in.select[Vector[S]]) yield {
      val strings = index(sent.span).map(_.substring(content)).toVector
      apply(initialized, strings).map(_.offset(sent.begin)).toVector
    }

    annotatedSentences.flatten :: HNil
  }

  def apply(initialized: I, sentence: Vector[String]): Iterable[Tagged[Tag]]
}

object Segmenter {
  def apply[S <: Sentence, T <: Token, Tag, I](initializer: (() => I), seg: (I, Vector[String]) => Iterable[Tagged[Tag]]) = new Segmenter[S, T, Tag, I] {
    override val initialize = initializer
    override def apply(initialized: I, sentence: Vector[String]): Iterable[Tagged[Tag]] = seg(initialized, sentence)
  }
}

trait TokenParser[S <: Sentence, T <: Token, Label, I] extends AnalysisFunctionN1[String, Vector[S] :: Vector[T] :: HNil, Tree[Label]] with Initialized[I] {
  override def apply(content: String, in: Vector[S] :: Vector[T] :: HNil): Vector[Tree[Label]] :: HNil = {
    // Duplicated from segmenter - refactor?
    val initialized = initialize()
    val index = SpanIndex(in.select[Vector[T]])
    var annotatedSentences = for(sent <- in.select[Vector[S]]) yield {
      val tokens = index(sent.span).toVector
      apply(initialized, content, tokens)
    }
    annotatedSentences :: HNil
  }
  def apply(initialized: I, body: String, tokens: Vector[T]): Tree[Label]
}

trait StringTokenParser[S <: Sentence, T <: Token, Label, I] extends TokenParser[S, T, Label, I] {
  def apply(initialized: I, tokens: Vector[String]): Tree[Label]
  override def apply(initialized: I, body: String, tokens: Vector[T]): Tree[Label] =
    apply(initialized, tokens.map(_.substring(body))).offset(tokens.headOption.map(_.begin).getOrElse(0))
}
