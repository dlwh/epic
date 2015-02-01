package epic.slab.legacyannotators
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

class SentenceSegmenter[S <: Sentence, I](val initialize: () => I, val seg: (I, String) => Iterable[S])
    extends AnalysisFunction01[String, S](SentenceSegmenter.apply(initialize)(seg))
    with (String => Iterable[String]) {
  def apply(document: String): Iterable[String] = {
    val init = initialize()
    val sentences = seg(init, document)
    sentences.map(s => document.substring(s.begin, s.end))
  }
}

object SentenceSegmenter {
  def apply[S <: Sentence, I](initialize: () => I)(fun: (I, String) => Iterable[S])(content: String): Iterable[S] = {
    val init = initialize()
    fun(init, content)
  }
}

/** Abstract trait for tokenizers, which annotate sentence-segmented
  *  text with tokens. Tokens are usually words, but e.g. 42 is also a
  *  token. The trait offsets the returned tokens according to the
  *  Sentence. Sentences are not guaranteed to be in order.
  */

class Tokenizer[S <: Sentence, T <: Token: Offsetter, I](val initialize: () => I, val seg: (I, String) => Iterable[T])
    extends AnalysisFunction11[String, S, T](Tokenizer.apply[S, T, I](initialize)(seg)(implicitly[Offsetter[T]])) {
}

object Tokenizer {
  def apply[S <: Sentence, T <: Token, I](initialize: () => I)(fun: (I, String) => Iterable[T])(offsetter: Offsetter[T])(content: String, sentences: List[S]): Iterable[T] = {
    val initialized = initialize()
    sentences.map({ sentence => 
      fun(initialized, content.substring(sentence.span.begin, sentence.span.end))
        .map(token => offsetter(token, sentence.span.begin))
    }).flatten
  }

  def apply[S <: Sentence, T <: Token: Offsetter, I](initializer: (() => I), tokenizer: ((I, String) => Iterable[T])): Tokenizer[S, T, I] = new Tokenizer[S, T, I](initializer, tokenizer)
}

/** Basic annotator. The function is passed the List of Tokens, one
  * Sentence per time. The sentences are not guaranteed to be in order.
  */

class Annotator[S <: Sentence, T <: Token, Annotated, I](val initialize: (() => I), val annot: ((I, String, Vector[T]) => Iterable[Annotated]))
    extends AnalysisFunctionN1[String, List[S] :: List[T] :: HNil, Annotated] {
  override def apply[In <: HList, Out <: HList]
    (slab: Slab[String, In])
    (implicit sel: SelectMany.Aux[In, List[S] :: List[T] :: HNil, List[S] :: List[T] :: HNil],
      adder: Adder.Aux[In, List[Annotated], Out]
    ): Slab[String, Out] = {
    val initialized = initialize()
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[List[T]])
    val annotatedSentences = for(sentence <- data.select[List[S]]) yield {
      annot(initialized, slab.content, index(sentence.span).toVector)
    }
    slab.add(annotatedSentences.flatten)(adder)
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
  // Merges tag information back into the List[Tagged[Tag]] format.
  def tag[T <: Token, Tag, I](fun: (I, Vector[String]) => Iterable[Tag])(initialized: I, content: String, tokens: Vector[T]): List[Tagged[Tag]] = {
    val strings = tokens.map(t => content.substring(t.span.begin, t.span.end))
    val tagSeq = fun(initialized, strings)
    tokens.zip(tagSeq).map({case (token, tag) => Tagged[Tag](token.span, tag)}).toList
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

class Segmenter[S <: Sentence, T <: Token, Tag, I](val initialize: () => I, val fun: (I, Vector[String]) => Iterable[Tagged[Tag]])
    extends AnalysisFunctionN1[String, List[S] :: List[T] :: HNil, Tagged[Tag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel:
      SelectMany.Aux[In, List[S] :: List[T] :: HNil, List[S] :: List[T] :: HNil], adder: Adder.Aux[In, List[Tagged[Tag]], Out]): Slab[String, Out] = {
    val initialized = initialize()
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[List[T]])
    val annotatedSentences = for(sent <- data.select[List[S]]) yield {
      val strings = index(sent.span).map(t => slab.substring(t)).toVector
      fun(initialized, strings).map(_.offset(sent.begin)).toList
    }

    slab.add(annotatedSentences.flatten)(adder)
  }
}

object Segmenter {
  def apply[S <: Sentence, T <: Token, Tag, I](initialize: (() => I), seg: (I, Vector[String]) => Iterable[Tagged[Tag]]) = new Segmenter[S, T, Tag, I](initialize, seg)
}
