package epic.slab
import shapeless._
import scalaz._
import std.list._

// Classes which implement common annotators. The initialize part is
// for implementing annotators which have a limited degree of
// referential transparency.

// Splits the input document into sentences.

trait SentenceSegmenter extends (String => List[Sentence]) with AnalysisFunction01[String, Sentence] {
  def apply(sentence: String): List[Sentence]
  def strings(document: String): List[String] = {
    val sentences = apply(document)
    sentences.map(s => document.substring(s.begin, s.end))
  }
}

/**
 * Abstract trait for tokenizers, which annotate sentence-segmented
 * text with tokens. Tokens are usually words, but e.g. 42 is also a
 * token. The trait offsets the returned tokens according to the
 * Sentence.
 */

trait Tokenizer extends AnalysisFunction11[String, Sentence, Token] {
  override def apply(content: String, sentences: List[epic.slab.Sentence]): List[epic.slab.Token] = {
    sentences.map({ sentence => 
      apply(content.substring(sentence.span.begin, sentence.span.end))
        .map(_.offset(sentence.span.begin))
    }).flatten
  }

  def apply(sentence: String): List[Token]
}

object Tokenizer {
  def apply(tokenizer: (String => List[Token])): Tokenizer = new Tokenizer {
    def apply(sentence: String): List[Token] = tokenizer(sentence)
  }
}


// A Tagger assigns a sequence of Tags to a Sentence.

object aliases {
  // Type alias to reduce the clutter in the Annotator signature.
  type input = List[Sentence] :: List[Token] :: HNil
}
import aliases._

// Basic annotator. The function is passed the List of Tokens, one Sentence per time.
class Annotator[T](val fun: ((String, List[Token]) => List[T])) extends AnalysisFunctionN1[String, input, T] {
  override def apply[In <: HList, Out <: HList]
    (slab: Slab[String, In])
    (implicit sel: SubSelectMany.Aux[In, input, input],
      adder: Adder.Aux[In, List[T], Out]
    ): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[List[Token]])
    val annotatedSentences = for(sentence <- data.select[List[Sentence]]) yield {
      fun(slab.content, index(sentence.span).toList)
    }
    slab.add(annotatedSentences.flatten)(adder)
  }
}

object Annotator {
  def apply[T](fun: ((String, List[Token]) => List[T])) = new Annotator(fun)
}

// Create a new tagger by creating a new class passing a tagger as
// function.
class Tagger[Tag](val tagger: (List[String] => List[Tag])) extends Annotator[Tagged[Tag]](Tagger.tag(tagger))

object Tagger {
  // Merges tag information back into the List[Tagged[Tag]] format.
  def tag[Tag](fun: List[String] => List[Tag])(content: String, tokens: List[Token]): List[Tagged[Tag]] = {
    val strings = tokens.map(t => content.substring(t.span.begin, t.span.end))
    val tagSeq = fun(strings)
    tokens.zip(tagSeq).map({case (token, tag) => Tagged[Tag](token.span, tag)})
  }
  def apply[Tag](fun: List[String] => List[Tag]): Tagger[Tag] = new Tagger[Tag](fun)
}

/**
 * A [[epic.slab.Segmenter]] splits up a sentence into labeled
 * segments. For instance, it might find all the people, places and
 * things (Named Entity Recognition) in a document. To create a new
 * Segmenter, either inherit from it and define `apply` or create pass
 * the segmenting function to the constructor.
 **/

trait Segmenter[Tag] extends AnalysisFunctionN1[String, input, Tagged[Tag]] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel: SubSelectMany.Aux[In, input, input], adder: Adder.Aux[In, List[Tagged[Tag]], Out]): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(data.select[List[Token]])
    val annotatedSentences = for(sent <- data.select[List[Sentence]]) yield {
      val strings = index(sent.span).map(t => slab.substring(t)).toList
      apply(strings).map(_.offset(sent.begin))
    }

    slab.add(annotatedSentences.flatten)(adder)
  }

  def apply(sentence: List[String]): List[Tagged[Tag]]
}

object Segmenter {
  def apply[Tag](seg: List[String] => List[Tagged[Tag]]) = new Segmenter[Tag] { def apply(sentence: List[String]) = seg(sentence) }
}
