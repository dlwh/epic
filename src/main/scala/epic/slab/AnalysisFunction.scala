package epic.slab

import epic.preprocess.{RegexSentenceSegmenter, Tokenizer}
import epic.trees.Span

/**
 * An analysis function that takes a Slab with declared annotation types in it and outputs
 * a new Slab with additional annotations of a new type.
 *
 * Documentation for the type variables:
 *   C = Content type
 *   B = Base annonation type
 *   I = Input annotation type
 *   O = Output annotation type
 */
trait AnalysisFunction[C,B,I,+O] {
  def apply[In <: I](slab: Slab[C,B,In]):Slab[C,B,In with O]

  def andThen[II >: (I with O), OO](other: AnalysisFunction[C, B, II, OO]):AnalysisFunction[C, B, I, O with OO] = {
    new ComposedAnalysisFunction[C, B, I, O, II, OO](this, other)
  }

}

case class ComposedAnalysisFunction[C, B, I, O, II >: (I with O), +OO](a: AnalysisFunction[C,B,I,O],
                                                                       b: AnalysisFunction[C,B,II,OO]) extends AnalysisFunction[C, B, I, O with OO] {

  def apply[In <: I](slab: Slab[C,B,In]):Slab[C,B,In with O with OO] = {
    // type inference can't figure this out...
    // can you blame it?
    b[In with O](a[In](slab))
  }

}

object StringIdentityAnalyzer extends StringAnalysisFunction[Any, Any] {
  def apply[In](slab: StringSlab[In]):StringSlab[In] = slab
}

/**
  * A simple regex tokenizer.
  */
object RegexTokenizer extends Tokenizer {
  def apply[I <: Sentence](slab: StringSlab[I]) =
    // the [Token] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.addLayer[Token](slab.iterator[Sentence].flatMap{ case (region, sentence) =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(slab.content.substring(region.begin, region.end)).map(m =>
        Span(region.begin + m.start, region.begin + m.end) -> Token(m.group(0)))
    })
}

object AnalysisPipeline {
  import AnnotatedSpan._

  // added only to demonstrate necesssity of [I] parameter on analyzers
  private[AnalysisPipeline] case class Document()
  private[AnalysisPipeline] def documentAdder(slab: StringSlab[Any]) =
    slab.addLayer(Iterator(Span(0, slab.content.length) -> Document()))

  def main (args: Array[String]) {
    def sentenceSegmenter:StringAnalysisFunction[Any, Sentence] = RegexSentenceSegmenter
    def tokenizer = RegexTokenizer
    val pipeline = sentenceSegmenter andThen tokenizer

    val inSlab = Slab("test\n.").+(Span(0, 5) -> Document())
    val slab = pipeline(inSlab)

    // just to show what the type is
    val typedSpan: Slab[String, Span, Document with Sentence with Token] = slab

    // added only to demonstrate necesssity of [I] parameter on analyzers
    val paragraphs = slab.iterator[Document].toList


    val sentences = slab.iterator[Sentence].toList
    println("\nSENTENCES\n\n" + sentences.map(r => slab.spanned(r._1)).mkString("\n\n"))
    
    val tokens = slab.iterator[Token].toList
    println("\nTOKENS\n\n" + tokens.map(r => slab.spanned(r._1)).mkString("\n\n"))

  }

}
