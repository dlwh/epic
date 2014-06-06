package epic.slab

import epic.preprocess.{RegexSentenceSegmenter, Tokenizer}

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
trait AnalysisFunction[C,B,I<:B,+O<:B] {
  def apply[In <: I](slab: Slab[C,B,In]):Slab[C,B,In with O]

  def andThen[II >: (I with O) <: B, OO <: B](other: AnalysisFunction[C, B, II, OO]):AnalysisFunction[C, B, I, O with OO] = {
    new ComposedAnalysisFunction[C, B, I, O, II, OO](this, other)
  }

}

case class ComposedAnalysisFunction[C, B, I <: B, O <: B, II >: (I with O) <: B, +OO <: B](a: AnalysisFunction[C,B,I,O],
                                                                                   b: AnalysisFunction[C,B,II,OO]) extends AnalysisFunction[C, B, I, O with OO] {

  def apply[In <: I](slab: Slab[C,B,In]):Slab[C,B,B with In with O with OO] = {
    // type inference can't figure this out...
    // can you blame it?
    b[In with O](a[In](slab))
  }

}


object StringIdentityAnalyzer extends StringAnalysisFunction[AnnotatedSpan, AnnotatedSpan] {
  def apply[In <: AnnotatedSpan](slab: StringSlab[In]):StringSlab[In] = slab
}


/**
  * A simple regex tokenizer.
  */
object RegexTokenizer extends Tokenizer {
  def apply[I <: Sentence](slab: StringSlab[I]) =
    // the [Token] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.++[Token](slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end, m.group(0)))))
}


object AnalysisPipeline {
  import AnnotatedSpan._

  // added only to demonstrate necesssity of [I] parameter on analyzers
  private[AnalysisPipeline] case class Document(begin: Int, end: Int) extends AnnotatedSpan
  private[AnalysisPipeline] def documentAdder(slab: StringSlab[AnnotatedSpan]) =
    slab ++ Iterator(Document(0, slab.content.length))

  def main (args: Array[String]) {
    def sentenceSegmenter = RegexSentenceSegmenter
    def tokenizer = RegexTokenizer
    val pipeline = sentenceSegmenter andThen tokenizer

    val inSlab = Slab("test\n.").+(Document(0, 5))
    val slab = pipeline(inSlab)

    // just to show what the type is
    val typedSpan: Slab[String, AnnotatedSpan, AnnotatedSpan with Document with Sentence with Token] = slab

    // added only to demonstrate necesssity of [I] parameter on analyzers
    val paragraphs = slab.iterator[Document].toList


    val sentences = slab.iterator[Sentence].toList
    println("\nSENTENCES\n\n" + sentences.map(_.in(slab).content).mkString("\n\n"))
    
    val tokens = slab.iterator[Token].toList
    println("\nTOKENS\n\n" + tokens.map(_.in(slab).content).mkString("\n\n"))

  }


}
