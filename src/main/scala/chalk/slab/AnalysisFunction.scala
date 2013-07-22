package chalk.slab

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
trait AnalysisFunction[C,B,I<:B,O<:B] extends (Slab[C,B,I] => Slab[C,B,B with I with O])

trait StringAnalysisFunction[I<:Span,O<:Span] extends (Slab[String,Span,I] => Slab[String,Span,Span with I with O])

object StringIdentityAnalyzer extends StringAnalysisFunction[Span, Span] {
  def apply(slab: Slab[String, Span, Span]) = slab
}

/**
  * A simple regex sentence segmenter.
  */
trait SentenceSegmenter[I <: Span] extends StringAnalysisFunction[I, Sentence] {
  def apply(slab: Slab[String, Span, I]) =
    // the [Sentence] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.++[Sentence]("[^\\s.!?]+[^.!?]+[.!?]".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end)))
}

/**
  * A simple regex tokenizer.
  */
trait Tokenizer[I <: Sentence] extends StringAnalysisFunction[I, Token] {
  def apply(slab: Slab[String, Span, I]) =
    // the [Token] is required because of https://issues.scala-lang.org/browse/SI-7647
    slab.++[Token](slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end))))
}


object AnalysisPipeline {
  import Span._

  // added only to demonstrate necesssity of [I] parameter on analyzers
  private[AnalysisPipeline] case class Document(val begin: Int, val end: Int) extends Span
  private[AnalysisPipeline] def documentAdder(slab: Slab[String, Span, Span]) =
    slab ++ Iterator(Document(0, slab.content.length))

  def main (args: Array[String]) {
    def sentenceSegmenter[I <: Span] = new SentenceSegmenter[I]{}
    def tokenizer[I <: Sentence] = new Tokenizer[I]{}
    val pipeline = StringIdentityAnalyzer andThen documentAdder andThen sentenceSegmenter andThen tokenizer
    val slab = pipeline(Slab(AnalysisEngine.text1))

    // added only to demonstrate necesssity of [I] parameter on analyzers
    val paragraphs = slab.iterator[Document].toList

    // Notice that the last sentence (lacking EOS char) is missing.
    val sentences = slab.iterator[Sentence].toList
    println("\nSENTENCES\n\n" + sentences.map(_.in(slab).content).mkString("\n"))
    
    val tokens = slab.iterator[Token].toList
    println("\nTOKENS\n\n" + tokens.map(_.in(slab).content).mkString("\n"))

  }
  

}
