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

object StringIdentityAnalyzer extends AnalysisFunction[String, StringAnnotation, StringAnnotation, StringAnnotation] {
  def apply(slab: Slab[String, StringAnnotation, StringAnnotation]) = slab
}

/**
  * A simple regex sentence segmenter.
  */
trait SentenceSegmenter extends AnalysisFunction[String, StringAnnotation, StringAnnotation, Sentence] {
  def apply(slab: Slab[String, StringAnnotation, StringAnnotation]) =
    slab ++ "[^\\s.!?]+[^.!?]+[.!?]".r.findAllMatchIn(slab.content).map(m => Sentence(m.start, m.end))
}

/**
  * A simple regex tokenizer.
  */
trait Tokenizer extends AnalysisFunction[String, StringAnnotation, Sentence, Token] {
  def apply(slab: Slab[String, StringAnnotation, Sentence]) =
    slab ++ slab.iterator[Sentence].flatMap(sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(sentence.in(slab).content).map(m =>
        Token(sentence.begin + m.start, sentence.begin + m.end)))
}


object AnalysisPipeline {
  import StringAnnotation._

  def main (args: Array[String]) {
    val sentenceSegmenter = new SentenceSegmenter{}
    val tokenizer = new Tokenizer {}
    val pipeline = StringIdentityAnalyzer andThen sentenceSegmenter andThen tokenizer
    val slab = pipeline(Slab(AnalysisEngine.text1))
    // Notice that the last sentence (lacking EOS char) is missing.
    val sentences = slab.iterator[Sentence].toList
    println("\nSENTENCES\n\n" + sentences.map(_.in(slab).content).mkString("\n"))
    
    val tokens = slab.iterator[Token].toList
    println("\nTOKENS\n\n" + tokens.map(_.in(slab).content).mkString("\n"))

  }
  

}
