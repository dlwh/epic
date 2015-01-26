package epic.slab
import org.scalatest._
import shapeless._
import ops.hlist._
import scalaz.std.list._
import epic.slab.typeclasses._

import org.scalatest.FunSpec

/**
  * A simple regex tokenizer.
  */
object RegexTokenizer11 extends AnalysisFunction11[String, Sentence, Token] {
  def apply(content: String, sentences: List[Sentence]): List[Token] = {
    sentences.flatMap { sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(
        content.substring(sentence.begin, sentence.end)
      ).map(
        m => Token(Span(sentence.begin + m.start, sentence.begin + m.end))
      ).toList
    }
  }
}

// Same again, except using a different interface. The API sucks,
// needs to be improved.
object RegexTokenizerN1 extends AnalysisFunctionN1[String, List[Sentence] :: HNil, Token] {
  def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel: SelectMany.Aux[In, List[Sentence] :: HNil, List[Sentence] :: HNil], adder: Adder.Aux[In, List[Token], Out]): Slab[String, Out] =
    slab.add(slab.selectMany[List[Sentence] :: HNil](sel).at(0).flatMap { sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(
        slab.content.substring(sentence.begin, sentence.end)
      ).map(
        m => Token(Span(sentence.begin + m.start, sentence.begin + m.end))
      ).toList
    })(adder)
}


class SimpleTokenizerTest extends FunSpec {
  describe("a 1 to 1 tokenizer") {
    val tokenizer = RegexTokenizer11
    val string = "A simple sentence to be analyzed."
    val slab = Slab(string, List(Sentence(Span(0, string.length - 1))) :: HNil)
    it("should annotate tokens") {
      assert(tokenizer(slab).select[Token] == List(Token(Span(0,1)), Token(Span(2,8)), Token(Span(9,17)), Token(Span(18,20)), Token(Span(21,23)), Token(Span(24,32))))
    }
  }
  describe("a N to 1 tokenizer") {
    val tokenizer = RegexTokenizerN1
    val string = "A simple sentence to be analyzed."
    val slab = Slab(string, List(Sentence(Span(0, string.length - 1))) :: HNil)
    it("should annotate tokens") {
      assert(tokenizer(slab).select[Token] == List(Token(Span(0,1)), Token(Span(2,8)), Token(Span(9,17)), Token(Span(18,20)), Token(Span(21,23)), Token(Span(24,32))))
    }
  }
}
