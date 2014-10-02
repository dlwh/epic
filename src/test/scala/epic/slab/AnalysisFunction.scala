import org.scalatest._
import epic.slab._
import shapeless._
import ops.hlist._
import Utils._

import org.scalatest.FunSpec

/**
  * A simple regex tokenizer.
  */
object RegexTokenizer extends AnalysisFunction[String] {
  def apply[In <: HList, Out <: HList](slab: StringSlab[In])(implicit sel: Selector[In, Vector[Sentence]], adder: Adder.Aux[In, Token, Vector[Token], Out]): Slab[String, Out] =
    slab.add(slab.get[Sentence](sel).flatMap { sentence =>
      "\\p{L}+|\\p{P}+|\\p{N}+".r.findAllMatchIn(
        slab.content.substring(sentence.begin, sentence.end)
      ).map(
        m => Token(sentence.begin + m.start, sentence.begin + m.end)
      ).toVector
    })(adder)
}

class SimpleTokenizerTest extends FunSpec {
  describe("a simple tokenizer") {
    val tokenizer = RegexTokenizer
    val string = "A simple sentence to be analyzed."
    val slab = Slab(string, Vector(Sentence(0, string.length - 1)) :: HNil)
    it("should annotate tokens") {
      assert(tokenizer(slab).get[Token] == Vector(Token(0,1), Token(2,8), Token(9,17), Token(18,20), Token(21,23), Token(24,32)))
    }
  }
}
