import epic.slab._
import org.scalatest._

class SpanIndexTest extends FunSpec {
  val data = List(Sentence(Span(0,1)), Sentence(Span(2,3)), Sentence(Span(0,3)), Sentence(Span(1,1)), Sentence(Span(0,2)))
  val index = SpanIndex(data)
  describe("the sorting") {
    it("should sort based on span start") {
      val result = index(Span(0,3)).toList
      assert(result(3) == Sentence(Span(1, 1)))
      assert(result(4) == Sentence(Span(2, 3)))
    }
  }
  describe("the spanning") {
    it("should include the edges") {
      assert(index(Span(0, 1)) == List(0, 3).map(data))
      assert(index(Span(2, 3)) == List(1).map(data))
      assert(index(Span(1, 3)) == List(3, 1).map(data))
      assert(index(Span(1, 1)) == List(3).map(data))
    }
  }
}
