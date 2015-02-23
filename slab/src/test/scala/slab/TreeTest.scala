import epic.slab._
import org.scalatest._

class TreeTest extends FunSpec {
  val small = Tree("a", Span(0,3), Vector(Tree("b", Span(0,2))))
  describe("map") {
    it("should map over the label") {
      assert(small.map(Map("a" -> 1, "b" -> 2)) == Tree(1, Span(0,3), Vector(Tree(2, Span(0,2)))))
    }
  }
}
