import epic.slab._
import org.scalatest._

class TreeTest extends FunSpec {
  val small = Tree("a", Span(0,3), Vector(Tree("b", Span(0,2))))
  val largeLeaf = Tree("a", Span(0,3), Vector(Tree("b", Span(0,4))))
  val sameSpan = Tree("a", Span(0,3), Vector(Tree("b", Span(0,3))))
  val smallDAG = Tree("a", Span(0,3), Vector(Tree("b", Span(0,3)), Tree("c", Span(0,3))))

  describe("map") {
    it("should map over the label") {
      assert(small.map(Map("a" -> 1, "b" -> 2)) == Tree(1, Span(0,3), Vector(Tree(2, Span(0,2)))))
    }
  }
  describe("offset") {
    it("should offset the simple tree") {
      assert(small.offset(3) == Tree("a", Span(3,6), Vector(Tree("b", Span(3,5)))))
    }
  }

  describe("isTree") {
    it("should pass a small tree") {
      assert(small.isTree)
    }
    it("should fail a tree with larger leaves") {
      assert(! largeLeaf.isTree)
    }
    it("should pass a tree with same spans") {
      assert(sameSpan.isTree)
    }
    it("should not allow a DAG") {
      assert(! smallDAG.isTree)
    }
  }

  describe("leaves") {
    it("should iterate with leaves") {
      small.leaves == Iterable(small, Vector(Tree("b", Span(0,2))))
    }
  }
}
