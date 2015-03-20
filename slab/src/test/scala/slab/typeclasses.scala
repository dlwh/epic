import epic.slab._
import epic.slab.typeclasses._
import org.scalatest._
import shapeless._
import shapeless.ops.hlist._
import scalaz._
import std.vector._
import std.list._

class Sentence(val loc: Int)
case class PSentence(override val loc: Int) extends Sentence(loc)
case class ISentence(override val loc: Int) extends Sentence(loc)
case class Token(val loc: Int)

class TypeclassesSpec extends FunSpec {
  import HOps.Ops
  val token = Vector(Token(0))
  val isent = Vector(ISentence(0))
  val psent = Vector(PSentence(0))
  val annotations = isent :: psent :: token :: HNil
  val hnil: HNil = HList()

  describe("SubSelect") {
    it("should select the first one") {
      assert(annotations.subselect[Vector[Sentence]] == isent)
    }
    it("should select only the specified subclass") {
      assert(annotations.subselect[Vector[PSentence]] == psent)
    }
  }

  describe("selectMany") {
    it("should get a single element") {
      val l = Vector(1) :: Vector("foo") :: HNil
      assert(l.selectMany[Vector[String] :: HNil].at(0) == Vector("foo"))
    }
    it("should get multiple elements") {
      val l = Vector(1) :: Vector("foo") :: Vector(2.3) :: HNil
      assert(l.selectMany[Vector[String] :: Vector[Int] ::  HNil] == Vector("foo") :: Vector(1) :: HNil)
      assert(l.selectMany[Vector[Int] :: Vector[String] :: HNil] == Vector(1) :: Vector("foo") :: HNil)
      assert(l.selectMany[Vector[Double] :: Vector[String] :: HNil] == Vector(2.3) :: Vector("foo") :: HNil)
      assert(l.selectMany[Vector[String] :: Vector[Double] :: HNil] == Vector("foo") :: Vector(2.3) :: HNil)
    }
  }
  describe("adder") {
    it("should add a new element to an empty list") {
      assert(hnil.add(Vector(1)) == Vector(1) :: HNil)
    }
    val l = Vector("foo") :: HNil
    it("should add a new element to an existing list") {
      assert(l.add(Vector(1)) == Vector("foo") :: Vector(1) :: HNil)
    }
    it("should add it to the existing element if one exists, and sort them") {
      assert(l.add(Vector("bar")) == Vector("bar", "foo") :: HNil)
    }
  }
  describe("addmany") {
    it("should be identity if L is empty") {
      val l = Vector(1) :: HNil
      val added = hnil.addMany(l)
      assert(added.select[Vector[Int]] == Vector(1))
    }
    it("should be identity if SL is empty") {
      val sl = Vector(1) :: HNil
      val added = sl.addMany(hnil)
      assert(added.select[Vector[Int]] == Vector(1))
    }
    it("should add unrelated") {
      val l = Vector(1, 2, 3) :: HNil
      val sl = Vector("a", "b", "c") :: HNil
      val added = l.addMany(sl)
      assert(added.select[Vector[Int]] == Vector(1, 2, 3))
      assert(added.select[Vector[String]] == Vector("a", "b", "c"))
    }
    it("should add and sort") {
      val l = Vector(1, 2, 5, 6) :: Vector("a", "b", "d") :: HNil
      val sl = Vector("c") :: Vector(3, 4) ::  HNil
      val added = l.addMany(sl)
      assert(added.select[Vector[Int]] == Vector(1, 2, 3, 4, 5, 6))
      assert(added.select[Vector[String]] == Vector("a", "b", "c", "d"))
    }
  }
}

package epic.slab.test {
  import epic.slab.{Sentence, Span}
  import shapeless._

  object TestVals {
    val list = Vector(Sentence(Span(0, 1)), Sentence(Span(2, 3))) :: HNil
    val added = Vector(Sentence(Span(1, 2)))
    val result = Vector(Sentence(Span(0, 1)), Sentence(Span(1, 2)), Sentence(Span(2, 3))) :: HNil
  }
}

package epic.slab.test2 {
  import org.scalatest.FunSpec
  import epic.slab.typeclasses.HOps._
  import epic.slab.test.TestVals._

  class ImplicitOrderingInAdderTest extends FunSpec {
    describe("implicit ordering in adder") {
      it("should work without importing") {
        assert(list.add(added) == result)
      }
    }
  }
}
