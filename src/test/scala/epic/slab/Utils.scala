package epic.slab

import org.scalatest._
import epic.slab._

import org.scalatest.FunSpec

class HListUtilsTest extends FunSpec {
  import shapeless._
  implicit def hlistOps[L <: HList](l : L) : Utils.HListOps[L] = new Utils.HListOps(l)

  describe("covariant filter") {
    it("should not filter subtypes") {
      class Foo(val foo: Int)
      class Bar(val bar: Int) extends Foo(bar)
      val l = new Foo(1) :: new Bar(2) :: new Foo(3) :: new Bar(4) :: HNil
      assert(l.covariantFilter[Foo] == l)
    }
    it("should filter supertypes") {
      class Foo(val foo: Int)
      class Bar(val bar: Int) extends Foo(bar)
      val l = new Foo(1) :: new Bar(2) :: new Foo(3) :: new Bar(4) :: HNil
      assert(l.covariantFilter[Bar] != l)
    }
  }
  describe("selectMany") {
    it("should get a single element") {
      val l = 1 :: "foo" :: HNil
      assert(l.selectMany[String :: HNil].at(0) == "foo")
    }
  }
}
