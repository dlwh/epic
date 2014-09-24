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
}

class ShapelessSlabTest extends FunSpec {
  import shapeless.test.illTyped
  import shapeless._
  import LUBConstraint._

  describe("basic slab") {
    val slab = Slab("Foobar")
    val annotation = new Annotation {}
    it("should store an annotation") {
      val slab2 = slab.add(annotation)
      assert(slab2.get[Annotation](0) === annotation)
    }
    it("should add further annotations to the same vector, with disturbing the order") {
      case class Foo(foo: Int) extends Annotation
      val slab2 = slab.add(Foo(13))
      assert(slab2.get[Foo](0) === Foo(13))
      val slab3 = slab2.add(Foo(42))
      assert(slab3.get[Foo](0) === Foo(13))
      assert(slab3.get[Foo](1) === Foo(42))
    }
  }
}
