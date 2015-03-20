import epic.slab._
import org.scalatest._
import scalaz.std.vector._

class ShapelessSlabTest extends FunSpec {
  import shapeless.test.illTyped
  import shapeless._

  describe("basic slab") {
    val slab = Slab("Foobar")
    val annotation = new Annotation {}
    it("should store an annotation") {
      val slab2 = slab.add(annotation)
      assert(slab2.select[Annotation](0) === annotation)
    }
    it("should add further annotations to the same vector, and sort them") {
      case class Foo(foo: Int) extends Annotation
      val slab2 = slab.add(Foo(13))
      assert(slab2.select[Foo](0) === Foo(13))
      val slab3 = slab2.add(Foo(42))
      assert(slab3.select[Foo] === Vector(Foo(13), Foo(42)))
    }
    it("should raise a type error if trying to retrieve that's not available") {
      case class Foo(foo: Int) extends Annotation
      val slab1 = slab.add(annotation)
      // Implicit not found:
      // epic.slab.Utils.SubSelector[shapeless.::[Vector[epic.slab.Annotation],shapeless.HNil],
      // Vector[Foo]]. You requested to select an element of type lower or
      // equal to Vector[Foo], but none were found in HList
      // shapeless.::[Vector[epic.slab.Annotation],shapeless.HNil]
      illTyped {
        """ slab1.select[Foo] """
      }
    }
    it("should also work with empty annotation lists") {
      case class Foo(foo: Int) extends Annotation
      val slab1 = slab.add(Vector[Int]().map(Foo(_)).toVector)
      assert(slab1.select[Foo] == Vector[Foo]())
    }
  }
}
