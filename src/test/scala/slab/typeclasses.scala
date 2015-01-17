import epic.slab._
import epic.slab.typeclasses._
import org.scalatest._
import shapeless._
import scalaz._
import std.vector._
import std.list._

class Sentence(val loc: Int)
case class Token(val loc: Int)

case class ISentence(override val loc: Int, val id: Option[String] = None) extends Sentence(loc)
case class PSentence(override val loc: Int, val prob: Double = 0.0) extends Sentence(loc)

class SSSpec extends FunSpec {
  import HOps.Ops
  val sent = Vector(new Sentence(0))
  val isent = Vector(ISentence(0))
  val psent = Vector(PSentence(0))
  val token = Vector(Token(0))
  val annotations = isent :: psent :: token :: HNil

  describe("SubSelect") {
    it("should select both") {
      assert(Set(annotations.subselect[Vector[Sentence]]).flatten == Set(isent, psent).flatten)
    }
    it("should select only one subclass") {
      assert(annotations.subselect[Vector[PSentence]] == psent)
    }
  }

  describe("selectMany") {
    it("should get a single element") {
      val l = List(1) :: List("foo") :: HNil
      assert(l.selectMany[List[String] :: HNil].at(0) == List("foo"))
    }
    it("should get multiple elements") {
      val l = List(1) :: List("foo") :: List(2.3) :: HNil
      assert(l.selectMany[List[String] :: List[Int] ::  HNil] == List("foo") :: List(1) :: HNil)
      assert(l.selectMany[List[Int] :: List[String] :: HNil] == List(1) :: List("foo") :: HNil)
      assert(l.selectMany[List[Double] :: List[String] :: HNil] == List(2.3) :: List("foo") :: HNil)
      assert(l.selectMany[List[String] :: List[Double] :: HNil] == List("foo") :: List(2.3) :: HNil)
    }
  }
  describe("adder") {
    val l = List("foo") :: HNil
    it("should add a new element") {
      assert(l.add(List(1)) == List("foo") :: List(1) :: HNil)
    }
    it("should add it to the existing element if one exists") {
      assert(l.add(List("bar")) == List("foo", "bar") :: HNil)
    }
    it("should work correctly with subtypes too") {
      val l = sent :: isent :: psent :: HNil
      assert(l.add(Vector(PSentence(1))) == sent :: isent :: (psent :+ PSentence(1)) :: HNil)
    }
  }
}
