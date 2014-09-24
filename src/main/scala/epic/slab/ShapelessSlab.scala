package epic.slab
import shapeless._
import LUBConstraint._
import shapeless.syntax.typeable._
import Utils._
import ops.hlist._

class Slab[Content, L <: HList](val content: Content, val annotations: L) {

  def get[T](index: Int)(implicit sel: Selector[L, Vector[T]]): T = sel(annotations)(index)
  def get[T](implicit sel: Selector[L, Vector[T]]): Vector[T] = sel(annotations)

  def add[A, Tmp <: HList, Result <: HList](newAnnotations: Vector[A])(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, newAnnotations))
  }
  def add[A, Tmp <: HList, Result <: HList](newAnnotation: A)(implicit adder: Adder.Aux[L, A, Vector[A], Result]): Slab[Content, Result] = {
    new Slab(content, adder(annotations, Vector(newAnnotation)))
  }

  def toHList: L = this.annotations
}

object Slab {
  def apply[C](content: C): Slab[C, HNil] = new Slab[C, HNil](content, HNil)
}

object Test {
  def main(args: Array[String]) = {
    val s = Slab("foo")
    val s1 = s.add(Vector(1,2,3))
    print(s1.toHList)
    val s2 = s1.add(Vector(4,5,6))
    print(s2.toHList)
    val s3 = s2.add(Vector("foo", "bar"))
    print(s3.toHList)
  }
}
