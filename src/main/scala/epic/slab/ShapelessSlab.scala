package epic.slab
import epic.slab.Slab._
import epic.trees.Span
import shapeless._
import LUBConstraint._
import ops.hlist.Prepend
import shapeless.syntax.typeable._

trait Annotation extends Serializable {}
trait Located

// Handles annotation over regions
// any-dimensional required
trait SpanAnnotation extends Annotation {}

// Handles annotation referencing other annotations
trait RecursiveAnnotation extends Annotation {}

// Handles annotation on the document-level
trait DocumentAnnotation extends Annotation {}

class InefficientShapelessSlab[Content, Annotations, L <: HList: <<:[Annotation]#λ](
  val content: Content,
  val annotations: L) {

  def prepend[A <: Annotation](annotation: A): InefficientShapelessSlab[Content, Annotations with A, A :: L] = {
    new InefficientShapelessSlab(this.content, annotation +: this.annotations)
  }

  def toHList: HList = this.annotations
  def subList[T: Typeable]: List[T] = Utils.sublist[T](this.toHList)
}

object InefficientShapelessSlab {
  def apply[C](content: C): InefficientShapelessSlab[C, Any, HNil] = new InefficientShapelessSlab[C, Any, HNil](content, HNil)
}

object Utils {
  def sublist[T: Typeable](l: HList): List[T] = (for(hd :: tail <- l.cast[_ :: HList]) yield hd.cast[T].toList ++ sublist[T](tail)).toList.flatten

  // https://stackoverflow.com/questions/25713668/do-a-covariant-filter-on-an-hlist
  trait CoFilter[L <: HList, U] extends DepFn1[L] { type Out <: HList }

  object CoFilter {
    def apply[L <: HList, U](implicit f: CoFilter[L, U]): Aux[L, U, f.Out] = f

    type Aux[L <: HList, U, Out0 <: HList] = CoFilter[L, U] { type Out = Out0 }

    implicit def hlistCoFilterHNil[L <: HList, U]: Aux[HNil, U, HNil] =
      new CoFilter[HNil, U] {
        type Out = HNil
        def apply(l: HNil): Out = HNil
      }

    implicit def hlistCoFilter1[U, H <: U, T <: HList]
      (implicit f: CoFilter[T, U]): Aux[H :: T, U, H :: f.Out] =
        new CoFilter[H :: T, U] {
          type Out = H :: f.Out
          def apply(l: H :: T): Out = l.head :: f(l.tail)
        }

    implicit def hlistCoFilter2[U, H, T <: HList]
      (implicit f: CoFilter[T, U], e: H <:!< U): Aux[H :: T, U, f.Out] =
        new CoFilter[H :: T, U] {
          type Out = f.Out
          def apply(l: H :: T): Out = f(l.tail)
        }
  }

  implicit final class HListOps[L <: HList](val l: L)  {
    def covariantFilter[U](implicit filter: CoFilter[L, U]): filter.Out = filter(l)
  }
}
