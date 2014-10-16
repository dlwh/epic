package epic.slab
import shapeless._
import shapeless.syntax.typeable._
import ops.hlist._
import scala.annotation.implicitNotFound

object Utils {
  def iterator[T: Typeable](l: HList): Iterator[T] = (for(hd :: tail <- l.cast[_ :: HList]) yield hd.cast[T].toList ++ iterator[T](tail)).toIterator.flatten

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

  sealed trait Adder[L <: HList, A, V <: Vector[A]] extends DepFn2[L, Vector[A]]

  object Adder {
    def apply[L <: HList, A, V <: Vector[A]](implicit adder: Adder[L, A, V]): Aux[L, A, V, adder.Out] = adder

    type Aux[L <: HList, A, V <: Vector[A], Out0] = Adder[L, A, V] { type Out = Out0 }

    implicit def adderFound[T <: HList, A]: Aux[Vector[A] :: T, A, Vector[A], Vector[A] :: T] =
      new Adder[Vector[A] :: T, A, Vector[A]] {
        type Out = Vector[A] :: T
        def apply(l: Vector[A] :: T, vector: Vector[A]): Out =
          { val v: Vector[A] = l.head; (v ++ vector) :: l.tail }}

    implicit def adderNotFound[H, T <: HList, A, OutT <: HList](implicit ut: Aux[T, A, Vector[A], OutT]): Aux[H :: T, A, Vector[A], H :: OutT] =
      new Adder[H :: T, A, Vector[A]] {
        type Out = H :: OutT
        def apply(l: H :: T, vector: Vector[A]): Out = {
          val outT = ut(l.tail, vector)
          l.head :: outT}
      }

    implicit def empty[A]: Aux[HNil, A, Vector[A], Vector[A] :: HNil] =
      new Adder[HNil, A, Vector[A]] {
        type Out = Vector[A] :: HNil
        def apply(l: HNil, vector: Vector[A]): Out = vector :: HNil
      }
  }

  @implicitNotFound("Implicit not found: epic.slab.Utils.SelectMany[${L}, ${SL}]. You requested to select elements of the types ${SL}, but not all were found in HList ${L}.")
  trait SelectMany[L <: HList, SL <: HList] extends DepFn1[L]

  object SelectMany {
    def apply[L <: HList, SL <: HList](implicit selectM: SelectMany[L, SL]): Aux[L, SL, selectM.Out] = selectM

    type Aux[L <: HList, SL <: HList, Out0] = SelectMany[L, SL] { type Out = Out0 }

    implicit def eol[L <: HList]: Aux[L, HNil, HNil] =
      new SelectMany[L, HNil] {
        type Out = HNil
        def apply(l : L): Out = HNil
      }

    implicit def hlistSelectMany[L <: HList, E, Result <: HList]
      (implicit selector: Selector.Aux[L, E], selectMany: Aux[L, Result, Result]): Aux[L, E :: Result, E :: Result] =
        new SelectMany[L, E :: Result] {
          type Out = E :: Result
          def apply(l : L): Out = {
            val e = selector(l)
            val sl = selectMany(l)
            e :: sl
          }
        }
  }

  implicit final class HListOps[L <: HList](val l: L)  {
    def covariantFilter[U](implicit filter: CoFilter[L, U]): filter.Out = filter(l)
    def selectMany[SL <: HList](implicit sm: SelectMany[L, SL]): sm.Out = sm(l)
  }

}
