package epic.slab.typeclasses

import shapeless._
import ops.hlist._
import scala.annotation.implicitNotFound

// Replace with RemoveAll at some point, or depend on it.
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
    (implicit selector: SubSelector[L, E], selectMany: Aux[L, Result, Result]): Aux[L, E :: Result, E :: Result] =
      new SelectMany[L, E :: Result] {
        type Out = E :: Result
        def apply(l : L): Out = {
          val e = selector(l)
          val sl = selectMany(l)
          e :: sl
        }
      }
}

@implicitNotFound("Implicit not found: epic.slab.Utils.SubSelector[${L}, ${V}]. You requested to select an element of type lower or equal to ${V}, but none were found in HList ${L}.")
trait SubSelector[L <: HList, V] {
  def apply(l: L): V
}

object SubSelector {
  def apply[L <: HList, V](implicit subsel: SubSelector[L, V]): SubSelector[L, V] = subsel

  implicit def found[H <: V, T <: HList, V]: SubSelector[H :: T, V] =
    new SubSelector[H :: T, V] {
      def apply(l : H :: T): V = l.head
    }

  implicit def notFound[H, T <: HList, V]
    (implicit subsel: SubSelector[T, V]): SubSelector[H :: T, V] =
    new SubSelector[H :: T, V] {
      def apply(l : H :: T): V = subsel(l.tail)
    }

}

trait LowPriorityAdderImplicits {
  implicit def foundNoOrdering[T <: HList, U]: Adder.Aux[Vector[U] :: T, U, Vector[U] :: T] =
    new Adder[Vector[U] :: T, U] {
      type Out = Vector[U] :: T
      def apply(l: Vector[U] :: T, collection: Vector[U]): Out = {
        (l.head ++ collection) :: l.tail
      }
    }

  implicit def notFound[H, T <: HList, U, OutT <: HList](implicit ut: Adder.Aux[T, U, OutT]): Adder.Aux[H :: T, U, H :: OutT] =
    new Adder[H :: T, U] {
      type Out = H :: OutT
      def apply(l: H :: T, collection: Vector[U]): Out = {
        val outT = ut(l.tail, collection)
        l.head :: outT
      }
    }

}

@implicitNotFound("Implicit not found: epic.slab.typeclasses.Adder[${L}, ${U}].")
sealed trait Adder[L <: HList, U] extends DepFn2[L, Vector[U]]

object Adder extends LowPriorityAdderImplicits {
  def apply[L <: HList, U: Ordering](implicit adder: Adder[L, U]): Aux[L, U, adder.Out] = adder

  type Aux[L <: HList, U, Out0] = Adder[L, U] { type Out = Out0 }

  implicit def foundWithOrdering[T <: HList, U: Ordering]: Adder.Aux[Vector[U] :: T, U, Vector[U] :: T] =
    new Adder[Vector[U] :: T, U] {
      type Out = Vector[U] :: T
      def apply(l: Vector[U] :: T, collection: Vector[U]): Out = {
        (l.head ++ collection).sorted :: l.tail
      }
    }

  implicit def empty[U]: Aux[HNil, U, Vector[U] :: HNil] =
    new Adder[HNil, U] {
      type Out = Vector[U] :: HNil
      def apply(l: HNil, collection: Vector[U]): Out = collection :: HNil
    }
}

object HOps {
  implicit class Ops[L <: HList](l: L) {
    def subselect[V](implicit subsel: SubSelector[L, V]): V = subsel(l)
    def selectMany[SL <: HList](implicit sm: SelectMany[L, SL]): sm.Out = sm(l)
    def add[U](v: Vector[U])(implicit adder: Adder[L, U]): adder.Out = adder(l, v)
  }
}
