package epic.slab.typeclasses

import shapeless._
import ops.hlist._
import scalaz._
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

  implicit def eol[L <: HList, V: Monoid]: SubSelector[L, V] =
    new SubSelector[L, V] {
      def apply(l : L): V = Monoid[V].zero
    }

  implicit def found[H <: V, T <: HList, E, V: Monoid]
    (implicit subsel: SubSelector[T, V]): SubSelector[H :: T, V] =
    new SubSelector[H :: T, V] {
      def apply(l : H :: T): V = {
        l.head
      }
    }

  implicit def notFound[H, T <: HList, E, V]
    (implicit subsel: SubSelector[T, V]): SubSelector[H :: T, V] =
    new SubSelector[H :: T, V] {
      def apply(l : H :: T): V = {
        subsel(l.tail)
      }
    }

}

@implicitNotFound("Implicit not found: epic.slab.typeclasses.Adder[${L}, ${V}]. Check that you imported scalaz.std.list._")
sealed trait Adder[L <: HList, V] extends DepFn2[L, V]

object Adder {
  def apply[L <: HList, V](implicit adder: Adder[L, V]): Aux[L, V, adder.Out] = adder

  type Aux[L <: HList, V, Out0] = Adder[L, V] { type Out = Out0 }

  implicit def found[T <: HList, V: Monoid]: Aux[V :: T, V, V :: T] =
    new Adder[V :: T, V] {
      type Out = V :: T
      def apply(l: V :: T, collection: V): Out = {
        val v: V = l.head
        Monoid[V].append(v, collection) :: l.tail
      }
    }

  implicit def notFound[H, T <: HList, V, OutT <: HList](implicit ut: Aux[T, V, OutT]): Aux[H :: T, V, H :: OutT] =
    new Adder[H :: T, V] {
      type Out = H :: OutT
      def apply(l: H :: T, collection: V): Out = {
        val outT = ut(l.tail, collection)
        l.head :: outT
      }
    }

  implicit def empty[V]: Aux[HNil, V, V :: HNil] =
    new Adder[HNil, V] {
      type Out = V :: HNil
      def apply(l: HNil, collection: V): Out = collection :: HNil
    }
}

object HOps {
  implicit class Ops[L <: HList](l: L) {
    def subselect[V](implicit subsel: SubSelector[L, V]): V = subsel(l)
    def selectMany[SL <: HList](implicit sm: SelectMany[L, SL]): sm.Out = sm(l)
    def add[V](v: V)(implicit adder: Adder[L, V]): adder.Out = adder(l, v)
  }
}
