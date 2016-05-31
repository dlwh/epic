import java.util.BitSet

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

/**
 *
 * @author dlwh
 */
package object epic {

  implicit class AwesomeBitSet(val bs: java.util.BitSet) extends AnyVal {
    def apply(r: Int) = bs.get(r)

    def iterator:Iterator[Int] = new BSIterator(bs)

    def map[U, C](f: Int=>U)(implicit cbf: CanBuildFrom[java.util.BitSet, U, C]) = {
      val r: mutable.Builder[U, C] = cbf(bs)
      r.sizeHint(bs.size)
      iterator foreach { i =>
        r += f(i)
      }

      r.result()
    }

    def foreach[U](f: Int=>U) {
      var i = bs.nextSetBit(0)
      while (i != -1) {
        f(i)
        i = bs.nextSetBit(i+1)
      }

    }

    def &=(other: BitSet) {
      bs and other
    }


    def &~=(other: BitSet) {
      bs andNot other
    }

    def &~(other: BitSet) = {
      val x = copy
      x andNot other
      x
    }

    def &(other: BitSet) = {
      val x = copy
      x and other
      x
    }

    def |=(other: BitSet) {
      bs or other
    }


    def |(other: BitSet) = {
      val x = copy
      x |= other
      x
    }

    def ^=(other: BitSet) {
      bs xor other
    }

    def ^(other: BitSet) = {
      val x = copy
      x ^= other
      x
    }

    def copy = bs.clone().asInstanceOf[java.util.BitSet]

    def nonEmpty = !bs.isEmpty

    def +=(i: Int) = {
      bs.set(i)
      bs
    }

    def ++=(xs: TraversableOnce[Int]) = {
      xs.foreach(+=)
      bs
    }
  }

  private class BSIterator(bs: java.util.BitSet) extends Iterator[Int] {
    var currentBit = bs.nextSetBit(0)
    def hasNext: Boolean = currentBit != -1

    def next() = {
      assert(currentBit != -1)
      val cur = currentBit
      currentBit = bs.nextSetBit(cur+1)
      cur
    }
  }

  implicit def _bitsetcbf[U]: CanBuildFrom[BitSet, U, Set[U]]  = new CanBuildFrom[java.util.BitSet, U, Set[U]] {
    def apply(from: BitSet): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
    def apply(): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
  }


  implicit class AwesomeScalaBitSet(val bs: collection.BitSet) extends AnyVal {
    def toJavaBitSet = {
      java.util.BitSet.valueOf(bs.toBitMask)
    }
  }

}
