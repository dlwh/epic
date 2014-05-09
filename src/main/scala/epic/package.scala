import java.text.CharacterIterator
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
      while(i != -1) {
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
      this
    }
  }

  private class BSIterator(bs: java.util.BitSet) extends Iterator[Int] {
    var currentBit = bs.nextSetBit(0)
    def hasNext: Boolean = currentBit != -1

    def next = {
      assert(currentBit != -1)
      val cur = currentBit
      currentBit = bs.nextSetBit(cur+1)
      cur
    }
  }

  implicit def _bitsetcbf[U] = new CanBuildFrom[java.util.BitSet, U, Set[U]] {
    def apply(from: BitSet): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
    def apply(): mutable.Builder[U, Set[U]] = Set.newBuilder[U]
  }


  implicit class AwesomeScalaBitSet(val bs: collection.BitSet) extends AnyVal {
    def toJavaBitSet = {
      java.util.BitSet.valueOf(bs.toBitMask)
    }
  }


  implicit class AwesomeCharacterIterator(val iter: CharacterIterator) extends AnyVal {

    def currentCodePoint = codePointAt(iter.getIndex)
    def charAt(pos: Int) = stateless {
      iter.setIndex(pos)
    }

    def codePointAt(pos: Int) = stateless {
      val ch = iter.setIndex(pos)
      if(Character.isHighSurrogate(ch)) {
        val ch2 = iter.next()
        if(Character.isLowSurrogate(ch2)) {
          Character.toCodePoint(ch, ch2)
        } else {
          ch
        }
      } else {
        ch
      }
    }


    def codePointBefore(pos: Int) = stateless {
      val ch = iter.setIndex(pos - 1)
      if(Character.isLowSurrogate(ch)) {
        val ch2 = iter.previous()
        if(Character.isHighSurrogate(ch2)) {
          Character.toCodePoint(ch2, ch)
        } else {
          ch
        }
      } else {
        ch
      }
    }


    def stateless[T](thunk: =>T) = {
      val ind = iter.getIndex
      try {
        thunk
      } finally {
        iter.setIndex(ind)
      }
    }

    def substring(from: Int, to: Int) = {
      if(from == to) ""
      else {
        stateless {
          val sb = new mutable.StringBuilder()
          iter.setIndex(from)
          sb += iter.current()
          for (i <- (from + 1) until to) {
            sb += iter.next()
          }
          sb.result()
        }
      }
    }

    def previousIndexWhere(pred: Char=>Boolean) = {
      stateless {

        var ch = iter.previous()
        var index = iter.getIndex
        while(ch != CharacterIterator.DONE && !pred(ch)) {
          ch = iter.previous()
          index = iter.getIndex
        }



        index
      }
    }

    def nextIndexWhere(pred: Char=>Boolean) = {
      stateless {

        var ch = iter.next()
        var index = iter.getIndex
        while(ch != CharacterIterator.DONE && !pred(ch)) {
          ch = iter.next()
          index = iter.getIndex
        }
        if(ch == CharacterIterator.DONE) -1
        else index
      }
    }
  }
}
