package epic.constraints

import org.scalatest.FunSuite
import breeze.collection.mutable.TriangularArray
import scala.collection.immutable.BitSet
import java.io.{ByteArrayInputStream, ObjectInputStream, ObjectOutputStream, ByteArrayOutputStream}

/**
 *
 *
 * @author dlwh
 */
class LabeledSpanConstraintsTest extends FunSuite {
  /*
  test("serialization") {
    val x = LabeledSpanConstraints[Int](TriangularArray.tabulate(10) { (i,j) =>
      if (i == j || i > 5) null
      else {
        if (i < j - 1) BitSet(1,2,3,4)
        else BitSet(i,j)
      }

    })
    val ba = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(ba)
    oos.writeObject(x)
    oos.close()
    val ooin = new ObjectInputStream(new ByteArrayInputStream(ba.toByteArray))
    val y = ooin.readObject().asInstanceOf[LabeledSpanConstraints[Int]]
    assert(x.containsAll(y))
    assert(y.containsAll(x))


  }
  */

  test("containsAll") {

    val x = LabeledSpanConstraints[Int](TriangularArray.tabulate(10) { (i,j) =>
      if (i == j || i > 5) null
      else {
        if (i < j) BitSet(1,2,3,4)
        else BitSet(1)
      }

    })
    val z = LabeledSpanConstraints[Int](TriangularArray.tabulate(10) { (i,j) =>
      if (i < j) BitSet(1,2,3,4)
      else BitSet(1)
    })

    assert(!x.containsAll(z))
  }

}
