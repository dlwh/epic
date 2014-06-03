package epic.util

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers
import org.scalacheck.Prop

/**
 * TODO
 *
 * @author dlwh
 **/
class BinarySearchTest extends FunSuite with Checkers {

  test("Basic Interpolation Search") {
    check(Prop.forAll{(array: Array[Int], needle: Int) =>
      java.util.Arrays.sort(array)
      val res1 = BinarySearch.interpolationSearch(array.toVector, identity[Int], needle)
      val res2 = java.util.Arrays.binarySearch(array, needle)
      res1 == res2 || (res1 >= 0 && res2 >= 0 && array(res1) == array(res2))
    })

  }

}
