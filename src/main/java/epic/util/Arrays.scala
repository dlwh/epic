package epic.util

import scala.reflect.ClassTag

/**
 * @author dlwh
 */
object Arrays {

  def fillArray(x: Int, y: Int, v: Double): Array[Array[Double]] = {
    val arr: Array[Array[Double]] = Array.ofDim[Double](x, y)
    if (v != 0.0) {
      var i: Int = 0
      while (i < x) {
        java.util.Arrays.fill(arr(i), v)
        i += 1
      }
    }
    arr
  }

  def concatenate(args: Array[Int]*): Array[Int] = {
    var totalLength: Int = 0
    for (a <- args) totalLength += a.length
    val ret: Array[Int] = new Array[Int](totalLength)
    var destPos: Int = 0
    for (a <- args) {
      System.arraycopy(a, 0, ret, destPos, a.length)
      destPos += a.length
    }
    ret
  }

  def concatenate[T:Manifest](args: Array[T]*): Array[T] = {
    var totalLength: Int = 0
    for (a <- args) totalLength += a.length
    val ret: Array[T] = new Array[T](totalLength)
    var destPos: Int = 0
    for (a <- args) {
      System.arraycopy(a, 0, ret, destPos, a.length)
      destPos += a.length
    }
    ret
  }


  def crossProduct[A,B,C](arr1: Array[A], arr2: Array[B])(f: (A,B) => C)(implicit man:ClassTag[C]): Array[C] = {
    val ret = new Array[C](arr1.length * arr2.length)
    var off = 0
    var i = 0
    while (i < arr1.length) {
      var j = 0
      while (j < arr2.length) {
        ret(off) = f(arr1(i), arr2(j))
        off += 1
        j += 1
      }
      i += 1
    }
    ret
  }

  /** Makes a arr1.length * arr2.length array, where entries are arr1(i) + arr2(j) * secondScale */
  def crossProduct(arr1: Array[Int], arr2: Array[Int], secondScale: Int): Array[Int] = {
    val ret = new Array[Int](arr1.length * arr2.length)
    var off = 0
    var i = 0
    while (i < arr1.length) {
      var j = 0
      while (j < arr2.length) {
        ret(off) = arr1(i) + arr2(j) * secondScale
        off += 1
        j += 1
      }
      i += 1
    }
    ret
  }
}

