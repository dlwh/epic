package epic.util

/**
 * TODO
 *
 * @author dlwh
 **/
object BinarySearch {

  def interpolationSearch[T](objs: IndexedSeq[T], proj: T=>Int, toFind: Int): Int = {
    if (objs.isEmpty) return ~0

    // Returns index of toFind in sortedArray, or -1 if not found
    var low = 0
    var lowV = proj(objs(low))
    var high = objs.length - 1
    var highV = proj(objs(high))

    while (lowV <= toFind && highV >= toFind) {
      val mid = (if (highV == lowV) low else low + ((toFind - lowV.toLong) * (high - low)) / (highV.toLong - lowV.toLong)).toInt

      val midV = proj(objs(mid))
      if (midV < toFind){
        low = mid + 1
        lowV = proj(objs(low))
      } else if (midV > toFind) {
        high = mid - 1
        highV = proj(objs(high))
      } else {
        return mid
      }
    }

    if (lowV == toFind) {
      low
    } else if (lowV > toFind) {
      ~low
    } else {
      ~(high + 1)
    }
  }

}
