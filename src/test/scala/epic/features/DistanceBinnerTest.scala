package epic.features

import org.scalatest.FunSuite

/**
 *
 * @author dlwh
 */
class DistanceBinnerTest extends FunSuite {
  test("Distance Bin") {
    val binner = new DistanceBinner(preserveDirection = true)
    val dists = Array.tabulate(20,20) { (i, j) =>
      val dist = binner.binnedDistance(i, j)
      if (i < j)
        assert(dist > 0, (dist, i, j))
      else if (i == j)
        assert(dist === 0)
      else if (i > j) assert(dist < 0, (dist, i, j))
      dist
    }
    assert(dists.flatten.toSet.size == binner.numBins * 2 - 1, dists.flatten.toSet -> binner.numBins)

    for(i <- 1 until 19; j <- 1 until 19) {
      if (i != j) {
        assert(dists(i)(j) >= dists(i +1)(j), (i,j,dists(i)(j),dists(i+1)(j)))
        assert(dists(i)(j) >= dists(i)(j-1))
        assert(dists(i)(j) <= dists(i -1)(j), (i,j,dists(i)(j),dists(i+1)(j)))
        assert(dists(i)(j) <= dists(i)(j+1))
      }
    }
  }

  test("Distance Bin, no direction") {
    val binner = new DistanceBinner(preserveDirection = false)
    val dists = Array.tabulate(20,20) { (i, j) =>
      val dist = binner.binnedDistance(i, j)
      if (i != j)
        assert(dist > 0, (dist, i, j))
      else if (i == j)
        assert(dist === 0)
      dist
    }
    assert(dists.flatten.toSet.size === binner.numBins)

    for(i <- 1 until 19; j <- i until 19) {
      if (i < j) {
        assert(dists(i)(j) >= dists(i +1)(j), (i,j,dists(i)(j),dists(i+1)(j)))
        assert(dists(i)(j) >= dists(i)(j-1))
        assert(dists(i)(j) <= dists(i -1)(j), (i,j,dists(i)(j),dists(i+1)(j)))
        assert(dists(i)(j) <= dists(i)(j+1))
      }
    }
  }

}
