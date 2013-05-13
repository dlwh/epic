package epic.newfeatures

import java.util

/**
 *
 * @author dlwh
 */
class DistanceBinner private (val binThresholds: Array[Int], preserveDirection: Boolean) {
  def numBins = binThresholds.length + 1

  def this(numBins: Int, numExactBins: Int, preserveDirection: Boolean) = this(DistanceBinner.mkBinArray(numBins, numExactBins), preserveDirection)
  def this(numBins: Int = 8, preserveDirection: Boolean = true) = this(numBins, numBins/2+1, preserveDirection)
  util.Arrays.sort(binThresholds)
  def distanceBin(a: Int, b: Int) = {
    var bin = util.Arrays.binarySearch(binThresholds, (a - b).abs)
    if(bin < 0) bin = ~bin
    if(preserveDirection) math.signum(b-a) * (bin+1)
    else bin+1
  }

  def binnedDistance(a: Int, b: Int) = {
    val bin = distanceBin(a, b)
    if(a == b) 0
    else if(bin < 0) {
      if(-bin-1 >= binThresholds.length)
        -(binThresholds.last + 1)
      else -binThresholds(-bin-1)
    } else if(bin >= binThresholds.length) {
        (binThresholds.last + 1)
    } else binThresholds(bin-1)
  }

}


object DistanceBinner {
  def mkBinArray(numBins: Int, numExactBins: Int): Array[Int] = {
    if(numBins <= 1) Array(1)
    else {
      val exact = Array.range(1, numExactBins+1)
      exact ++ Array.iterate(exact.last, (numBins - numExactBins) max 1)(exact => exact * 2).drop(1)
    }
  }

  def apply(binThresholds: Array[Int], preserveDirection: Boolean): DistanceBinner = new DistanceBinner(binThresholds, preserveDirection)
  def apply(numBins: Int, numExactBins: Int, preserveDirection: Boolean):DistanceBinner = apply(DistanceBinner.mkBinArray(numBins, numExactBins), preserveDirection)
  def apply(numBins: Int = 8, preserveDirection: Boolean = true): DistanceBinner = apply(numBins, numBins/2 + 1, preserveDirection)

}