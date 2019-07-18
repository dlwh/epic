package epic.features

import java.util

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class DistanceBinner private (val binThresholds: Array[Int], preserveDirection: Boolean) extends  Serializable{
  def numBins = binThresholds.length + 1

  def this(numBins: Int, numExactBins: Int, preserveDirection: Boolean) = this(DistanceBinner.mkBinArray(numBins, numExactBins), preserveDirection)
  def this(numBins: Int = 8, preserveDirection: Boolean = true) = this(numBins, numBins/2+1, preserveDirection)
  util.Arrays.sort(binThresholds)

  val bins = Array.tabulate(binThresholds.last + 5){dist =>
    var bin = util.Arrays.binarySearch(binThresholds, math.abs(dist))
    if (bin < 0) bin = ~bin
    if (preserveDirection) math.signum(dist) * (bin + 1)
    else bin + 1
  }
  val negativeBins = Array.tabulate(binThresholds.last + 5){dist =>
    var bin = util.Arrays.binarySearch(binThresholds, math.abs(dist))
    if (bin < 0) bin = ~bin
    if (preserveDirection) -(bin + 1)
    else bin + 1
  }

  def distanceBin(a: Int, b: Int): Int = {
    val dist: Int = b - a
    distanceBin(dist)
  }

  def distanceBin(dist: Int): Int = {
    val array = if (dist < 0) negativeBins else bins
    val adist = math.min(math.abs(dist), array.length - 1)
    array(adist)
  }

  def binnedDistance(a: Int, b: Int): Int = {
    binnedDistance(b-a)
  }

  def binnedDistance(dist: Int): Int = {
    val bin = distanceBin(dist)
    if (dist == 0) 0
    else if (bin < 0) {
      if (-bin-1 >= binThresholds.length)
        -(binThresholds.last + 1)
      else -binThresholds(-bin-1)
    } else if (bin >= binThresholds.length) {
        binThresholds.last + 1
    } else binThresholds(bin-1)
  }

  def binIds = 0 +: binThresholds.toIndexedSeq :+ (binThresholds.last + 1)

}

object DistanceBinner {
  def mkBinArray(numBins: Int, numExactBins: Int): Array[Int] = {
    if (numBins <= 1) Array(1)
    else {
      val exact = Array.range(1, numExactBins+1)
      exact ++ Array.iterate(exact.last, (numBins - numExactBins) max 1)(exact => exact * 2).drop(1)
    }
  }

  def apply(binThresholds: Array[Int], preserveDirection: Boolean): DistanceBinner = new DistanceBinner(binThresholds, preserveDirection)
  def apply(numBins: Int, numExactBins: Int, preserveDirection: Boolean):DistanceBinner = apply(DistanceBinner.mkBinArray(numBins, numExactBins), preserveDirection)
  def apply(numBins: Int = 8, preserveDirection: Boolean = true): DistanceBinner = apply(numBins, numBins/2 + 1, preserveDirection)

}

