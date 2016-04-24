package epic.features

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
case class HashFeature(hashBucket: Int) extends Feature

object HashFeature {
  sealed trait Scale {
    def numFeatures(nonHashFeatures: Int): Int
  }

  case class Absolute(numHashFeatures: Int) extends Scale {
    def numFeatures(nonHashFeatures: Int): Int = numHashFeatures
  }

  case class Relative(scale: Double) extends Scale {
    def numFeatures(nonHashFeatures: Int): Int = (scale * nonHashFeatures).toInt
  }

  val None = Absolute(0)

}

