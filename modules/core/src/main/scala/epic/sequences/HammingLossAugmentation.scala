package epic.sequences

import epic.framework.LossAugmentation
import epic.constraints.LabeledSpanConstraints
import breeze.util.{OptionIndex, Index}

/**
 * TODO
 *
 * @author dlwh
 **/
class HammingLossAugmentation[L, W](startSymbol: L, labelIndex: Index[L],
                                    precisionScale: Double = 1.0,
                                    recallScale: Double = 1.0) extends LossAugmentation[Segmentation[L, W], SemiCRF.Anchoring[L, W]] {
  def lossAugmentation(datum: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = {
    val gt = GoldSegmentPolicy.goldSegmentForcing[L](datum.segments.map{ case (k,v) => (labelIndex(k), v)})

    new HammingLossAugmentation.Anchoring(startSymbol, new OptionIndex(labelIndex), datum.words, gt, precisionScale, recallScale)
}

object HammingLossAugmentation {
  case class Anchoring[L, W](startSymbol: L,
                             labelIndex: OptionIndex[L],
                             words: IndexedSeq[W],
                             gt: GoldSegmentPolicy[L],
                             precisionScale: Double,
                             recallScale: Double,
                             constraints: LabeledSpanConstraints[L] = LabeledSpanConstraints.noConstraints[L])  extends SemiCRF.Anchoring[L, W]{


    def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int): Double = {
      if (gt.isGoldSegment(begin, end, cur)) -precisionScale
      else recallScale
    }

    override def ignoreTransitionModel: Boolean = true
  }

  }
}