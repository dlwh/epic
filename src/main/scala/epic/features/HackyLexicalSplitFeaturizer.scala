package epic.features

import epic.framework.Feature

class HackyLexicalSplitFeaturizer[W]() extends SplitSpanFeaturizer[W] {
  val label = s"RelativeDifference"
  private val emptyArray = Array.empty[Feature]
  
  private val theSplitNeedingAnchoring = new SplitSpanFeatureAnchoring[W] with Serializable {
    def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        emptyArray
        // Array(DistanceFeature(db.binnedDistance((end-split) - (split-begin)), label))
    }

    def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
  }

  def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = {
    theSplitNeedingAnchoring
  }

}