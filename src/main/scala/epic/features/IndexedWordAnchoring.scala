package epic.features

import epic.features.FeaturizationLevel.FullFeatures

trait IndexedWordAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForWord(pos: Int, level: FeaturizationLevel = FullFeatures):Array[Int]
}


