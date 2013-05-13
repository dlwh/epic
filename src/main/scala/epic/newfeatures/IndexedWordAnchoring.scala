package epic.newfeatures

import epic.newfeatures.FeaturizationLevel.FullFeatures

trait IndexedWordAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForWord(pos: Int, level: FeaturizationLevel = FullFeatures):Array[Int]
}


