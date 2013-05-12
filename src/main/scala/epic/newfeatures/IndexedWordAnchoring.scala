package epic.newfeatures

import epic.newfeatures.FeaturizationLevel.FullFeatures
import breeze.util.Index
import epic.framework.Feature

trait IndexedWordAnchoring[W] {
  def words = innerAnchoring.words
  def wordFeatureIndex: Index[Feature]
  def innerAnchoring: WordFeatureAnchoring[W]
  def featuresForWord(pos: Int, level: FeaturizationLevel = FullFeatures):Array[Int]
}

trait IndexedWordFeaturizer[Datum, W] {
  def wordFeatureIndex: Index[Feature]
  def featurizer: WordFeaturizer[W]
  def anchor(datum: Datum):IndexedWordAnchoring[W]
}
