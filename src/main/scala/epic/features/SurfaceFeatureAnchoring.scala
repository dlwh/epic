package epic.features

import epic.framework.Feature
import breeze.util.Index

/**
 *
 * @author dlwh
 */
trait SurfaceFeatureAnchoring[W] {
  def words: IndexedSeq[W]

  def basicFeatures(pos: Int): Array[Int]
  def featuresForWord(pos: Int): Array[Int]
  def featuresForSpan(beg: Int, end: Int): Array[Int]

  def wordFeatureIndex: Index[Feature]
  def spanFeatureIndex: Index[Feature]
}
