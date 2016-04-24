package epic.features

import epic.framework.Feature

/**
 * Includes all features in a window but doesn't include where the features are.
 * @author dlwh
 */
@SerialVersionUID(1L)
case class ContextFeaturizer[W](featurizer: WordFeaturizer[W], window: Int) extends WordFeaturizer[W] with Serializable {

  def anchor(w: IndexedSeq[W]): WordFeatureAnchoring[W] = new WordFeatureAnchoring[W] {
    val anchs = featurizer.anchor(w)
    def words: IndexedSeq[W] = w

    private val features: IndexedSeq[Array[Feature]] = {
      IndexedSeq.tabulate(words.length)(anchs.featuresForWord(_).map(ContextFeature))
    }

    private val settedFeatures = Array.tabulate(words.length) { pos =>
      (math.max(pos - window, 0) to math.min(pos + window, words.length - 1) filter(_ != pos)).flatMap(features(_)).toSet[Feature].toArray
    }

    def featuresForWord(pos: Int): Array[Feature] = {
      if (pos < 0 || pos >= words.length)
        Array.empty
      else
        settedFeatures(pos)
    }
  }
}

case class ContextFeature(f: Feature) extends Feature
