package epic.features

import breeze.linalg.Counter
import epic.framework.Feature
import epic.features.LongestFrequentSuffixFeaturizer.LongestFrequentSuffix
import breeze.numerics.I

/**
 * TODO
 *
 * @author dlwh
 **/
class LongestFrequentSuffixFeaturizer private (fixedMap: Map[String, Feature],
                                               suffixCounts: Counter[String, Double], commonWordThreshold: Double) extends WordFeaturizer[String] with Serializable {
  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    val feats = words.map(w => Array(fixedMap.getOrElse(w, LongestFrequentSuffix(lookup(w)))))

    def featuresForWord(pos: Int): Array[Feature] = if (pos < 0 || pos >= w.length) Array(BeginSentFeature) else feats(pos)

    def words: IndexedSeq[String] = w
  }

  def lookupSentence(sent: IndexedSeq[String]) = {
    sent.map(w => fixedMap.getOrElse(w, LongestFrequentSuffix(lookup(w))) match {
      case LongestFrequentSuffix(s) => "-" + s
      case IndicatorFeature(w) => w
    })
  }

  private def lookup(x: String): String = {
    x.tails.find(suffixCounts(_) >= commonWordThreshold).getOrElse("-UNK-")
  }
}

object LongestFrequentSuffixFeaturizer {
  def apply(counts: Counter[String, Double], commonWordThreshold: Int = 100) = {
    var suffixCounts = Counter[String, Double]()
    for( (k, v) <- counts.iterator; if v <= commonWordThreshold; tail <- k.tails) {
      suffixCounts(tail) += v
    }

    suffixCounts = suffixCounts.mapValues(v => v * I(v >= commonWordThreshold))

    def lookup(x: String): String = {
      x.tails.find(suffixCounts(_) >= commonWordThreshold).getOrElse("-UNK-")
    }

    val map = Map.empty ++ (for( (w,v) <- counts.iterator) yield {
      if (v > commonWordThreshold)
        w -> IndicatorFeature(w)
      else
        w -> LongestFrequentSuffix(lookup(w))
    })

    new LongestFrequentSuffixFeaturizer(map, suffixCounts, commonWordThreshold)
  }

  case class LongestFrequentSuffix(suffix: String) extends Feature
}
