package epic.features

import java.util.zip.GZIPInputStream
import scala.io.Source
import epic.framework.Feature

/**
 *
 *
 * @author dlwh
 */
object BrownClusters {

  // word -> cluster, clusters
  lazy val theClusters: Map[String, String] = {
    val in = new GZIPInputStream(this.getClass.getResourceAsStream("bllip-clusters.gz"))
    val src = Source.fromInputStream(in)
    val pairs = for {
      line <- src.getLines
      Array(cluster, word, cnt) = line.split("\t")
      if cnt.toInt > 1
    } yield {
      word -> cluster.intern
    }
    val map = pairs.toMap
    in.close()
    map
  }

  lazy val clusterIds = theClusters.values.toSet

  def clusterFor(w: String, default:String = "00"): String = theClusters.getOrElse(w, default)

  trait DSL {
    // Tkachenko and Simanovsky liked these values
    val brown = new BrownClusterFeaturizer(Array(7, 11, 13))
    def brownClusters(lengths: Int*) = new BrownClusterFeaturizer(lengths.toArray)
  }
}

@SerialVersionUID(1L)
case class BrownClusterFeature(f: String) extends Feature with Serializable

@SerialVersionUID(1L)
case class BrownClusterFeaturizer(lengths: Array[Int]) extends WordFeaturizer[String] with Serializable {

  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    def featuresForWord(pos: Int): Array[Feature] = {
      if (pos < 0 || pos >= words.length)
        Array(BoundaryFeature)
      else
        features(pos)
    }

    def words: IndexedSeq[String] = w

    val features = words.map { w =>
      val c = BrownClusters.clusterFor(w)
      clusterFeatures.getOrElse(c, Array[Feature](UnknownWordFeature))
    }
  }

  @SerialVersionUID(1L)
  case object UnknownWordFeature extends Feature with Serializable

  // prefixes of lengths prefixBegin to prefixEnd
  private val clusterFeatures = {
    BrownClusters.clusterIds
      .iterator
      .map(k => k -> lengths.map(l => if (l > k.length) BrownClusterFeature(k) else BrownClusterFeature(k.substring(0, l))).toSet[Feature].toArray[Feature])
      .toMap
  }
}
