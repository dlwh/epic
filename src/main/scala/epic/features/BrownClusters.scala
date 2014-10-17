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

  def clusterFor(w: String, default:String = "00"):String = theClusters.getOrElse(w, default)


  trait DSL {
    val brown = new BrownClusterFeaturizer()
    def brownClusters(prefixBegin: Int = 7, prefixEnd: Int = 10) = new BrownClusterFeaturizer(prefixBegin, prefixEnd)
  }
}


case class BrownClusterFeature(f: String) extends Feature

case class BrownClusterFeaturizer(prefixBegin: Int = 7, prefixEnd: Int = 10) extends WordFeaturizer[String] with Serializable {

  def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
    def featuresForWord(pos: Int): Array[Feature] = {
      if(pos < 0 || pos >= words.length)
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

  case object UnknownWordFeature extends Feature

  // prefixes of lengths prefixBegin to prefixEnd
  private val clusterFeatures = {
    BrownClusters.clusterIds
      .iterator
      .map(k => k -> Array.range(prefixBegin min k.length, prefixEnd min k.length).map(l => BrownClusterFeature(k.substring(0, l)):Feature))
      .toMap
  }
}
