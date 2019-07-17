package epic.features

import java.util.Locale
import java.util.zip.GZIPInputStream

import scala.io.Source

/**
 * TODO
 *
 * @author dlwh
 **/
object HierarchicalClusters {

  def English = forLanguage("en")

  def forLanguage(lang: String): Map[String, String] = forLanguage(Locale.forLanguageTag(lang))

  def forLanguage(lang: Locale): Map[String, String] = {
    val in = new GZIPInputStream(this.getClass.getResourceAsStream(s"hierarchical-${lang.getLanguage}-clusters.gz"))

    val src = Source.fromInputStream(in)
    val pairs = for {
      line <- src.getLines
      Array(word, cluster) = line.split("\t")
      wMorph <- Set(word, word.map { case x if x.isDigit => '1' case x => x})
    } yield {
      word -> cluster.intern
    }

    val map = pairs.toMap
    in.close()

    map
  }

}

