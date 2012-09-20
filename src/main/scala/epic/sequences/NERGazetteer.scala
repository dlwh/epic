package epic.sequences

import io.Source

object NERGazetteer {
  /**
   * Returns the gazetteer for a given language (just english right now).
   *
   * A Gazeteer is a map from IndexedSeq[String]->String, where the second string is NER type.
   *
   * @param lang
   * @return
   */
  def load(lang: String = "en"):Map[IndexedSeq[String], String] = {
    val resource = NERGazetteer.getClass.getClassLoader.getResourceAsStream("ner/" + lang +".lst")
    val src = Source.fromInputStream(resource)
    val map: Map[IndexedSeq[String], String] = {for(line <- src.getLines()) yield {
      val arr = line.split(" " )
       arr.drop(1).toIndexedSeq -> arr(0).intern()
    }}.toMap

    resource.close()
    map
  }

}
