package epic.parser.morph

case class MorphFeat(label: String, value: String)

object MorphFeat {
  def readMorphFeatsFromBit(morphBit: String): Set[MorphFeat] = {
    if (morphBit == "_") {
      Set()
    } else {
      val morphFeats = morphBit.split("\\|").filter(_ != "_")
      val morphFeatsSeq = for (feat <- morphFeats) yield {
        if (feat.contains("=")) {
          val equalsIndex = feat.indexOf("=")
          MorphFeat(feat.substring(0, equalsIndex), feat.substring(equalsIndex + 1))
        } else {
          MorphFeat(feat, "")
        }
      }
      morphFeatsSeq.toSet
    }
  }
}
