package epic.features

trait IndexedWordAnchoring[W] {
  def words: IndexedSeq[W]
  def featuresForWord(pos: Int):Array[Int]
}


