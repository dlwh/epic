package epic.ontonotes

import epic.trees.Span

/**
 * Represents a Span in a particular sentence in a particular Document
 */
case class DSpan(doc: String, sentence: Int, begin: Int, end: Int) {
  def length = end - begin

  def span = Span(begin, end)

  /**
   * Returns the words associated with this dspan as a string.
   * @param doc
   * @return
   */
  def render(doc: Document): String = render(doc.sentences.map(_.words))
  def render(doc: IndexedSeq[IndexedSeq[String]]): String = getYield(doc).mkString("[",", ", "]")

  /**
   * Gets the words associated with this document.
   * @param doc
   * @return
   */
  def getYield(doc: Document):IndexedSeq[String] = getYield(doc.sentences.map(_.words))
  def getYield(doc: IndexedSeq[IndexedSeq[String]]):IndexedSeq[String] = (begin until end).map(doc(sentence))

  override def toString = s"$doc:$sentence:$begin-$end"
}

object DSpan {
  implicit val ordering: Ordering[DSpan] = new Ordering[DSpan] {
    def compare(x: DSpan, y: DSpan): Int = {
      x.doc.compare(y.doc) match {
        case 0 =>
          if (x.sentence < y.sentence) -1
          else if (x.sentence > y.sentence) 1
          else if (x.begin < y.begin) -1
          else if (x.begin > y.begin) 1
          else x.end - y.end
        case z => z
      }
    }
  }
}

/**
 * Represents a discrete word position in a sentence in a document
 */
case class DPos(doc: String, sentence: Int, pos: Int) {
  /** Returns DSpan(doc, sentence, pos, pos + 1) */
  def asDSpan = DSpan(doc, sentence, pos, pos + 1)
}

object DPos {
  implicit val ordering: Ordering[DPos] = new Ordering[DPos] {
    def compare(x: DPos, y: DPos): Int = {
      x.doc.compare(y.doc) match {
        case 0 =>
          if (x.sentence < y.sentence) -1
          else if (x.sentence > y.sentence) 1
          else x.pos - y.pos
        case z => z
      }
    }
  }
}
