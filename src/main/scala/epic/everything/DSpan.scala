package epic.everything

import epic.trees.Span

case class DSpan(doc: String, sentence: Int, begin: Int, end: Int) {
  def length = end - begin

  def span = Span(begin, end)

  def render(doc: Document):String = render(doc.sentences.map(_.words))
  def render(doc: IndexedSeq[IndexedSeq[String]]):String = (begin until end).map(doc(sentence)).mkString("["," ","]")

  def getYield(doc: Document):IndexedSeq[String] = getYield(doc.sentences.map(_.words))
  def getYield(doc: IndexedSeq[IndexedSeq[String]]):IndexedSeq[String] = (begin until end).map(doc(sentence))

  override def toString = doc + ":" + sentence + ":" + begin + "-" + end
}

object DSpan {
  implicit val ordering: Ordering[DSpan] = new Ordering[DSpan] {
    def compare(x: DSpan, y: DSpan): Int = {
      x.doc.compare(y.doc) match {
        case 0 =>
          if(x.sentence < y.sentence) -1
          else if(x.sentence > y.sentence) 1
          else if(x.begin < y.begin) -1
          else if (x.begin > y.begin) 1
          else x.end - y.end
        case z => z
      }
    }
  }
}

case class DPos(doc: String, sentence: Int, pos: Int)


object DPos {
  implicit val ordering: Ordering[DPos] = new Ordering[DPos] {
    def compare(x: DPos, y: DPos): Int = {
      x.doc.compare(y.doc) match {
        case 0 =>
          if(x.sentence < y.sentence) -1
          else if(x.sentence > y.sentence) 1
          else x.pos - y.pos
        case z => z
      }
    }
  }
}
