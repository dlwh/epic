package epic.srl

import breeze.data.Example
import epic.ontonotes.{Frame, Argument}

case class SRLInstance(words: IndexedSeq[String], frame: Frame, id: String) extends Example[IndexedSeq[Argument],(IndexedSeq[String], String, Int)] {
  def features: (IndexedSeq[String], String, Int) = (words, frame.lemma, frame.pos)

  def label: IndexedSeq[Argument] = frame.args

  def stripEmbedded = copy(frame=frame.stripEmbedded)
}
