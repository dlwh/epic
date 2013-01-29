package epic.srl

import breeze.data.Example
import epic.ontonotes.{DSpan, Frame, Argument}
import epic.trees.Span
import epic.sequences.Segmentation
import collection.mutable.ArrayBuffer

case class SRLInstance(words: IndexedSeq[String], frame: Frame, id: String) extends Example[IndexedSeq[Argument],(IndexedSeq[String], String, Int)] {
  def asSegmentation(outsideLabel: String = "O"): Segmentation[String, String] = {
    Segmentation(asSegments(outsideLabel), words, id+"-segmentation")
  }

  def lemma: String = frame.lemma
  def args = frame.args
  def pos: Int = frame.pos

  def length = words.length


  def features: (IndexedSeq[String], String, Int) = (words, frame.lemma, frame.pos)

  def label: IndexedSeq[Argument] = args

  def stripEmbedded = copy(frame=frame.stripEmbedded)


  def asSegments(outsideLabel: String="O"): IndexedSeq[(String, Span)] = {
    val sorted = label.sortBy(_.span.start)
    var out = new ArrayBuffer[(String, Span)]()
    var last = 0
    for( arg <- sorted ) {
      assert(last <= arg.span.start)
      while(arg.span.start != last) {
        out += (outsideLabel -> Span(last,last+1))
        last += 1
      }
      out += (arg.arg -> Span(arg.span.start, arg.span.end))
      last = arg.span.end
    }
    while(words.length != last) {
      out += (outsideLabel -> Span(last,last+1))
      last += 1
    }

    out
  }
}
