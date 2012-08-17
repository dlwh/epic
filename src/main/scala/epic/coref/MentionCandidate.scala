package epic.coref

import epic.trees.Span

case class MentionCandidate(sentence: Int, span: Span, words: IndexedSeq[String]) extends Ordered[MentionCandidate] {
  def compare(that: MentionCandidate): Int = {
    if (this.sentence < that.sentence) -1
    else if (this.sentence > that.sentence) 1
    else if (this.span.start < that.span.start) -1
    else if (this.span.start > that.span.start) 1
    else if (this.span.end < that.span.end) -1
    else if (this.span.end > that.span.end) 1
    else 0
  }


  override def toString() = ("MC[" + sentence + ":" + span.start + "-" + span.end + ", " + words.mkString(" ") + "]")
}

object MentionCandidate {
  implicit val ordering = new Ordering[MentionCandidate] {
    def compare(x: MentionCandidate, y: MentionCandidate): Int = x compare y
  }
}

