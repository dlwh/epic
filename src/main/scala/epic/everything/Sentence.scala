package epic.everything

/**
 * represents an annotation ontonotes sentence. Doesn't include raw sentence, for now.
 *
 * @author dlwh
 */
case class Sentence(docId: String, sentId: Int,
                   words: IndexedSeq[String],
                   annotations: OntoAnnotations) extends Example[OntoAnnotations,Seq[String]] {
  def id = docId +":"+sentId
  def features = words
  def label = annotations

  def tree = annotations.tree
  def ner = annotations.ner
  def coref = annotations.coref
}

case class DSpan(doc: String, sent: Int, begin: Int, end: Int) {
  def span = Span(begin, end)

  override def toString = doc + ":" + sent + ":" + begin + "-" + end
}