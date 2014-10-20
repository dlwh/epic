package epic

/**
 * TODO
 *
 * @author dlwh
 **/
package object preprocess {
  def tokenize(sentence: String): IndexedSeq[String] = TreebankTokenizer(sentence).map(t => sentence.substring(t.begin, t.end))
}
