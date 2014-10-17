package epic

/**
 * TODO
 *
 * @author dlwh
 **/
package object preprocess {
  def tokenize(sentence: String): IndexedSeq[String] = TreebankTokenizer(sentence)

}
