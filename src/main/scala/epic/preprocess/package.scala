package epic

import java.io.File
import java.net.URL

/**
 * TODO
 *
 * @author dlwh
 **/
package object preprocess {
  def tokenize(sentence: String): IndexedSeq[String] = TreebankTokenizer(sentence)

  def loadContent(url: URL): String = TextExtractor.extractText(url)
  
  def preprocess(url: URL):IndexedSeq[IndexedSeq[String]] = {
    preprocess(loadContent(url))
  }

  def preprocess(text: String): IndexedSeq[IndexedSeq[String]] = {
    _seg(text).map(tokenize)
  }

  def preprocess(file: File): IndexedSeq[IndexedSeq[String]] = {
    preprocess(file.toURI.toURL)
  }

  private lazy val _seg = MLSentenceSegmenter.bundled().get

}
