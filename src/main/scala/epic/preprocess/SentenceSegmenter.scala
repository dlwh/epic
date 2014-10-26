package epic.preprocess

import epic.slab._
import epic.slab.Sentence

/**
 *
 * @author dlwh
 */
class SentenceSegmenter extends (String => Vector[Sentence]) {
  override def toString = getClass.getName

  def apply(sentence: String): Vector[Token]
  def strings(document: String): Vector[String] = {
    val sentences = apply(document)
    sentences.map(s => document.substring(s.begin, s.end))
  }
}
