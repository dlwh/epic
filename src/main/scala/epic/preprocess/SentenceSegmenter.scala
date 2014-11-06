package epic.preprocess

import epic.slab._
import epic.slab.Sentence

/**
 * AnalysisFunction01(SentenceSegmenter) will create an
 * AnalysisFunction01 ready to use.
 */
trait SentenceSegmenter extends (String => Vector[Sentence]) with AnalysisFunction01[String, Sentence] {
  override def toString = getClass.getName

  def apply(sentence: String): Vector[Sentence]
  def strings(document: String): Vector[String] = {
    val sentences = apply(document)
    sentences.map(s => document.substring(s.begin, s.end))
  }
}
