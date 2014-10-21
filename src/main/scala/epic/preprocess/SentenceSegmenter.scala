package epic.preprocess

import epic.slab._
import epic.slab.Sentence

/**
 *
 * @author dlwh
 */
trait SentenceSegmenter extends SourceAnalysisFunction[String, Sentence] with (String => Vector[Sentence]) {
  override def toString = getClass.getName

  def apply(content: String): Vector[Sentence]
}
