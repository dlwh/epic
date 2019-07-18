package epic.sequences

import epic.util.ProcessTextMain
import epic.models.PosTagSelector
import epic.trees.AnnotatedLabel

/**
 * Simple class that reads in a bunch of files and tags them. Output is dumped to standard out.
 * @author dlwh
 */
object TagText extends ProcessTextMain[CRF[AnnotatedLabel, String], TaggedSequence[AnnotatedLabel, String]] {

  override def render(model: CRF[AnnotatedLabel, String], ann: TaggedSequence[AnnotatedLabel, String], tokens: IndexedSeq[String]): String = ann.render

  override def annotate(model: CRF[AnnotatedLabel, String], text: IndexedSeq[String]): TaggedSequence[AnnotatedLabel, String] = {
    model.bestSequence(text)
  }

  override def classPathLoad(language: String): CRF[AnnotatedLabel, String] = {
    PosTagSelector.loadTagger(language).get
  }
}
