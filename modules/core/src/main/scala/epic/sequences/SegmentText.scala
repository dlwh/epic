package epic.sequences

import epic.models.NerSelector
import epic.util.ProcessTextMain

/**
 * Simple class that reads in a bunch of files and parses them. Output is dumped to standard out.
 * @author dlwh
 */
object SegmentText extends ProcessTextMain[SemiCRF[Any, String], Segmentation[Any, String]] {

  override def render(model: SemiCRF[Any, String], ann: Segmentation[Any, String], tokens: IndexedSeq[String]): String = {
    ann.render
  }

  override def annotate(model: SemiCRF[Any, String], text: IndexedSeq[String]): Segmentation[Any, String] = {
    model.bestSequence(text)
  }

  override def classPathLoad(language: String): SemiCRF[Any, String] = {
    NerSelector.loadNer(language).get
  }
}
