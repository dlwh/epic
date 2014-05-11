package epic.sequences

import epic.util.ProcessTextMain

/**
 * Simple class that reads in a bunch of files and tags them. Output is dumped to standard out.
 * @author dlwh
 */
object TagText extends ProcessTextMain[CRF[Any, String], TaggedSequence[Any, String]] {


  override def render(model: CRF[Any, String], ann: TaggedSequence[Any, String], tokens: IndexedSeq[String]): String = ann.render

  override def annotate(model: CRF[Any, String], text: IndexedSeq[String]): TaggedSequence[Any, String] = {
    model.bestSequence(text)
  }


}
