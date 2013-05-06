package epic.sequences

/**
 * A BIOETag is a tag that us to represent [[epic.sequences.Segmentation]]s as [[epic.sequences.TaggedSequence]]s.
 * It includes Begins, Inside, Outside, and End tags. Sometimes we just use IO, or BIO.
 * @author dlwh
 */
sealed trait BIOETag[+L] {
  def labelOpt: Option[L]
}

object BIOETag {

  /**
   * Marker for all tags that aren't "Outside"
   * @tparam L
   */
  sealed trait InsideSpan[L] extends BIOETag[L]                    {
    def label: L
    def labelOpt = Some(label)
  }
  case class Begin[L](label: L) extends BIOETag[L] with InsideSpan[L]
  case class End[L](label: L) extends BIOETag[L] with InsideSpan[L]
  case class Inside[L](label: L) extends BIOETag[L]  with InsideSpan[L]
  case object Outside extends BIOETag[Nothing] {
    def labelOpt = None
  }
}
