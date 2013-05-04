package epic.sequences

/**
 *
 * @author dlwh
 */
sealed trait BIOETag[+L] {
  def labelOpt: Option[L]
}

object BIOETag {
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
