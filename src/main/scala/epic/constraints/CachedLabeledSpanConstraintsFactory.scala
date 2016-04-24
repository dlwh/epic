package epic.constraints

import epic.util.CacheBroker

/**
 * A cached version of [[epic.constraints.LabeledSpanConstraints.Factory]].
 * Uses the [[epic.util.CacheBroker]] infrastructure
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CachedLabeledSpanConstraintsFactory[L, W](factory: LabeledSpanConstraints.Factory[L, W], name: String)(implicit broker: CacheBroker) extends LabeledSpanConstraints.Factory[L, W] {
  private val cache = broker.make[IndexedSeq[W], LabeledSpanConstraints[L]](name)
  def constraints(w: IndexedSeq[W]): LabeledSpanConstraints[L] = {
    cache.getOrElseUpdate(w, factory.constraints(w))
  }
}
