package epic.constraints

import epic.util.Cache

/**
 *
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CachedLabeledSpanConstraintsFactory[L, W](factory: LabeledSpanConstraints.Factory[L, W]) extends LabeledSpanConstraints.Factory[L, W] {
  private val cache = new Cache[IndexedSeq[W], LabeledSpanConstraints[L]]
  def constraints(w: IndexedSeq[W]): LabeledSpanConstraints[L] = {
    cache.getOrElseUpdate(w, factory.constraints(w))
  }
}
