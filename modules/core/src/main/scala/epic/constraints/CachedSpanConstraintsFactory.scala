package epic.constraints

import epic.util.CacheBroker

/**
 * A cached version of [[epic.constraints.SpanConstraints.Factory]].
 * Uses the [[epic.util.CacheBroker]] infrastructure
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CachedSpanConstraintsFactory[W](factory: SpanConstraints.Factory[W], name: String)(implicit broker: CacheBroker) extends SpanConstraints.Factory[W] {
   private val cache = broker.make[IndexedSeq[W], SpanConstraints](name)
   def constraints(w: IndexedSeq[W]): SpanConstraints = {
     cache.getOrElseUpdate(w, factory.constraints(w))
   }
 }
