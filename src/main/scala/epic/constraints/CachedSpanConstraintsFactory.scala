package epic.constraints

import epic.util.Cache

/**
  *
  *
  * @author dlwh
  */
@SerialVersionUID(1L)
class CachedSpanConstraintsFactory[W](factory: SpanConstraints.Factory[W]) extends SpanConstraints.Factory[W] {
   private val cache = new Cache[IndexedSeq[W], SpanConstraints]
   def constraints(w: IndexedSeq[W]): SpanConstraints = {
     cache.getOrElseUpdate(w, factory.constraints(w))
   }
 }
