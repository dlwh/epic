import epic.constraints.SpanConstraints
import epic.util.Has

/**
 *
 * @author dlwh
 */
package object epic {
  type HasSpanConstraints[T] = Has[SpanConstraints]#R[T]
  type HasWords[T] = Has[IndexedSeq[String]]#R[T]
  def iCanHas[WhatIHave]:Has[WhatIHave] = new Has[WhatIHave]{}
}
