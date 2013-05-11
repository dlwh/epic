import epic.constraints.SpanConstraints
import epic.util.Has

/**
 *
 * @author dlwh
 */
package object epic {
  type HasSpanConstraints[T] = Has[SpanConstraints]#R[T]
  def iCanHas[WhatIHave]:Has[WhatIHave] = new Has[WhatIHave]{}
}
