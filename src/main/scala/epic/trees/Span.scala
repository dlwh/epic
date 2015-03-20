package epic

import spire.syntax.cfor

package object trees {
  type Span = epic.slab.Span
  val Span = epic.slab.Span

  implicit class SpanOps(span: Span) {
    @inline
    def foreach(f: Int=>Unit) = {
      cfor.cfor(span.begin)(_ < span.end, _ +1) { f }
    }
  }
}
