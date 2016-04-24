package epic.constraints

import breeze.util.Index
import epic.constraints.LabeledSpanConstraints.PromotedSpanConstraints

/**
 *
 *
 * @author dlwh
 */
object LongSpanConstraints {
  case class Factory[L](maxSimpleLength: Int = 30, okWords: Set[String]) extends ChartConstraints.Factory[L, String] {
    def constraints(w: IndexedSeq[String]): ChartConstraints[L] = {
      val spans = new SpanConstraints {
        val oks = w.map(ww => okWords(ww) || !ww.head.isLetterOrDigit)
        def maxSpanLengthStartingAt(begin: Int): Int = w.length - begin
        def maxSpanLengthForLabel(label: Int): Int = w.length
        def decode(labelIndex: Index[L]): String = "..."
        def isAllowedLabeledSpan(begin: Int, end: Int, label: Int): Boolean = isAllowedSpan(begin, end)
        def isAllowedSpan(begin: Int, end: Int): Boolean = (
          end - begin <= maxSimpleLength
            || end == w.length
            || begin == 0
            || oks(begin)
            || oks(begin-1)
            || oks(end)
            || oks(end-1)
          )
      }
      new ChartConstraints(PromotedSpanConstraints(spans), PromotedSpanConstraints(spans))
    }
  }
}
