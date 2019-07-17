package epic.parser

import epic.trees._
import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class GenerativeParserTest extends FunSuite {
  test("Max's bug") {
    val (tree,words) = Tree.fromString(
      """
        |(TOP (S
        |      (NP
        |        (NP
        |          (RB (RB A) )
        |          (PDT (PDT QQQ) )
        |          (DT (DT ZZZZ) ))
        |        (SBAR
        |         (SBAR
        |          (WHNP (WP which) )
        |          (S (VBD qqqed) ))))
        |       (VP
        |        (VBP (VBP vbes) )
        |         (NP
        |          (DT (DT the) )
        |          (NNS (NNS dogs) )))
        |      (. (. .) )))
        |
      """.stripMargin)
    assert(words.length == 9)
    var tp = new StandardTreeProcessor().apply(tree.map(AnnotatedLabel(_)))
    tp = UnaryChainCollapser.collapseUnaryChains(tp, keepChains = false)
    val ti = TreeInstance("...", tp, words)
    val ann = GenerativeParser.defaultAnnotator().apply(ti)
    println(ann.tree)
    val counts = GenerativeParser.extractCounts(IndexedSeq(ann))
    println(counts)
    val s_sbar = AnnotatedLabel("S", parents = IndexedSeq("SBAR"))
    val vbd_s = AnnotatedLabel("VBD", parents = IndexedSeq("S"))
    assert(counts._3(s_sbar, UnaryRule(s_sbar, vbd_s, IndexedSeq.empty)) == 1.0)
  }

}
