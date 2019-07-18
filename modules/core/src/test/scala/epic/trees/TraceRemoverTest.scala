package epic.trees

import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class TraceRemoverTest extends FunSuite {
  test("Double trace test") {
    val str =
      """(TOP (S
        |    (SBAR-NOM-SBJ
        |      (WHNP-2 (WP What) )
        |      (S
        |        (NP-SBJ-1 (PRP we) )
        |        (VP
        |          (VBP want)
        |          (S
        |            (NP-SBJ (-NONE- *PRO*-1) )
        |            (VP
        |              (TO to)
        |              (VP
        |                (VB do)
        |                (NP (-NONE- *T*-2) )))))))
        |    (VP
        |      (VBZ is)
        |      (S-PRD
        |        (NP-SBJ (-NONE- *PRO*) )
        |        (VP
        |          (VB have)
        |          (S
        |            (NP-SBJ (DT the)  (NN evidence) )
        |            (VP
        |              (VB speak)
        |              (PP-CLR
        |                (IN for)
        |                (NP (PRP itself) )))))))
        |     (. /.) ))""".stripMargin

    val (tree, words) = Tree.fromString(str)
    val annTree = tree.map(AnnotatedLabel.parseTreebank(_))
    val (remTree) = new TraceRemover[AnnotatedLabel, String](_.label == "-NONE-").apply(annTree)

    assert(words == "What we want to do is have the evidence speak for itself .".split(" ").toIndexedSeq)
    assert(remTree.map(_.clearFeatures).preorder.map(_.label).toSeq startsWith Seq(
      AnnotatedLabel("TOP"), AnnotatedLabel("S"),
      AnnotatedLabel("SBAR"),
      AnnotatedLabel("WHNP", index = 2), AnnotatedLabel("WP"),
      AnnotatedLabel("S"),
      AnnotatedLabel("NP", index = 1), AnnotatedLabel("PRP"),
      AnnotatedLabel("VP"),
      AnnotatedLabel("VBP"),
      AnnotatedLabel("S"),
      AnnotatedLabel("VP"),
      AnnotatedLabel("TO"),
      AnnotatedLabel("VP"),
      AnnotatedLabel("VB"),
      AnnotatedLabel("VP")
    )
    )

  }

}
