package epic.trees

import org.mapdb.CC
import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class TraceToSlashCategoryConverterTest extends FunSuite {

  test("Double trace test") {
    val str = """(TOP (S
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
    val remTree = new TraceToSlashCategoryConverter().apply(annTree)

    assert(words == "What we want to do is have the evidence speak for itself .".split(" ").toIndexedSeq)
    assert(remTree.map(_.clearFeatures).preorder.map(_.label).toSeq startsWith  Seq(
      AnnotatedLabel("TOP"), AnnotatedLabel("S"),
      AnnotatedLabel("SBAR"),
        AnnotatedLabel("WHNP", index = 2), AnnotatedLabel("WP"),
        AnnotatedLabel("S", siblings = IndexedSeq(Left("NP"))),
        AnnotatedLabel("NP", index = 1), AnnotatedLabel("PRP"),
        AnnotatedLabel("VP", siblings = IndexedSeq(Left("NP"), Left("NP"))),
          AnnotatedLabel("VBP"),
          AnnotatedLabel("S", siblings = IndexedSeq(Left("NP"), Left("NP"))),
            AnnotatedLabel("NP"),
            AnnotatedLabel("VP", siblings = IndexedSeq(Left("NP"))),
              AnnotatedLabel("TO"),
              AnnotatedLabel("VP", siblings = IndexedSeq(Left("NP"))),
                AnnotatedLabel("VB"),
                AnnotatedLabel("NP"),
      AnnotatedLabel("VP")
    ), remTree.render(words))
  }

  test("Simple test, trace on word") {
    val (tree, words) = Tree.fromString("""(TOP (S (CC And)
      (NP-SBJ-1 (PRP we))
      (VP (VBP 're)
    (VP (VBG going)
      (S (NP-SBJ (-NONE- *-1))
    (VP (TO to)
      (VP (VB get)
        (VP (VBN started)
          (ADVP-LOC (RB here))))))))
    (. .)))""")
    val annTree = tree.map(AnnotatedLabel.parseTreebank(_))
    val remTree = new TraceToSlashCategoryConverter().apply(annTree)
    assert(remTree.children.head.children(2).label.siblings.nonEmpty, remTree.render(words))
  }


  test("Simple test, trace on cat") {
    val (tree, words) = Tree.fromString("""(TOP (S (CC And)
      (NP-SBJ-1 (PRP we))
      (VP (VBP 're)
    (VP (VBG going)
      (S (NP-SBJ-1 (-NONE- *))
    (VP (TO to)
      (VP (VB get)
        (VP (VBN started)
          (ADVP-LOC (RB here))))))))
    (. .)))""")
    val annTree = tree.map(AnnotatedLabel.parseTreebank(_))
    val remTree = new TraceToSlashCategoryConverter().apply(annTree)
    assert(remTree.children.head.children(2).label.siblings.nonEmpty, remTree.render(words))
  }

}
