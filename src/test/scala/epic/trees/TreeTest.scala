package epic.trees

import org.scalatest.FunSuite
import epic.trees.Trees.Zipper

/**
 *
 * @author dlwh
 */
class TreeTest extends FunSuite {
  test("Zipper Iteration") {
      val (tree,words) = Tree.fromString("""
    ( (S
        (NP-SBJ (DT The) (JJ top) (NN money) (NNS funds) )
        (VP (VBP are)
          (ADVP-TMP (RB currently) )
          (VP (VBG yielding)
            (NP
              (QP (RB well) (IN over) (CD 9) )
              (NN %) )))
        (. .) ))
    """)

    val processed = new StandardTreeProcessor().apply(tree.map(AnnotatedLabel.parseTreebank))
    val deunaried = UnaryChainCollapser.collapseUnaryChains(processed)
    val zip = Trees.Zipper(deunaried)
    val zippers = zip.iterator.toIndexedSeq
    val trees = zippers.map(_.tree)
    assert(trees.head.label.label === "TOP")
    assert(trees(1).label.baseLabel === "S", trees.head)
    assert(trees(2).label.label === "NP")
    assert(trees(3).label.baseLabel === "NP", trees.head)
    assert(trees(trees.length-1).label.label === ".")
    assert(trees.last.label.label === ".")
    assert(zippers.forall(_.upToRoot == zippers.head))
  }

  test("Zipper Next on a nullarytree with a right sibling behaves correctly") {
    val begin = 0
    val split = 4
    val end = 10
    val zipper = Zipper(BinaryTree(1,
                NullaryTree(2, Span(begin,split)),
                NullaryTree(3, Span(split, end)), Span(begin, end))).down.get
    assert(zipper.next.nonEmpty,zipper)
    assert(zipper.next.get.label === 3)
  }

}
