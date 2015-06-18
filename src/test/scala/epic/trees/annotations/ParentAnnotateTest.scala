package epic.trees.annotations

import org.scalatest.FunSuite
import epic.trees.{StandardTreeProcessor, AnnotatedLabel, Trees, Tree}
import epic.trees.Trees.Zipper

/**
 * TODO
 *
 * @author dlwh
 **/
class ParentAnnotateTest extends FunSuite {

  test("basic test") {
    val (tree, words) = Tree.fromString("( (S (NP (DT A) (NN record) (NN date)) (VP (VBZ has) (RB n't) (VP (VBN been) (VP (VBN set)))) (. .)))")
    val binarized = StandardTreeProcessor().apply(tree.map(AnnotatedLabel.parseTreebank)).map(_.baseAnnotatedLabel)
    val tree2 = ParentAnnotate(2).apply(binarized, words)

    assert(tree2.preorder.filter(_.label.label == "S").next().label.parents === IndexedSeq("TOP"))
    assert(tree2.preorder.filter(_.label.label == "NP").next().label.parents === IndexedSeq("S", "TOP"))
    assert(tree2.preorder.filter(_.label.label == "VBZ").next().label.parents === IndexedSeq("VP", "S"))
    assert(tree2.preorder.filter(_.label.label == "VBN").next().label.parents === IndexedSeq("VP", "VP"))
    assert(tree2.preorder.filter(_.label.label == "NN").next().label.parents === IndexedSeq("NP", "S"))

    assert(tree2.children.head.label.parents === IndexedSeq("TOP")) // the S
    assert(tree2.children.head.label.parents === IndexedSeq("TOP"))

    println(tree2)

  }

}
