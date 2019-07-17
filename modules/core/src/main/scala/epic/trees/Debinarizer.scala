package epic.trees

import java.io.ObjectStreamException

/**
 * Class that turns BinarizedTrees into normal trees. Should
 * replace unary chains in addition to removing intermediates.
 *
 * @author dlwh
 **/
trait Debinarizer[L] extends (BinarizedTree[L] => Tree[L]) with Serializable

object Debinarizer {
  @SerialVersionUID(1L)
  implicit object AnnotatedLabelDebinarizer extends Debinarizer[AnnotatedLabel] {

    def apply(t: BinarizedTree[AnnotatedLabel]): Tree[AnnotatedLabel] = {
      Trees.debinarize(replaceUnaries(t), {(_: AnnotatedLabel).isIntermediate}).map(_.baseAnnotatedLabel)
    }

    def replaceUnaries(t: Tree[AnnotatedLabel]): Tree[AnnotatedLabel] = t match {
      case UnaryTree(a, child, chain, span) if a.label == child.label.label && chain.isEmpty =>
        replaceUnaries(child)
      case UnaryTree(a, child, chain, span) =>
        val deunaried = replaceUnaries(child).asInstanceOf[BinarizedTree[AnnotatedLabel]]
        val withChain = chain.foldRight(deunaried){ (label, child) =>
          UnaryTree(AnnotatedLabel(label), child, IndexedSeq.empty, span)
        }
        UnaryTree(a,withChain, IndexedSeq.empty, t.span)
      case t@BinaryTree(a, lchild, rchild, span) =>
        BinaryTree(a,
          replaceUnaries(lchild).asInstanceOf[BinarizedTree[AnnotatedLabel]],
          replaceUnaries(rchild).asInstanceOf[BinarizedTree[AnnotatedLabel]], t.span)
      case _ => t
    }

  }

  @SerialVersionUID(1L)
  implicit object StringDebinarizer extends Debinarizer[String] {

    def apply(t: BinarizedTree[String]): Tree[String] = {
      Trees.debinarize(Trees.deannotate(replaceUnaries(t)))
    }

    def replaceUnaries(t: Tree[String]): Tree[String] = t match {
      case UnaryTree(a, child, chain, span) if a == child.label && chain.isEmpty =>
        replaceUnaries(child)
      case UnaryTree(a, child, chain, span) =>
        val deunaried = replaceUnaries(child).asInstanceOf[BinarizedTree[String]]
        val withChain = chain.foldRight(deunaried){ (label, child) =>
          UnaryTree(label, child, IndexedSeq.empty, span)
        }
        UnaryTree(a,withChain, IndexedSeq.empty, t.span)
      case t@BinaryTree(a, lchild, rchild, span) =>
        BinaryTree(a,
          replaceUnaries(lchild).asInstanceOf[BinarizedTree[String]],
          replaceUnaries(rchild).asInstanceOf[BinarizedTree[String]], t.span)
      case _ => t
    }
  }

}
