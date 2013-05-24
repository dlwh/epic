package epic.trees
package annotations

import epic.trees.BinarizedTree
import runtime.ScalaRunTime

/**
 *
 * @author dlwh
 */
trait TreeAnnotator[L, W, M] extends ((BinarizedTree[L], Seq[W])=>BinarizedTree[M]) with (TreeInstance[L, W]=>TreeInstance[M, W]) with Serializable {
  def apply(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[M]
  def apply(ti: TreeInstance[L, W]):TreeInstance[M, W] = {
    val newTree = apply(ti.tree, ti.words)
    ti.copy(tree=newTree)
  }

  final def andThen[N](other: TreeAnnotator[M, W, N]) = this map other

  def map[N](other: TreeAnnotator[M, W, N]) = {
    new ComposedAnnotator(this, other)
  }

  final def compose[N](other: TreeAnnotator[N, W, L]) = other map this

  override def toString() = this match {
    case x: Product =>  ScalaRunTime._toString(x)
    case _ => this.getClass.toString +"()"
  }

}

case class ComposedAnnotator[L, W, M, N](a: TreeAnnotator[L, W, M],
                                         b: TreeAnnotator[M, W, N]) extends TreeAnnotator[L, W, N] {

  def apply(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[N] = {
    b(a(tree, words),words)
  }

}

/** to be used with the config stuff, because I haven't figured out how to handle seqs yet... */
case class PipelineAnnotator[L, W](ann: Seq[TreeAnnotator[L, W, L]]) extends TreeAnnotator[L, W, L] {

  val pipeline = ann.filterNot(null eq).reduceLeft(_ andThen _)

  def apply(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[L] = {
    pipeline(tree, words)
  }

}

import TreeAnnotations._

class IdentityAnnotator[L, W] extends TreeAnnotator[L, W, L] {
  def apply(tree: BinarizedTree[L], words: Seq[W]) = tree
  override def toString() = "IdentityTransformation"
}

case class FilterAnnotations[W](toKeep: Set[Annotation]=Set.empty) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def this(keep: String*) = this(keep.map(FunctionalTag).toSet[Annotation])
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(l => l.copy(features = l.features.filter(toKeep)))
  }
}

case class StripAnnotations[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(l => l.copy(features = Set.empty))
  }
}

case class AddMarkovization[W](horizontal: Int=1, vertical: Int=2) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    annHorz(annVert(tree))
  }

  private def annVert(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = {
    def join(base: AnnotatedLabel, parent: AnnotatedLabel) = {
      base.copy(parents = base.parents :+ parent.label)
    }
    Trees.annotateParentsBinarized(tree, join, {(_:AnnotatedLabel).isIntermediate}, vertical)
  }

  private def annHorz(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = {
    def join(base: AnnotatedLabel, siblings: Seq[Either[AnnotatedLabel, AnnotatedLabel]]) = {
      val news = siblings.map {
        case Left(x) => Left(x.label)
        case Right(x) => Right(x.label)
      }

      base.copy(siblings = news)
    }
    Trees.addHorizontalMarkovization(tree, horizontal, join, {(_:AnnotatedLabel).isIntermediate})
  }

}


case class SplitAuxiliary() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {
  val beVerbs = Set("be", "is", "are", "were", "am", "was", "been", "being" )
  val hasVerbs = Set("has", "have", "had")

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = {
    tree.extend { t =>
      t match {
        case UnaryTree(label, NullaryTree(lbl2, _), chain, span) if label.baseLabel == lbl2.baseLabel =>
          val w = words(span.begin)
          if (beVerbs.contains(w.toLowerCase)) label.annotate(AuxBe).annotate(Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate(AuxHave).annotate(Aux)
          else label
        case NullaryTree(label, span) =>
          val w = words(span.begin)
          if (beVerbs.contains(w.toLowerCase)) label.annotate(AuxBe).annotate(Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate(AuxHave).annotate(Aux)
          else label
        case _ => t.label
      }
    }

  }

}

case class SplitVP() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {
  val activeVerbs = Set("VBZ", "VBD", "VBP", "MD")
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.extend { t =>
    if(t.label.baseLabel != "VP") {
      t.label
    } else {
      val headTag = HeadFinder.collins.lensed[AnnotatedLabel].findHeadTag(t)
      val base = headTag.baseLabel
      if (activeVerbs(base)) {
        t.label.annotate(VPisVBF)
      } else {
        t.label.annotate(VPisX(base))
      }
    }
  }

}


case class SplitIN[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    def rec(tree: BinarizedTree[AnnotatedLabel], root: String,
            parent: Option[String] = None,
            grandParent: Option[String] = None):BinarizedTree[AnnotatedLabel] = {
      val blbl = tree.label.baseLabel
      tree match {
        case tree@NullaryTree(lbl, span) if blbl == "IN" =>
        if(grandParent.isEmpty || grandParent.exists(_ == root) || parent.exists(_ == root)) {
          tree
        } else if (grandParent.exists(_(0) == 'N') && (parent.exists(s => s(0) == 'P' || s(0) == 'A'))) {
          tree.copy(lbl.annotate(IN_N), span)
        } else if (parent.exists(_(0) == 'Q') && (grandParent.exists(s => s(0) == 'N' || s.startsWith("ADJP")))) {
          tree.copy(lbl.annotate(IN_Q), span)
        } else if(grandParent.exists(_ == "S")) {
          if(parent.exists(_ == "SBAR")) {
            tree.copy(lbl.annotate(IN_SCC), span)
          } else {
            tree.copy(lbl.annotate(IN_SC), span)
          }
        } else {
          tree
        }
        case u @ UnaryTree(lbl, c, chain, span) =>
        if(blbl != "IN") {
          if(parent.exists(_ != blbl))
            u.copy(lbl, rec(c, root, Some(blbl), parent))
            else
              u.copy(lbl, rec(c, root, parent, grandParent))
            } else {
              val nc = rec(c, root, parent, grandParent)
              u.copy(nc.label, nc)
            }
            case BinaryTree(lbl, l,r, span) =>
            BinaryTree(lbl, rec(l, root, Some(blbl), parent), rec(r, root, Some(blbl), parent), span)
            case _ => tree
      }
    }
    rec(tree, tree.label.label)
  }

}

case class SplitPossNP[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = tree.extend { t =>
    if(t.label.baseLabel != "NP") t.label
    else {
      val headTag = HeadFinder.collins.lensed[AnnotatedLabel].findHeadTag(t)
      if (headTag.baseLabel == "POS") {
        t.label.annotate(NP_Possessive)
      } else {
        t.label
      }
    }
  }

}

case class AnnotateBaseNP[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    val root = tree.label.label
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel], Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> true
      case t@UnaryTree(lbl1, NullaryTree(lbl2, _), chain, span) if lbl1.baseLabel == lbl2.baseLabel =>
        t -> true
      case t@UnaryTree(lbl1, child, chain, span) =>
        val (newchild, ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate(BaseNP), newchild, chain, span) -> true
        } else if(lbl1.label == root) {
          UnaryTree(lbl1, newchild, chain, span) -> false
        } else {
          UnaryTree(lbl1, newchild, chain, span) -> false
        }
      case t@BinaryTree(lbl, lc, rc, span) =>
        val (newlc, lok) = rec(lc)
        val (newrc, rok) = rec(rc)
        if(lok && rok && lbl.baseLabel == "NP") {
          BinaryTree(lbl.annotate(BaseNP), newlc, newrc, span) -> true
        } else {
          BinaryTree(lbl, newlc, newrc, span) -> false
        }

    }
    rec(tree)._1

  }
}

case class AnnotateRightRecNP[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel], Boolean) = tree match {
      case t@UnaryTree(lbl1, child, chain, span) =>
        val (newchild, ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate(RRNP), newchild, chain, span) -> true
        } else {
          UnaryTree(lbl1, newchild, chain, span) -> (ok || lbl1.label == "NP")
        }
      case t@BinaryTree(lbl, lc, rc, span) =>
        val (newrc, rok) = rec(rc)
        if(rok && lbl.baseLabel == "NP") {
          val (newlc, _) = rec(lc)
          val lclc = annotateDownwards(newlc)
          BinaryTree(lbl.annotate(RRNP), lclc, newrc, span) -> true
        } else {
          val (newlc, _) = rec(lc)
          BinaryTree(lbl, newlc, newrc, span) -> (rok || (lbl.label == "NP"))
        }
      case _ => tree -> false
    }

    def annotateDownwards(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case t:NullaryTree[AnnotatedLabel] => t
      case UnaryTree(lbl, child, chain, span) if lbl.baseLabel == "NP" =>
        UnaryTree(lbl.annotate(RRNP), annotateDownwards(child), chain, span)
      case BinaryTree(lbl, lc, rc, span) if lbl.baseLabel == "NP" =>
        BinaryTree(lbl.annotate(RRNP), annotateDownwards(lc), annotateDownwards(rc), span)
      case _ => tree
    }
    rec(tree)._1
  }
}

case class MarkNonIdentityUnaries[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    val root = tree.label.label
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case BinaryTree(label, lc, rc, span) =>
        BinaryTree(label, rec(lc), rec(rc), span)
      case NullaryTree(label, span) => tree
      case u@UnaryTree(label, c, chain, span) =>
        if (label.label != root && label.label != c.label.label)
        UnaryTree(label.annotate(RealUnary), rec(c), chain, span)
        else
          u.copy(child = rec(c))
    }

    rec(tree)
  }
}

case class MarkExternalUnaries[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    val root = tree.label.label
    val shouldAnnotate = Set("RB", "DT")
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
     case BinaryTree(label, lc, rc, span) => BinaryTree(label, rec(lc), rec(rc), span)
     case NullaryTree(label, span) => tree
     case u@UnaryTree(label, c, chain, span)  =>
       if (label.label != root && label.label != c.label.label && shouldAnnotate(c.label.label))
         u.copy(child=rec(c).relabelRoot(_.annotate(ExternalUnary)))
       else
         u.copy(child=rec(c))
    }

    rec(tree)
  }
}

trait MarkDominates[W] extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  protected def dominates(x: Tree[AnnotatedLabel]):Boolean
  protected def sym: String
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = tree.extend { t =>
    if(t eq tree) t.label
    else if(dominates(t)) t.label.annotate(Dom(sym))
    else t.label
  }
}

case class DominatesV[W]() extends MarkDominates[W] {
  protected def dominates(x: Tree[AnnotatedLabel]):Boolean = x.leaves.exists { t => t.label.label.startsWith("V") || t.label.label.startsWith("MD")}
  def sym = "V"
}

