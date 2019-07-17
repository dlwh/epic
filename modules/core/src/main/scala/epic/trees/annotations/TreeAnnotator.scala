package epic.trees
package annotations

import epic.parser.projections.ProjectionIndexer

import scala.runtime.ScalaRunTime

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

  def localized[C](proj: ProjectionIndexer[C, M]) = {(ti: BinarizedTree[L], w: IndexedSeq[W]) =>
    this(ti, w).map(proj.localize)
  }

  def latent = {(ti: BinarizedTree[L], w: IndexedSeq[W]) =>
    this(ti, w).map(IndexedSeq(_))
  }

}

object TreeAnnotator {
  def identity[L, W]:TreeAnnotator[L, W, L] = new IdentityAnnotator[L, W]
}

case class ComposedAnnotator[L, W, M, N](a: TreeAnnotator[L, W, M],
                                         b: TreeAnnotator[M, W, N]) extends TreeAnnotator[L, W, N] {

  def apply(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[N] = {
    b(a(tree, words),words)
  }

}

case class PipelineAnnotator[L, W](ann: Seq[TreeAnnotator[L, W, L]]) extends TreeAnnotator[L, W, L] {

  def apply(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[L] = {
    ann.foldLeft(tree)((b,a) => a(b, words))
  }

}

import epic.trees.annotations.TreeAnnotations._

class IdentityAnnotator[L, W] extends TreeAnnotator[L, W, L] with Serializable {
  def apply(tree: BinarizedTree[L], words: Seq[W]) = tree
  override def toString() = "IdentityTransformation"
}

/**
 * @param toKeep the annotations we want to keep
 * @tparam W
 */
case class FilterAnnotations[W](toKeep: Set[Annotation]=Set.empty) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(l => l.copy(features = l.features.filter(toKeep)))
  }
}

/**
 * Removes all features from the [[epic.trees.AnnotatedLabel]]
 * @tparam W
 */
case class StripAnnotations[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(l => l.clearFeatures)
  }
}

/**
 * Removes all features from the [[epic.trees.AnnotatedLabel]]
 * @tparam W
 */
case class Xbarize[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(l => l.baseAnnotatedLabel)
  }
}

case class Markovize[W](horizontal: Int=0, vertical: Int=2) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  val vert = new ParentAnnotate[W](vertical)
  val horz = new MarkovizeSiblings[W](horizontal)
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    horz(vert(tree, words), words)
  }
}

case class ParentAnnotate[W](order: Int = 0,  skipPunctTags: Boolean = true) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    if (order == 0) {
      tree
    } else {
      def join(base: AnnotatedLabel, parent: Seq[AnnotatedLabel]) = {
        base.copy(parents = parent.map(_.label).toIndexedSeq)
      }
      try  {
        Trees.annotateParentsBinarized(tree, join, {(_:AnnotatedLabel).isIntermediate}, {(l:Tree[AnnotatedLabel])=> l.label.label.isEmpty || (l.label.label.head != '@' && !l.label.label.head.isLetterOrDigit)}, order)
      } catch {
        case ex: AssertionError =>
          throw new RuntimeException(s"While handling $words", ex)
      }
    }
  }
}

case class ParentAnnotatePosTags[W](order: Int = 1,  skipPunctTags: Boolean = true) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    if (order == 0) {
      tree
    } else {
      def join(base: AnnotatedLabel, parent: Seq[AnnotatedLabel]) = {
        base.copy(parents = parent.map(_.label).toIndexedSeq)
      }
      Trees.annotateParentsBinarized(tree, join, {(_:AnnotatedLabel).isIntermediate}, {(l:Tree[AnnotatedLabel])=> !(l.isLeaf || l.children.length == 1 && l.children.head.label.label == l.label.label && l.span.length == 1)  || l.label.label.isEmpty || (l.label.label.head != '@' && !l.label.label.head.isLetterOrDigit)}, order)
    }
  }
}

case class ForgetHeadTag[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree.map(_.copy(headTag=None))
  }
}

case class MarkovizeSiblings[W](order: Int=0) extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    if (order == 0) tree.map {l => l.copy(siblings = IndexedSeq.empty)}
    else tree.map { l => l.copy(siblings = l.siblings.takeRight(order)) }
  }

  /*
    def join(base: AnnotatedLabel, siblings: IndexedSeq[Either[AnnotatedLabel, AnnotatedLabel]]) = {
      val news = siblings.map {
        case Left(x) => Left(x.label)
        case Right(x) => Right(x.label)
      }

      base.copy(siblings = news)
    }
    Trees.addHorizontalMarkovization(tree, horizontal, join, {(_:AnnotatedLabel).isIntermediate})
  }
  */

}
/**
 * Marks verb tags based on the auxiliary
 */
case class SplitAuxiliary() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {
  val beVerbs = Set("be", "is", "are", "were", "am", "was", "been", "being" )
  val hasVerbs = Set("has", "have", "had")

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = {
    tree.extend {
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
      case t => t.label
    }
  }

}

case class Punct(word: String) extends Annotation

/**
 * Marks tags that immediately dominate punctuation that don't include that punctuation
 */
case class SplitPunct() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], w: Seq[String]) = {
    val words = w
    tree.extend {
      case UnaryTree(label, NullaryTree(lbl2, _), chain, span) if label.baseLabel == lbl2.baseLabel =>
        val w = words(span.begin)

        if (w.forall(!_.isLetterOrDigit) && label.baseLabel != w) label.annotate(Punct(w))
        else if (w.matches("-[LR].B-") && label.baseLabel != w) label.annotate(Punct(w))
        else label
      case NullaryTree(label, span) =>
        val w = words(span.begin)
        if (w.forall(!_.isLetterOrDigit) && label.baseLabel != w) label.annotate(Punct(w))
        else label
      case t => t.label
    }

  }

}

/**
 * Marks VPs based on the kind of verb that it has.
 */
case class SplitVP() extends TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] {
  val finiteVerbs = Set("VBZ", "VBD", "VBP", "MD")
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.extend { t =>
    if (t.label.baseLabel != "VP") {
      t.label
    } else {
      val headTag = HeadFinder.collins.lensed[AnnotatedLabel].findHeadTag(t)
      val base = headTag.baseLabel
      if (finiteVerbs(base)) {
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
        if (grandParent.isEmpty || grandParent.contains(root) || parent.contains(root)) {
          tree
        } else if (grandParent.exists(_(0) == 'N') && parent.exists(s => s(0) == 'P' || s(0) == 'A')) {
          tree.copy(lbl.annotate(IN_N), span)
        } else if (parent.exists(_(0) == 'Q') && grandParent.exists(s => s(0) == 'N' || s.startsWith("ADJP"))) {
          tree.copy(lbl.annotate(IN_Q), span)
        } else if (grandParent.contains("S")) {
          if (parent.contains("SBAR")) {
            tree.copy(lbl.annotate(IN_SCC), span)
          } else {
            tree.copy(lbl.annotate(IN_SC), span)
          }
        } else {
          tree
        }
        case u @ UnaryTree(lbl, c, chain, span) =>
        if (blbl != "IN") {
          if (parent.exists(_ != blbl))
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
    if (t.label.baseLabel != "NP") t.label
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

/**
 * A BaseNP dominates only preterminals, or @NPs that are also base nps.
 * @tparam W
 */
case class AnnotateBaseNP[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel], Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> true
      case t@UnaryTree(lbl1, NullaryTree(lbl2, _), chain, span) if lbl1.baseLabel == lbl2.baseLabel =>
        t -> true
      case t@UnaryTree(lbl1, child, chain, span) =>
        val (newchild, ok) = rec(child)
        if (lbl1.baseLabel == "NP" && (ok || newchild.label.hasAnnotation(BaseNP))) {
          UnaryTree(lbl1.annotate(BaseNP), newchild, chain, span) -> lbl1.isIntermediate
        } else {
          UnaryTree(lbl1, newchild, chain, span) -> false
        }
      case t@BinaryTree(lbl, lc, rc, span) =>
        val (newlc, lok) = rec(lc)
        val (newrc, rok) = rec(rc)
        if (lok && rok && lbl.baseLabel == "NP") {
          BinaryTree(lbl.annotate(BaseNP), newlc, newrc, span) -> lbl.isIntermediate
        } else {
          BinaryTree(lbl, newlc, newrc, span) -> false
        }
    }
    rec(tree)._1

  }
}

/**
 * An NP or an @NP is Right Recursive if
 *    1) its right child is an NP
 * or 2) if its right child is @NP and it is RRNP
 * or 3) it is a unary and its child is RRNP
 * @tparam W
 */
case class AnnotateRightRecNP[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    // boolean is whether or not that child is either an NP, or an @NP[RightRecNP]
    def rec(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case t@UnaryTree(lbl1, child, chain, span) =>
        val newchild = rec(child)
        if (lbl1.baseLabel == "NP" && newchild.label.hasAnnotation(RightRecNP)) {
          UnaryTree(lbl1.annotate(RightRecNP), newchild, chain, span)
        } else {
          UnaryTree(lbl1, newchild, chain, span)
        }
      case t@BinaryTree(lbl, lc, rc, span) =>
        val newrc = rec(rc)
        val isRightRec = lbl.baseLabel == "NP" && (newrc.label.label == "NP" || (newrc.label.label == "@NP" && newrc.label.hasAnnotation(RightRecNP)))
        val newlc = rec(lc)
        if (isRightRec) {
          val lclc = annotateDownwards(newlc)
          BinaryTree(lbl.annotate(RightRecNP), lclc, newrc, span)
        } else {
          BinaryTree(lbl, newlc, newrc, span)
        }
      case _ => tree
    }

    // annotate all intermediate @NPs as RightRecNP
    def annotateDownwards(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case _ if !tree.label.isIntermediate => tree
      case t:NullaryTree[AnnotatedLabel] => t
      case UnaryTree(lbl, child, chain, span) if lbl.label == "@NP" =>
        UnaryTree(lbl.annotate(RightRecNP), annotateDownwards(child), chain, span)
      case BinaryTree(lbl, lc, rc, span) if lbl.label == "@NP" =>
        BinaryTree(lbl.annotate(RightRecNP), if (lc.label.isIntermediate) annotateDownwards(lc) else lc, if (rc.label.isIntermediate) annotateDownwards(rc) else rc, span)
      case _ => tree
    }
    rec(tree)
  }
}

/**
 * Marks if an XP immediately dominates a CC or if an @XP that recursively dominates a CC.
 * @tparam W
 */
case class AnnotateDomCC[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    // boolean is whether or not that child is either an NP, or an @NP[RightRecNP]
    def rec(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case t@UnaryTree(lbl1, child, chain, span) =>
        val newchild = rec(child)
        if (newchild.label.hasAnnotation(DomCCLeft)) {
          UnaryTree(lbl1.annotate(DomCCLeft), newchild, chain, span)
        } else if (newchild.label.hasAnnotation(DomCCRight)) {
          UnaryTree(lbl1.annotate(DomCCRight), newchild, chain, span)
        } else {
          UnaryTree(lbl1, newchild, chain, span)
        }
      case t@BinaryTree(lbl, lc, rc, span) =>
        val newrc = rec(rc)
        val newlc = rec(lc)
        val domsCCR = newrc.label.label == "CC" || (newrc.label.isIntermediate && newrc.label.hasAnnotation(DomCCRight))
        val domsCCL = newlc.label.label == "CC" || (newlc.label.isIntermediate && newlc.label.hasAnnotation(DomCCLeft))
        val sym = if (domsCCL) lbl.annotate(DomCCLeft) else if (domsCCR) lbl.annotate(DomCCRight) else lbl
        BinaryTree(sym, newlc, newrc, span)
      case _ => tree
    }
    rec(tree)
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
        if (label.label != root && label.baseLabel != c.label.baseLabel)
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

// For sentiment: we don't want to suffer from parent annotation at the root when all other
// symbols are symmetric, so we'd rather have something generic like 3^3 rather than 3^TOP
case class FixRootLabelVerticalAnnotation[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    tree match {
      case b@BinaryTree(label, lc, rc, span) => b
      case n@NullaryTree(label, span) => n
      case u@UnaryTree(label, c, chain, span)  =>
      UnaryTree(label,
       c.relabelRoot(rootLabel => new AnnotatedLabel(rootLabel.label, rootLabel.headTag, (0 until 1).map(i => rootLabel.label), rootLabel.siblings, rootLabel.features)),
       chain,
       span)
    }
  }
}

case class PreterminalAnnotation() extends Annotation
case class TagAnnotation() extends Annotation

// For sentiment: we don't want to suffer from parent annotation at the root when all other
// symbols are symmetric, so we'd rather have something generic like 3^3 rather than 3^TOP
case class MarkPreterminals[W]() extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = {
    def rec(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case b@BinaryTree(label, lc, rc, span) => BinaryTree(label, rec(lc), rec(rc), span)
      case n@NullaryTree(label, span) => NullaryTree(label, span)
      case u@UnaryTree(label, c, chain, span) =>
        if (c.isLeaf) {
          UnaryTree(new AnnotatedLabel(label.label, label.headTag, label.parents, label.siblings, label.features ++ Set(new PreterminalAnnotation())),
                    c.relabelRoot(cLabel => new AnnotatedLabel(cLabel.label, cLabel.headTag, cLabel.parents, cLabel.siblings, cLabel.features ++ Set(new TagAnnotation()))),
                    chain,
                    span)
        } else {
          UnaryTree(label, rec(c), chain, span)
        }
    }
    rec(tree)
  }
}

trait MarkDominates[W] extends TreeAnnotator[AnnotatedLabel, W, AnnotatedLabel] {
  protected def dominates(x: Tree[AnnotatedLabel]): Boolean
  protected def sym: String
  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[W]) = tree.extend { t =>
    if (t eq tree) t.label
    else if (dominates(t)) t.label.annotate(Dom(sym))
    else t.label
  }
}

case class DominatesV[W]() extends MarkDominates[W] {
  protected def dominates(x: Tree[AnnotatedLabel]): Boolean = x.leaves.exists { t => t.label.label.startsWith("V") || t.label.label.startsWith("MD")}
  def sym = "V"
}

