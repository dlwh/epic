package scalanlp.parser

import scalanlp.trees._

import scalanlp.parser.ParseEval.Statistics
import scalanlp.trees.Trees
import collection.IndexedSeq


@SerialVersionUID(1)
case class KMPipeline(horizontal: Int = 2,
                 vertical: Int = 2,
                 splitAux: Boolean = true,
                 splitVP: Boolean = true,
                 splitIN: Boolean = true,
                 splitPossNP: Boolean = true,
                 annotateBaseNP: Boolean = true,
                 annotateRightRecNP: Boolean = true,
                 markNonIdentityUnaries: Boolean = true,
                 markExternalUnaries: Boolean = true,
                 markDominatesV: Boolean = true) extends ((BinarizedTree[AnnotatedLabel], Seq[String])=>BinarizedTree[AnnotatedLabel]) {

  override def toString() = scala.runtime.ScalaRunTime._toString(this)

  import KMPipeline._

  private implicit def enrichFn[U, T](f: U=>T) = new {
    def >>?(b: Boolean, f2: T=> T) = {
      if(b) f andThen f2
      else f
    }
  }

  private val pipeline = { (tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) =>
    val root = tree.label.label
    ( {(_:BinarizedTree[AnnotatedLabel]).map(_.clearFeatures)}
      andThen {annVert _}
      andThen {annHorz _}
      >>? (splitAux, {_splitAux(_, words)})
      >>? (splitVP, {_splitVP(_)})
      >>? (splitIN, {_splitIN(_, root)})
      >>? (splitPossNP, {_splitPossNP(_)})
      >>? (annotateBaseNP, {_annotateBaseNP(_, root)})
      >>? (annotateRightRecNP, {_annotateRightRecNP(_)})
      >>? (markNonIdentityUnaries, {_markNonIdentityUnaries(_, root)})
      >>? (markExternalUnaries, {_markExternalUnaries(_, root)})
      >>? (markDominatesV, {markDominates(_, "V", l => l.startsWith("V") || l.startsWith("MD"))})
      )(tree)
  }

  def apply(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) =  pipeline(tree, words)

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

  val beVerbs = Set("be", "is", "are", "were", "am", "was", "been", "being")
  val hasVerbs = Set("has", "have", "had")

  private def _splitAux(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]): BinarizedTree[AnnotatedLabel] = {
    tree.extend { t =>
      t match {
        case UnaryTree(label, NullaryTree(lbl2)) if label.baseLabel == lbl2.baseLabel =>
          val w = words(t.span.start)
          if (beVerbs.contains(w.toLowerCase)) label.annotate(AuxBe).annotate(Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate(AuxHave).annotate(Aux)
          else label
        case NullaryTree(label) =>
          val w = words(t.span.start)
          if (beVerbs.contains(w.toLowerCase)) label.annotate(AuxBe).annotate(Aux)
          else if (hasVerbs.contains(w.toLowerCase)) label.annotate(AuxHave).annotate(Aux)
          else label
        case _ => t.label
      }
    }
  }

  val activeVerbs = Set("VBZ", "VBD", "VBP", "MD")
  private def _splitVP(tree: BinarizedTree[AnnotatedLabel]) = tree.extend { t =>
    if(t.label.baseLabel != "VP") t.label
    else {
      val headTag = HeadFinder.collins.lensed[AnnotatedLabel].findHeadTag(t)
      val base = headTag.baseLabel
      if (activeVerbs(base)) {
        t.label.annotate(VPisVBF)
      } else {
        t.label.annotate(VPisX(base))
      }
    }
  }


  private def _splitIN(tree: BinarizedTree[AnnotatedLabel], root: String,
              parent: Option[String] = None,
              grandParent: Option[String] = None):BinarizedTree[AnnotatedLabel] = {
    val blbl = tree.label.baseLabel
    tree match {
      case tree@NullaryTree(lbl) if blbl == "IN" =>
        if(grandParent.isEmpty || grandParent.exists(_ == root) || parent.exists(_ == root)) {
          tree
        } else if (grandParent.exists(_(0) == 'N') && (parent.exists(s => s(0) == 'P' || s(0) == 'A'))) {
          tree.copy(lbl.annotate(IN_N))(tree.span)
        } else if (parent.exists(_(0) == 'Q') && (grandParent.exists(s => s(0) == 'N' || s.startsWith("ADJP")))) {
          tree.copy(lbl.annotate(IN_Q))(tree.span)
        } else if(grandParent.exists(_ == "S")) {
          if(parent.exists(_ == "SBAR")) {
            tree.copy(lbl.annotate(IN_SCC))(tree.span)
          } else {
            tree.copy(lbl.annotate(IN_SC))(tree.span)
          }
        } else {
          tree
        }
      case UnaryTree(lbl, c) =>
        if(blbl != "IN") {
          if(parent.exists(_ != blbl))
            UnaryTree(lbl, _splitIN(c, root, Some(blbl), parent))(tree.span)
          else
            UnaryTree(lbl, _splitIN(c, root, parent, grandParent))(tree.span)
        } else {
          val nc = _splitIN(c, root, parent, grandParent)
          UnaryTree(nc.label, nc)(tree.span)
        }
      case BinaryTree(lbl, l,r) =>
        BinaryTree(lbl, _splitIN(l, root, Some(blbl), parent), _splitIN(r, root, Some(blbl), parent))(tree.span)
      case _ => tree
    }

  }

  def _splitPossNP(tree: BinarizedTree[AnnotatedLabel]) = tree.extend{ t =>
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


  def _annotateBaseNP(tree: BinarizedTree[AnnotatedLabel], root: String) = {
    // boolean is whether or not it's a "base"
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel], Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> true
      case t@UnaryTree(lbl1, NullaryTree(lbl2)) if lbl1.baseLabel == lbl2.baseLabel =>
        t -> true
      case t@UnaryTree(lbl1, child) =>
        val (newchild, ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate(BaseNP), newchild)(t.span) -> true
        } else if(lbl1.label == root) {
          UnaryTree(lbl1, newchild)(t.span) -> false
        } else {
        UnaryTree(lbl1, newchild)(t.span) -> false
        }
      case t@BinaryTree(lbl, lc, rc) =>
        val (newlc, lok) = rec(lc)
        val (newrc, rok) = rec(rc)
        if(lok && rok && lbl.baseLabel == "NP") {
          BinaryTree(lbl.annotate(BaseNP), newlc, newrc)(t.span) -> true
        } else {
          BinaryTree(lbl, newlc, newrc)(t.span) -> false
        }

    }
    rec(tree)._1

  }

  // TODO: fix
  def _annotateRightRecNP(tree: BinarizedTree[AnnotatedLabel]) = {
    // boolean is whether or not it has a right-most np
    def rec(tree: BinarizedTree[AnnotatedLabel]):(BinarizedTree[AnnotatedLabel], Boolean) = tree match {
      case t:NullaryTree[AnnotatedLabel] => t -> false
      case t@UnaryTree(lbl1, child) =>
        val (newchild, ok) = rec(child)
        if(ok && lbl1.baseLabel == "NP") {
          UnaryTree(lbl1.annotate(RRNP), newchild)(t.span) -> true
        } else {
          UnaryTree(lbl1, newchild)(t.span) -> (ok||lbl1.label == "NP")
        }
      case t@BinaryTree(lbl, lc, rc) =>
        val (newrc, rok) = rec(rc)
        if(rok && lbl.baseLabel == "NP") {
          val (newlc, _) = rec(lc)
          val lclc = annotateDownwards(newlc)
          BinaryTree(lbl.annotate(RRNP), lclc, newrc)(t.span) -> true
        } else {
          val (newlc, _) = rec(lc)
          BinaryTree(lbl, newlc, newrc)(t.span) -> (rok || (lbl.label == "NP"))
        }

    }

    def annotateDownwards(tree: BinarizedTree[AnnotatedLabel]):BinarizedTree[AnnotatedLabel] = tree match {
      case t:NullaryTree[AnnotatedLabel] => t
      case UnaryTree(lbl, child) if lbl.baseLabel == "NP" =>
        UnaryTree(lbl.annotate(RRNP), annotateDownwards(child))(tree.span)
      case BinaryTree(lbl, lc, rc) if lbl.baseLabel == "NP" =>
        BinaryTree(lbl.annotate(RRNP), annotateDownwards(lc), annotateDownwards(rc))(tree.span)
      case _ => tree
    }
    rec(tree)._1

  }


  def _markNonIdentityUnaries(tree: BinarizedTree[AnnotatedLabel], root: String):BinarizedTree[AnnotatedLabel] = tree match {
    case BinaryTree(label, lc, rc) => BinaryTree(label, _markNonIdentityUnaries(lc, root), _markNonIdentityUnaries(rc, root))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label, c) if label.label != root && label.label != c.label.label => UnaryTree(label.annotate(RealUnary), _markNonIdentityUnaries(c, root))(tree.span)
    case UnaryTree(label, c) => UnaryTree(label, _markNonIdentityUnaries(c, root))(tree.span)
  }

  def _markExternalUnaries(tree: BinarizedTree[AnnotatedLabel], root: String, shouldAnnotate: String=>Boolean = Set("RB", "DT")):BinarizedTree[AnnotatedLabel] = tree match {
    case BinaryTree(label, lc, rc) => BinaryTree(label, _markExternalUnaries(lc, root, shouldAnnotate), _markExternalUnaries(rc, root, shouldAnnotate))(tree.span)
    case NullaryTree(label) => tree
    case UnaryTree(label, c) if label.label != root && label.label != c.label.label && shouldAnnotate(c.label.label) =>
      UnaryTree(label, _markExternalUnaries(c, root, shouldAnnotate).relabelRoot(_.annotate(ExternalUnary)))(tree.span)
    case UnaryTree(label, c) => UnaryTree(label, _markExternalUnaries(c, root, shouldAnnotate))(tree.span)
  }

  def markDominates(tree: BinarizedTree[AnnotatedLabel], label: String, pred: String=>Boolean) = {
    def dominates(x: Tree[AnnotatedLabel]) = x.leaves.exists { t => pred(t.label.label) }
    tree.extend { t =>
      if(t eq tree) t.label
      else if(dominates(t)) t.label.annotate(Dom(label))
      else t.label
    }
  }
}

object KMPipeline {
  // Annotations
  trait Aux extends Annotation
  case object AuxBe extends Aux
  case object Aux extends Aux
  case object AuxHave extends Aux

  case object VPisVBF extends Annotation
  case class VPisX(x: String) extends Annotation

  sealed trait INKind extends Annotation
  case object IN_N extends INKind
  case object IN_Q extends INKind
  case object IN_SCC extends INKind
  case object IN_SC extends INKind

  case object NP_Possessive extends Annotation
  case object BaseNP extends Annotation
  case object RRNP extends Annotation

  case object RealUnary extends Annotation
  case object ExternalUnary extends Annotation

  case class Dom(str: String) extends Annotation
}

/**
 *
 * @author dlwh
 */

object KleinAndManningPipeline extends ParserPipeline {
  protected val paramManifest = manifest[Params]
  case class Params(baseParser: ParserParams.BaseParser,
                    pipeline: KMPipeline)

  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  validate: (Parser[AnnotatedLabel, String]) => Statistics,
                  params: Params) = {

    val xbarParser = params.baseParser.xbarGrammar(trainTrees)

    val pipeline = params.pipeline

    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree, ti.words)
      TreeInstance(ti.id, t,ti.words)
    }.seq
    val (words, binary, unary) = GenerativeParser.extractCounts(transformed);
    val grammar = WeightedGrammar.generative(AnnotatedLabel.TOP, binary, unary, words)
    val parser = SimpleChartParser[AnnotatedLabel, String](grammar)
    Iterator("Markovized" -> parser)
  }

}