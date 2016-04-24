package epic.trees

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import java.io.StringReader

import breeze.util.Lens
import epic.preprocess.TreebankTokenizer

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
trait Tree[+L] extends Serializable {
  def label: L
  def children: IndexedSeq[Tree[L]]
  def span: Span

  def begin = span.begin
  def end = span.end

  def isLeaf = children.isEmpty
  /**
  * A tree is valid if this' span contains all children's spans 
  * and each child abuts the next one.
  */
  def isValid = isLeaf || {
    children.map(_.span).forall(this.span contains _) &&
    children.iterator.drop(1).zip(children.iterator).forall { case (next,prev) =>
      prev.span.end == next.span.begin
    } &&
    children(0).span.begin == this.span.begin &&
    children.last.span.end == this.span.end
  }

  def leaves: Iterable[Tree[L]] = if (isLeaf) {
    IndexedSeq(this).view
  } else  {
    children.map(_.leaves).foldLeft[Stream[Tree[L]]](Stream.empty){_ append _}
  }

  /**
   * Useful for stripping the words out of a tree
   * Returns (tree without leaves, leaves)
   */
  def cutLeaves: (Tree[L], IndexedSeq[L]) = {
    def recCutLeaves(tree: Tree[L]): (Option[Tree[L]], IndexedSeq[L]) = {
      if (tree.isLeaf) (None,IndexedSeq(tree.label))
      else {
        val fromChildren = tree.children.map(recCutLeaves _)
        Some(Tree(tree.label, fromChildren.flatMap(_._1), tree.span)) -> fromChildren.flatMap(_._2)
      }
    }
    val (treeOpt, leaves) = recCutLeaves(this)
    treeOpt.get -> leaves
  }

  def map[M](f: L => M): Tree[M] = Tree(f(label), children map (_ map f), span)
  def extend[B](f: Tree[L] => B): Tree[B] = Tree(f(this),children map (_ extend f), span)
  def relabelRoot[B >: L](f: L => B): Tree[B]

  def allChildren = preorder

  def preorder: Iterator[Tree[L]] = {
    children.map(_.preorder).foldLeft( Iterator(this))(_ ++ _)
  }

  def postorder: Iterator[Tree[L]] = {
    children.map(_.postorder).foldRight(Iterator(this))(_ ++ _)
  }

  def leftHeight: Int = if (isLeaf) 0 else 1 + children(0).leftHeight

  import epic.trees.Tree._
  override def toString = toString(false)

  def toString(newline: Boolean) = recursiveToString(this, 0, newline, new StringBuilder).toString

  def render[W](words: Seq[W], newline: Boolean = true) = recursiveRender(this, 1, words, newline, new StringBuilder).toString

}

object Tree {
  def apply[L](label: L, children: IndexedSeq[Tree[L]], span: Span): NaryTree[L] = NaryTree(label,children, span)
  def unapply[L](t: Tree[L]): Option[(L, IndexedSeq[Tree[L]], Span)] = Some((t.label, t.children, t.span))
  def fromString(input: String): (Tree[String], IndexedSeq[String]) = new PennTreeReader(new StringReader(input)).next

  private def recursiveToString[L](tree: Tree[L], depth: Int, newline: Boolean, sb: StringBuilder): StringBuilder = {
    import tree._
    sb append "( " append tree.label append " [" append span.begin append "," append span.end append "] "
    for( c <- tree.children ) {
      if (newline && c.children.nonEmpty) sb append "\n" append "  " * depth
      else sb.append(' ')
      recursiveToString(c,depth+1,newline, sb)
    }
    sb append ")"
    sb
  }

  private def recursiveRender[L, W](tree: Tree[L], depth: Int, words: Seq[W], newline: Boolean, sb: StringBuilder): StringBuilder =  {
    import tree._
    sb append "(" append tree.label
    if (isLeaf) {
      sb append TreebankTokenizer.tokensToTreebankTokens(span.map(words).map(_.toString)).mkString(" "," ","")
    } else {
      val anyNonTerminals = children.exists(!_.isLeaf)
      //sb append "\n"
      for( c <- children ) {
        if (newline && (c.span.length != words.length) && anyNonTerminals) sb append "\n" append "  " * depth
        else sb.append(' ')
        recursiveRender(c,depth+1, words, newline, sb)
      }
    }
    sb append ')'
    if (sb.length > 1 && sb(sb.length-2) != ')'  && sb(sb.length-2) != ' ')
      sb append ' '
    sb
  }

}

case class NaryTree[L](label: L, children: IndexedSeq[Tree[L]], span: Span) extends Tree[L] {
  def relabelRoot[B >: L](f: L => B): Tree[B] = copy(f(label))
}

sealed trait BinarizedTree[+L] extends Tree[L] {
  def findSpan(begin: Int, end: Int): Option[Tree[L]] = this match {
    case t if t.span == Span(begin, end) => Some(t)
    case t@BinaryTree(a, b, c, span) if end <= t.splitPoint => if (t.begin <= begin) b.findSpan(begin, end) else None
    case t@BinaryTree(a, b, c, span) if t.splitPoint <= begin => if (t.end <= end) c.findSpan(begin, end) else None
    case t@UnaryTree(a, b, chain, span) => if (span.contains(Span(begin, end))) b.findSpan(begin, end) else None
    case _ => None
  }

  override def map[M](f: L => M): BinarizedTree[M] = null
  // have to override to trick scala to refine the type
  override def extend[B](f: Tree[L] => B): BinarizedTree[B] = { sys.error("...") }
  def relabelRoot[B >: L](f: L => B): BinarizedTree[B]
}

case class BinaryTree[+L](label: L,
                          leftChild: BinarizedTree[L],
                          rightChild: BinarizedTree[L],
                          span: Span) extends BinarizedTree[L] {
  def children = IndexedSeq(leftChild, rightChild)

  override def map[M](f: L => M): BinaryTree[M] = BinaryTree(f(label), leftChild map f, rightChild map f, span)
  override def extend[B](f: Tree[L] => B) = BinaryTree(f(this), leftChild extend f, rightChild extend f, span)
  def relabelRoot[B >: L](f: L => B): BinarizedTree[B] = BinaryTree(f(label), leftChild, rightChild, span)
  def splitPoint = leftChild.span.end

  override def allChildren: Iterator[BinaryTree[L]] = super.allChildren.asInstanceOf[Iterator[BinaryTree[L]]]

  override def preorder: Iterator[BinaryTree[L]] = super.preorder.asInstanceOf[Iterator[BinaryTree[L]]]

  override def postorder: Iterator[BinaryTree[L]] = super.postorder.asInstanceOf[Iterator[BinaryTree[L]]]
}

case class UnaryTree[+L](label: L, child: BinarizedTree[L], chain: IndexedSeq[String], span: Span) extends BinarizedTree[L] {
  def children = IndexedSeq(child)
  override def map[M](f: L => M): UnaryTree[M] = UnaryTree( f(label), child map f, chain, span)
  override def extend[B](f: Tree[L] => B) = UnaryTree( f(this), child extend f, chain, span)
  def relabelRoot[B >: L](f: L => B): BinarizedTree[B] = UnaryTree(f(label), child, chain, span)
}

case class NullaryTree[+L](label: L, span: Span) extends BinarizedTree[L] {
  def children = IndexedSeq.empty
  override def map[M](f: L => M): NullaryTree[M] = NullaryTree( f(label), span)
  override def extend[B](f: Tree[L] => B) = NullaryTree( f(this), span)
  def relabelRoot[B >: L](f: L => B): BinarizedTree[B] = NullaryTree(f(label), span)
}

object Trees {

  def binarize[L](tree: Tree[L],
                  makeIntermediate: (L, L) => L,
                  extendIntermediate: (L, Either[L,L]) => L,
                  headFinder: HeadFinder[L]): BinarizedTree[L] = tree match {
    case Tree(l, Seq(), span) => NullaryTree(l, span)
    case Tree(l, Seq(oneChild), span) => UnaryTree(l,binarize(oneChild, makeIntermediate, extendIntermediate, headFinder), IndexedSeq.empty, tree.span)
    case Tree(l, Seq(leftChild,rightChild), span) =>
      BinaryTree(l,binarize(leftChild, makeIntermediate, extendIntermediate, headFinder),binarize(rightChild, makeIntermediate, extendIntermediate, headFinder), tree.span)
    case Tree(l, children, span) =>
      val headChildIndex = headFinder.findHeadChild(tree)
      val binarized = children.map(binarize(_, makeIntermediate, extendIntermediate, headFinder))
      val headChild = binarized(headChildIndex)
      // fold in right arguments
      // newArg is hthe next right child
      // for a rule A -> B C [D] E F, we want A -> @A[D]>E F, @A[D]>E -> D E
      val right = binarized.drop(headChildIndex+1).foldLeft(headChild){ (tree,newArg) =>
        // TODO ugh
        val intermediate =  {
          if (tree eq headChild)
            makeIntermediate(l, headChild.label)
          else
            tree.label
        }
        val newIntermediate = extendIntermediate(intermediate, Right(newArg.label))
        BinaryTree(newIntermediate,tree,newArg, Span(tree.span.begin,newArg.span.end))
      }
      // now fold in left args
      val fullyBinarized = binarized.take(headChildIndex).foldRight(right){(newArg,tree) =>
        val intermediate =  {
          if (tree eq headChild)
            makeIntermediate(l, headChild.label)
          else
            tree.label
        }
        val newIntermediate = extendIntermediate(intermediate, Left(newArg.label))
        BinaryTree(newIntermediate, newArg,tree, Span(newArg.span.begin,tree.span.end))
      }
//      UnaryTree(l, fullyBinarized, IndexedSeq.empty, tree.span)
      fullyBinarized.relabelRoot(_ => l)
  }

  def binarize(tree: Tree[String], headFinder: HeadFinder[String] = HeadFinder.collins): BinarizedTree[String] = {
    def stringBinarizer(currentLabel: String, headTag: String) = {
      if (currentLabel.startsWith("@")) currentLabel
      else s"@$currentLabel[$headTag]"
    }

    def extendIntermediate(currentLabel: String, sib: Either[String, String]) = {
      sib match {
        case Left(s) =>
          s"$currentLabel<$s"
        case Right(s) =>
          s"$currentLabel>$s"

      }
    }

    binarize[String](tree, stringBinarizer, extendIntermediate, headFinder)
  }

  def deannotate(tree: Tree[String]): Tree[String] = tree.map(deannotateLabel _)
  def deannotate(tree: BinarizedTree[String]): BinarizedTree[String] = tree.map(deannotateLabel _)
  def deannotateLabel(l: String) = l.takeWhile(c => c != '^' && c != '>')

  /**
   * Adds horizontal markovization to an already binarized tree with no markovization.
   *
   * The sibling history of a node is:
   * If one of its children is an intermediate, then its history concatenated with its sibling (if it exists), markovizing.
   * if neither is an intermediate, and it is an intermediate, then the right child.
   * Otherwise it is empty
   *
   */
  def addHorizontalMarkovization[T](tree: BinarizedTree[T],
                                    order: Int,
                                    join: (T, IndexedSeq[Either[T, T]]) => T,
                                    isIntermediate: T => Boolean): BinarizedTree[T] = {
    def rec(tree: BinarizedTree[T]): (BinarizedTree[T], IndexedSeq[Either[T, T]]) = {
      tree match {
        case BinaryTree(label, t1, t2, span) if isIntermediate(t1.label) =>
          val (newt1, newhist) = rec(t1)
          val (newt2, _) = rec(t2)
          val newHistory = (Right(t2.label) +: newhist).take(order)
          val newLabel = join(label, newHistory)
          BinaryTree(newLabel, newt1, newt2, span) -> newHistory
        case BinaryTree(label, t1, t2, span) if isIntermediate(t2.label) =>
          val (newt1, _) = rec(t1)
          val (newt2, newhist) = rec(t2)
          val newHistory = (Left(t1.label) +: newhist).take(order)
          val newLabel = join(label, newHistory)
          BinaryTree(newLabel, newt1, newt2, span) -> newHistory
        case BinaryTree(label, t1, t2, span) =>
          val (newt1, _) = rec(t1)
          val (newt2, _) = rec(t2)
          val newHistory = if (isIntermediate(label) && order > 0) IndexedSeq(Right(t2.label)) else IndexedSeq.empty
          val newLabel = if (isIntermediate(label)) join(label, newHistory) else  label
          BinaryTree(newLabel, newt1, newt2, tree.span) -> newHistory
        case UnaryTree(label, child, chain, span) =>
          val (newt1, hist) = rec(child)
          val newHistory = if (isIntermediate(label)) hist else IndexedSeq.empty
          val newLabel = if (isIntermediate(label)) join(label, newHistory) else  label
          UnaryTree(newLabel, newt1, chain, tree.span) -> newHistory
        case tree@NullaryTree(_, span) => tree -> IndexedSeq.empty
      }

    }

    rec(tree)._1
  }

  def addHorizontalMarkovization(tree: BinarizedTree[String], order: Int): BinarizedTree[String] = {
    def join(t: String, chain: IndexedSeq[Either[String, String]]) = chain.map{ case Left(l) => "\\" + l case Right(r) => "/" + r}.mkString(t +">","_","")
    addHorizontalMarkovization(tree,order,join,(_: String).startsWith("@"))
  }

  def debinarize[L](tree: Tree[L], isBinarized: L => Boolean): Tree[L] = {
    val l = tree.label
    val children = tree.children
    val buf = new ArrayBuffer[Tree[L]]
    for(c <- children) {
      if (isBinarized(c.label)) {
        buf ++= debinarize(c,isBinarized).children
      } else {
        buf += debinarize(c,isBinarized)
      }
    }
    Tree(l,buf, tree.span)
  }

  def debinarize(tree: Tree[String]): Tree[String] = debinarize(tree, (x: String) => x.startsWith("@"))

  def annotateParents[L](tree: Tree[L], join: (L, L) => L, depth: Int, history: List[L] = List.empty):Tree[L] = {
    if (depth == 0) tree
    else {
      val newLabel = (tree.label :: history).iterator.take(depth).reduceLeft(join)
      Tree(newLabel,tree.children.map(c => annotateParents[L](c,join,depth,tree.label :: history.take(depth-1 max 0))), tree.span)
    }
  }

  def annotateParents(tree: Tree[String], depth: Int):Tree[String] = annotateParents(tree, { (x: String, b: String) => x + '^' + b },depth)

  /**
   * Adds parent-markovization to an already binarized tree. Also handles the unary layering we do by ignoring
   * identity unary transitions in the history
   * @param tree the tree
   * @param join join: join two elements of the history into a single label. (child,parent)=>newChild
   * @param isIntermediate is this an intermediate symbol? Determines whether or not we should include the immediate parent of this label in the history
   * @param depth how much history to keep
   * @tparam L type of the tree
   * @return
   */
  def annotateParentsBinarized[L](tree: BinarizedTree[L], join: (L, Seq[L]) => L, isIntermediate: L => Boolean, dontAnnotate: Tree[L] => Boolean, depth: Int): BinarizedTree[L] = {
    val ot = tree
    def rec(tree: BinarizedTree[L], history: List[L] = List.empty): BinarizedTree[L] = {
      import tree._
      val newLabel = if (dontAnnotate(tree)) {
        label
      } else if (isIntermediate(label)) {
        assert(history.length > 1, label + " " + history + "\n\n\n" + tree + "\n\n\n" + ot)
        join(label, history drop 1 take depth)
      } else {
        join(label, history take depth)
      }

      tree match {
        //invariant: history is the (depth) non-intermediate symbols, where we remove unary-identity transitions
        case BinaryTree(label, t1, t2, span) =>
          val newHistory = if (!isIntermediate(label)) label :: history else history
          val lchild = rec(t1,newHistory)
          val rchild = rec(t2,newHistory)
          BinaryTree(newLabel, lchild, rchild, span)
        case u@UnaryTree(label, child, chain, span) =>
          if (isIntermediate(label)) assert(history.nonEmpty, ot.toString(true) + "\n" + u.toString(true) )
          //if (isIntermediate(label)) assert(label != newLabel, label + " " + newLabel + " " + u + " " + history)
          val newHistory = if (!isIntermediate(label) && label != child.label) label :: history else history
          UnaryTree(newLabel,rec(child,newHistory), chain, span)
        case NullaryTree(label, span) =>
          NullaryTree(newLabel, span)
      }
    }
    rec(tree)

  }

  def annotateParentsBinarized(tree: BinarizedTree[String], depth: Int): BinarizedTree[String] = {
    annotateParentsBinarized(tree, { (x: String, b: Seq[String]) => b.foldLeft(x)(_ + '^' + _) },(_: String).startsWith("@"), { (l: Tree[String]) => l.label.nonEmpty && l != "$" && !l.label.head.isLetterOrDigit && l.label != "." }, depth)
  }

  object Transforms {

    @SerialVersionUID(1L)
    class EmptyNodeStripper[T](implicit lens: Lens[T, String]) extends (Tree[T] => Option[Tree[T]]) with Serializable {
      def apply(tree: Tree[T]): Option[Tree[T]] = {
        if (lens.get(tree.label) == "-NONE-") None
        else if (tree.span.begin == tree.span.end) None // screw stupid spans
        else {
          val newC = tree.children map this filter (None!=)
          if (newC.isEmpty && !tree.isLeaf) None
          else Some(Tree(tree.label,newC map (_.get), tree.span))
        }
      }
    }

    class XOverXRemover[L] extends (Tree[L] => Tree[L]) {
      def apply(tree: Tree[L]): Tree[L] = {
        if (tree.children.size == 1 && tree.label == tree.children(0).label) {
          this(tree.children(0))
        } else {
          Tree(tree.label,tree.children.map(this), tree.span)
        }
      }
    }

    class FunctionNodeStripper[T](implicit lens: Lens[T, String]) extends (Tree[T] => Tree[T]) {
      def apply(tree: Tree[T]): Tree[T] = {
        tree.map { label =>
          lens.get(label) match {
          case "-RCB-" | "-RRB-" | "-LRB-" | "-LCB-" => label
          case "PRT|ADVP" => lens.set(label, "PRT")
          case x =>
            if (x.startsWith("--")) lens.set(label,x.replaceAll("---.*","--"))
            else lens.set(label,x.replaceAll("[-|=].*",""))
          }
        }
      }
    }

    class StripLabels[L](labels: String*)(implicit lens: Lens[L, String]) extends (Tree[L] => Tree[L]) {
      private val badLabels = labels.toSet
      def apply(tree: Tree[L]): Tree[L] = {
        def rec(t: Tree[L]): IndexedSeq[Tree[L]] = {
          if (badLabels(lens.get (t.label))) {
            t.children.flatMap(rec)
          } else {
            IndexedSeq(Tree (t.label, t.children.flatMap(rec), t.span))
          }
        }
        rec(tree).head
      }
    }

    object StandardStringTransform extends (Tree[String] => Tree[String]) {
      private val ens = new EmptyNodeStripper[String]
      private val xox = new XOverXRemover[String]
      // private val fns = new FunctionNodeStripper[String]
      def apply(tree: Tree[String]): Tree[String] = {
        xox(ens(tree).get)
      }
    }

    class LensedStandardTransform[T](implicit lens: Lens[T, String]) extends (Tree[T] => Tree[T]) {
      private val ens = new EmptyNodeStripper[T]
      private val xox = new XOverXRemover[T]
      private val fns = new FunctionNodeStripper[T]
      def apply(tree: Tree[T]) = {
        xox(fns(ens(tree).get)) map(l => lens.set(l, lens.get(l)))
      }
    }

    /*
    object GermanTreebankTransform extends (Tree[String]=>Tree[String]) {
      private val ens = new EmptyNodeStripper
      private val xox = new XOverXRemover[String]
      private val fns = new FunctionNodeStripper
      private val tr = GermanTraceRemover
      def apply(tree: Tree[String]): Tree[String] = {
        xox(tr(fns(ens(tree).get))) map (_.intern)
      }
    }

    object GermanTraceRemover extends (Tree[String]=>Tree[String]) {
      def apply(tree: Tree[String]):Tree[String] = {
        tree.map(_.replaceAll("\\-\\*T.\\*",""))
      }
    }
    */

  }

  import epic.trees.Trees.Zipper._

  final case class Zipper[+L](tree: BinarizedTree[L], location: Location[L] = Zipper.Root) {
    @tailrec
    def upToRoot: Zipper[L] = up match {
      case Some(next) => next.upToRoot
      case None => this
    }

    def label = tree.label
    def begin = tree.begin
    def end = tree.end

    /*
     * assertion
     *
     */
    location match {
      case RightChild(_, _, ls) => assert(ls.end == tree.begin)
      case LeftChild(_, _, rs) => assert(tree.end == rs.begin)
      case _ =>
    }

    def up: Option[Zipper[L]] =  location match {
      case Root => None
      case LeftChild(pl, p, rightSibling) =>
        val parentTree = BinaryTree(pl, tree, rightSibling, Span(tree.begin, rightSibling.end))
        Some(Zipper(parentTree, p))
      case RightChild(pl, p, leftSibling) =>
        val parentTree = BinaryTree(pl, leftSibling, tree, Span(leftSibling.begin, tree.end))
        Some(Zipper(parentTree, p))
      case UnaryChild(pl, chain, p) =>
        val parentTree = UnaryTree(pl, tree, chain, tree.span)
        Some(Zipper(parentTree, p))
    }

    def left: Option[Zipper[L]] = location match {
      case RightChild(pl, parent, leftSibling) => Some(Zipper(leftSibling, LeftChild(pl, parent, tree)))
      case _ => None
    }

    def right = location match {
      case LeftChild(pl, parent, rightSibling) => Some(Zipper(rightSibling, RightChild(pl, parent, tree)))
      case _ => None
    }

    /**
     * goes to the left child
     * @return
     */
    def down: Option[Zipper[L]] = tree match {
      case BinaryTree(l, lc, rc, span) => Some(Zipper(lc, LeftChild(tree.label, location, rc)))
      case NullaryTree(_, _) => None
      case UnaryTree(parent, child, chain, span) =>
        Some(Zipper(child, UnaryChild(tree.label, chain, location)))
      case _ => sys.error("Shouldn't be here!")
    }

    /**
     * Goes to right child
     * @return
     */
    def downRight: Option[Zipper[L]] = tree match {
      case BinaryTree(l, lc, rc, span) => Some(Zipper(rc, RightChild(tree.label, location, lc)))
      case _ => None
    }

    def iterator:Iterator[Zipper[L]] = Iterator.iterate(Some(this):Option[Zipper[L]]){
      case None => None
      case Some(x) => x.next
    }.takeWhile(_.nonEmpty).flatten

    def next: Option[Zipper[L]] = tree match {
      case NullaryTree(l, span) =>
        // go up until we find a LeftChild, then go to its right child.
        // if we hit the root (that is, we only go up right children), there is no next.
        var cur: Option[Zipper[L]] = Some(this)
        while (true) {
          cur match {
            case None => return None
            case Some(loc@Zipper(_, LeftChild(_, _, _))) =>
              return loc.right
            case Some(y) =>
              cur = y.up
          }

        }
        sys.error("Shouldn't be here!")
      case _ =>
        down
    }
  }

  object Zipper {
    sealed trait Location[+L]
    case object Root extends Location[Nothing]
    sealed trait NotRoot[+L] extends Location[L] { def parent: Location[L]; def parentLabel: L}
    case class LeftChild[+L](parentLabel: L, parent: Location[L], rightSibling: BinarizedTree[L]) extends NotRoot[L]
    case class RightChild[+L](parentLabel: L, parent: Location[L], leftSibling: BinarizedTree[L]) extends NotRoot[L]
    case class UnaryChild[+L](parentLabel: L, chain: IndexedSeq[String], parent: Location[L]) extends NotRoot[L]
  }

}
