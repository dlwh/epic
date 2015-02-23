package epic.slab

@SerialVersionUID(1L)
class Tree[+L](val label: L, val span: Span, val children: IndexedSeq[Tree[L]]) extends SpanAnnotation with Serializable {
  def isLeaf = children.size == 0

  def isTree = isLeaf || {
    children.map(_.span).forall(this.span contains _) &&
    children.iterator.drop(1).zip(children.iterator).forall { case (next,prev) =>
      prev.span.end <= next.span.begin
    } &&
    this.span.begin <= children(0).span.begin &&
    children.last.span.end <= this.span.end
  }

  def leaves:Iterable[Tree[L]] = if(isLeaf) {
    IndexedSeq(this).view
  } else  {
    children.map(_.leaves).foldLeft[Stream[Tree[L]]](Stream.empty){_ append _}
  }

  /**
   * Useful for stripping the words out of a tree
   * Returns (tree without leaves, leaves)
   */
  def cutLeaves: (Tree[L],IndexedSeq[L]) = {
    def recCutLeaves(tree: Tree[L]): (Option[Tree[L]],IndexedSeq[L]) = {
      if(tree.isLeaf) (None,IndexedSeq(tree.label))
      else {
        val fromChildren = tree.children.map(recCutLeaves _)
        Some(Tree(tree.label, span, fromChildren.flatMap(_._1))) -> fromChildren.flatMap(_._2)
      }
    }
    val (treeOpt,leaves) = recCutLeaves(this)
    treeOpt.get -> leaves
  }

  def map[B](f: L=>B):Tree[B] = Tree(f(label), span, children map {_ map f})
  def extend[B](f: Tree[L]=>B):Tree[B] = Tree(f(this), span, children map {_ extend f})
  def offset(by: Int):Tree[L] = Tree(label, span.offset(by), children map {_ offset by})

  def allChildren = preorder

  def preorder: Iterator[Tree[L]] = {
    children.map(_.preorder).foldLeft(Iterator(this)){_ ++ _}
  }

  def postorder: Iterator[Tree[L]] = {
    children.map(_.postorder).foldRight(Iterator(this)){_ ++ _}
  }

  import epic.slab.Tree._
  // override def toString = toString(false)

  def toString(newline: Boolean) = recursiveToString(this, 0, newline, new StringBuilder).toString

  override def equals(other: Any) = other match {
    case that: Tree[L] => this.label == that.label && this.span == that.span && this.children == that.children
    case _ => false
  }
  override def hashCode = label.hashCode ^ span.hashCode ^ children.hashCode
}

object Tree {
  def apply[L](label: L, span: Span, children: IndexedSeq[Tree[L]]): Tree[L] = new Tree(label, span, children)
  def apply[L](label: L, span: Span): Tree[L] = Tree(label, span, Vector())
  def unapply[L](t: Tree[L]): Option[(L,IndexedSeq[Tree[L]], Span)] = Some((t.label,t.children, t.span))

  private def recursiveToString[L](tree: Tree[L], depth: Int, newline: Boolean, sb: StringBuilder):StringBuilder = {
    import tree._
    sb append "( " append tree.label append " [" append span.begin append "," append span.end append "] "
    for( c <- tree.children ) {
      if(newline && (c.children.nonEmpty)) sb append "\n" append "  " * depth
      else sb.append(' ')
      recursiveToString(c,depth+1,newline, sb)
    }
    sb append ")"
    sb
  }
}
