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
    Iterable(this).view
  } else  {
    children.map(_.leaves).foldLeft[Stream[Tree[L]]](Stream.empty){_ append _}
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
  override def toString = toString(false)

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

  def unfoldTree[A, L](v: A)(f: A => (IndexedSeq[Tree[L]] => Tree[L], IndexedSeq[A])): Tree[L] = f(v) match {
    case (tree, a) => tree(a.map(unfoldTree(_)(f)))
  }

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
