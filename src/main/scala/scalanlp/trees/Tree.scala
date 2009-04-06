package scalanlp.trees;

import util.Implicits._;

case class Tree[+L](label: L, children: Seq[Tree[L]])(val span: Span) {
  def isLeaf = children.size == 0;
  /**
  * A tree is valid if this' span contains all children's spans 
  * and each child abuts the next one.
  */
  def isValid = isLeaf || {
    children.map(_.span).forall(this.span contains _) &&
    children.elements.drop(1).zip(children.elements).forall { case (next,prev) =>
      prev.span.end == next.span.start
    } &&
    children(0).span.start == this.span.start && 
    children.last.span.end == this.span.end
  }

  def leaves:Iterable[Tree[L]] = if(isLeaf) {
    List(this).projection
  } else  {
    children.map(_.leaves).foldLeft[Stream[Tree[L]]](Stream.empty)(_ append _)
  }

  def allChildren = preorder;

  def preorder: Iterable[Tree[L]] = {
    children.map(_.preorder).foldLeft(List(this).projection)(_ append _)
  }

  def postorder: Iterable[Tree[L]] = {
    children.map(_.postorder).foldLeft[Stream[Tree[L]]](Stream.empty)(_ append _) append List(this).projection
  }

  import Tree._;
  override def toString = recursiveToString(this,0,new StringBuilder).toString;
  def render[W](words: Seq[W]) = recursiveRender(this,0,words, new StringBuilder).toString;
}

object Tree {

  def fromString(input: String):(Tree[String],Seq[String]) = PennTreeReader(input).left.get;

  private def recursiveToString[L](tree: Tree[L], depth: Int, sb: StringBuilder):StringBuilder = {
    val sb = new StringBuilder;
    import tree._;
    sb append "( " append tree.label append " [" append span.start append "," append span.end append "] ";
    for( c <- tree.children ) {
      sb append recursiveToString(c,depth+1,sb) append " ";
    }
    sb append ")";
    sb
  }


  private def recursiveRender[L,W](tree: Tree[L], depth: Int, words: Seq[W], sb: StringBuilder): StringBuilder =  {
    import tree._;
    if(isLeaf) {
      sb append "( " append tree.label append words(span).mkString(" "," "," ");
    } else {
      sb append "( " append tree.label append " ";
      for( c <- children ) {
        sb append recursiveRender(c,depth+1,words,sb) append " ";
      }
    }
    sb append ")";
    sb
  }

}

sealed trait BinarizedTree[+L] extends Tree[L];

case class BinaryTree[+L](l: L,
                          leftChild: BinarizedTree[L],
                          rightChild: BinarizedTree[L])(span: Span
                        ) extends Tree[L](l,List(leftChild,rightChild))(span
                        ) with BinarizedTree[L] {
}
case class UnaryTree[+L](l: L, child: BinarizedTree[L])(span: Span
                        ) extends Tree[L](l,List(child))(span
                        ) with BinarizedTree[L] {
}
case class NullaryTree[+L](l: L)(span: Span) extends Tree[L](l,Seq())(span) with BinarizedTree[L]{
}

object Trees {
  def binarize[L](tree: Tree[L], relabel: (L,L)=>L):BinarizedTree[L] = tree match {
    case Tree(l, Seq()) => NullaryTree(l)(tree.span)
    case Tree(l, Seq(oneChild)) => UnaryTree(l,binarize(oneChild,relabel))(tree.span);
    case Tree(l, Seq(leftChild, otherChildren@ _*)) =>
      val newLeftChild = binarize(leftChild,relabel);
      val newRightLabel = relabel(l,leftChild.label);
      val newRightChildSpan = Span(newLeftChild.span.end,tree.span.end);
      val newRightChild = binarize(Tree(newRightLabel,otherChildren)(newRightChildSpan), relabel);
      BinaryTree(l, newLeftChild, newRightChild)(tree.span) 
  }

  private def stringBinarizer(currentLabel: String, append: String) = { 
      val head = if(currentLabel(0) != '@') '@' + currentLabel + "->" else currentLabel
      head + "_" + append
  }
  def binarize(tree: Tree[String]):BinarizedTree[String] = binarize[String](tree, stringBinarizer _ );
}
