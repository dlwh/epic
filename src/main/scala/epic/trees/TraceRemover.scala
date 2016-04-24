package epic.trees

/**
 * Removes all traces from the word sequence, deleting all empty categories while it's at it.
 *
 * @author dlwh
 **/
class TraceRemover[T, W](emptyCategory: T=>Boolean) extends (Tree[T] =>Tree[T]) {
  def apply(tree: Tree[T]):Tree[T] = {
    def rec(tree: Tree[T]):Option[Tree[T]] = {
      if (emptyCategory(tree.label) || tree.span.begin == tree.span.end) {
        None
      } else if (tree.children.isEmpty) {
        Some(tree)
      } else {
        val newChildren = tree.children.map(rec).collect{ case Some(t) => t }
        if (newChildren.isEmpty && !tree.isLeaf) {
          None
        } else {
          Some(Tree(tree.label,newChildren, tree.span))
        }
      }
    }
    rec(tree).get
  }
}
