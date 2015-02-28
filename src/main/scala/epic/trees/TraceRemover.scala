package epic.trees

/**
 * Removes all traces from the word sequence, deleting all empty categories while it's at it.
 *
 * @author dlwh
 **/
class TraceRemover[T, W](emptyCategory: T=>Boolean) extends ( (Tree[T], IndexedSeq[W]) =>(Tree[T], IndexedSeq[W]) ) {
  def apply(tree: Tree[T], words: IndexedSeq[W]):(Tree[T], IndexedSeq[W]) = {
    def rec(tree: Tree[T]):(Option[Tree[T]], IndexedSeq[W]) = {
      if (emptyCategory(tree.label) || tree.span.begin == tree.span.end) {
        None -> IndexedSeq.empty
      } else if (tree.children.length == 0) {
        Some(tree) -> words.slice(tree.begin, tree.end)
      } else {
        val (newChildren, _newWords) = tree.children.map(rec).collect{ case (Some(t), w) => t -> w}.unzip
        val newWords = _newWords.flatten
        if (newChildren.length == 0 && !tree.isLeaf) {
          assert(newWords.length == 0)
          None -> IndexedSeq.empty
        } else {
          Some(Tree(tree.label,newChildren, tree.span)) -> newWords
        }
      }
    }

    val (t, w) = rec(tree)
    t.get -> w
  }

}
