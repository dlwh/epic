package epic.trees

/**
 *
 * @author dlwh
 */
case class DependencyTree[+L, +W](dependencies: IndexedSeq[(L, Int)], words: IndexedSeq[W]) {
  def render : String = {
    for(((label, head),dep) <- dependencies.zipWithIndex if label != null) yield s"$label(${words(head)}-$head, ${words(dep)}-$dep)"
  }.mkString("\n")

}

object DependencyTree {
  def fromTree[L, W](tree: Tree[L], words: IndexedSeq[W], headFinder: HeadFinder[L]=HeadFinder.collins): DependencyTree[L, W] = {
    val annotated = headFinder.annotateHeadIndices(tree)
    val deps = new Array[(L, Int)](words.length)
    for( subtree <- annotated.allChildren) {
      for(t <- subtree.children) {
        deps(t.label._2) = (t.label._1 -> t.label._2)
      }
    }
    deps(annotated.label._2) = (null.asInstanceOf[L] -> words.length)
    DependencyTree(deps, words)
  }
}
