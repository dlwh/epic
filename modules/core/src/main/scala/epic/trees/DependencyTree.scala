package epic.trees

/**
 * root index is words.length
 * @author dlwh
 */
case class DependencyTree[+L, +W](dependencies: IndexedSeq[(L, Int)], words: IndexedSeq[W]) {
  def render : String = {
    for(((label, head),dep) <- dependencies.zipWithIndex) yield {
      if (head == words.length) s"ROOT(${words(dep)}-$dep)" else s"$label(${words(head)}-$head, ${words(dep)}-$dep)"
    }
  }.mkString("\n")

  def arcs = for( ((_, head),dep) <- dependencies.zipWithIndex) yield (head, dep)
}

object DependencyTree {
  def fromTreeInstance[L, W](ti: TreeInstance[L, W], finder: HeadFinder[L] = HeadFinder.collins): DependencyTree[L, W] = {
    fromTree(ti.tree, ti.words, finder)
  }

  def fromTree[L, W](tree: Tree[L], words: IndexedSeq[W], headFinder: HeadFinder[L]=HeadFinder.collins): DependencyTree[L, W] = {
    val annotated = headFinder.annotateHeadIndices(tree)
    val deps = new Array[(L, Int)](words.length)
    for( subtree <- annotated.allChildren) {
      for(t <- subtree.children if t.label._2 != subtree.label._2) {
        deps(t.label._2) = t.label._1 -> subtree.label._2
      }
    }
    deps(annotated.label._2) = annotated.label._1 -> words.length
    DependencyTree(deps, words)
  }
}
