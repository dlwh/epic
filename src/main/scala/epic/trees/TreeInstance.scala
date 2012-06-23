package epic.trees

import breeze.data.Example

case class TreeInstance[L, +W](id: String,
                               tree: BinarizedTree[L],
                               words: Seq[W]) extends Example[Tree[L], Seq[W]] {
  def mapLabels[U](f: L => U) = copy(tree = tree.map(f))

  def label = tree;

  def features = words
}
