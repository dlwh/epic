package epic.trees
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import epic.framework.Example
import epic.sequences.TaggedSequence

case class TreeInstance[L, +W](id: String,
                               tree: BinarizedTree[L],
                               words: IndexedSeq[W]) extends Example[BinarizedTree[L], IndexedSeq[W]] {

  def mapLabels[U](f: L => U) = copy(tree = tree.map(f))

  def label = tree

  def features = words

  def asTaggedSequence: TaggedSequence[L, W] = {
    new TaggedSequence(tree.leaves.map(_.label).toIndexedSeq, words.toIndexedSeq, id)
  }

  override def toString(): String = s"TreeInstance(id = $id, tree=${tree.render(words, newline = false)}\n}"

  def render(newline:Boolean = true) = tree.render(words, newline)
}
