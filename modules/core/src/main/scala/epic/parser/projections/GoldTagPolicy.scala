package epic.parser.projections

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
import epic.trees.BinarizedTree
import breeze.collection.mutable.TriangularArray

/**
 * GoldTagPolicys are used in places where sometimes (mostly for debugging) we'd like to know
 * which labeled spans are "gold" (correct) and which are not.
 *
 * @author dlwh
 *
 * @tparam L
 */
trait GoldTagPolicy[L] {
  def isGoldSpan(start: Int, end: Int): Boolean
  def isGoldTopTag(start: Int, end: Int, tag: Int): Boolean
  def isGoldBotTag(start: Int, end: Int, tag: Int): Boolean
}

object GoldTagPolicy {
  def noGoldTags[L]:GoldTagPolicy[L] = new GoldTagPolicy[L] {
    def isGoldTopTag(start: Int, end: Int, tag: Int): Boolean = false
    def isGoldBotTag(start: Int, end: Int, tag: Int): Boolean = false
    def isGoldSpan(start: Int, end: Int): Boolean = false
  }

  def goldTreeForcing[L](trees: BinarizedTree[Int]*):GoldTagPolicy[L] ={
    val goldTop = TriangularArray.raw(trees.head.span.end+1,collection.mutable.BitSet())
    val goldBot = TriangularArray.raw(trees.head.span.end+1,collection.mutable.BitSet())
    for(tree <- trees) {
      if (tree != null) {
        for( t <- tree.allChildren if t.label != -1) {
          if (t.children.size == 1)
            goldTop(TriangularArray.index(t.span.begin,t.span.end)) += t.label
          else
            goldBot(TriangularArray.index(t.span.begin,t.span.end)) += t.label
        }
      }
    }
    new GoldTagPolicy[L] {
      def isGoldSpan(start: Int, end: Int): Boolean = {
        val set = goldTop(TriangularArray.index(start,end))
        set != null && set.nonEmpty
      }
      def isGoldTopTag(start: Int, end: Int, tag: Int) = {
        val set = goldTop(TriangularArray.index(start,end))
        set != null && set.contains(tag)
      }

      def isGoldBotTag(start: Int, end: Int, tag: Int): Boolean = {
        val set = goldBot(TriangularArray.index(start,end))
        set != null && set.contains(tag)
      }
    }
  }
}