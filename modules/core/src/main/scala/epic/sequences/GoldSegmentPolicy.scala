package epic.sequences

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
import epic.trees.{Span, BinarizedTree}
import breeze.collection.mutable.TriangularArray

trait GoldSegmentPolicy[L] {
  def isGoldSegment(start: Int, end: Int, tag: Int): Boolean
}

object GoldSegmentPolicy {
  def noGoldSegments[L]:GoldSegmentPolicy[L] = new GoldSegmentPolicy[L] {
    def isGoldSegment(start: Int, end: Int, tag: Int): Boolean = false
  }

  def goldSegmentForcing[L](trees: IndexedSeq[(Int,Span)]*):GoldSegmentPolicy[L] ={
    val gold = TriangularArray.raw(trees.last.last._2.end+1,collection.mutable.BitSet())
    for(tree <- trees) {
      if (tree != null) {
        for( (label, span) <- tree) {
          gold(TriangularArray.index(span.begin,span.end)) += label
        }
      }
    }
    new GoldSegmentPolicy[L] {
      def isGoldSegment(start: Int, end: Int, tag: Int) = {
        val set = gold(TriangularArray.index(start,end))
        set != null && set.contains(tag)
      }
    }
  }
}