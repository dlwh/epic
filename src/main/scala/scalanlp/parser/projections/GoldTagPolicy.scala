package scalanlp.parser.projections

import scalanlp.trees.BinarizedTree
import scalanlp.collection.mutable.TriangularArray

trait GoldTagPolicy[L] {
  def isGoldTag(start: Int, end: Int, tag: Int): Boolean
}

object GoldTagPolicy {
  def noGoldTags[L]:GoldTagPolicy[L] = new GoldTagPolicy[L] {
    def isGoldTag(start: Int, end: Int, tag: Int) = false
  }

  def candidateTreeForcing[L](tree: BinarizedTree[Seq[Int]]):GoldTagPolicy[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)) ++= t.label
      }
    }
    new GoldTagPolicy[L] {
      def isGoldTag(start: Int, end: Int, tag: Int) = {
        val set = gold(TriangularArray.index(start,end))
        set != null && set.contains(tag)
      }
    }
  }


  def goldTreeForcing[L](trees: BinarizedTree[Int]*):GoldTagPolicy[L] ={
    val gold = TriangularArray.raw(trees.head.span.end+1,collection.mutable.BitSet());
    for(tree <- trees) {
      if(tree != null) {
        for( t <- tree.allChildren) {
          gold(TriangularArray.index(t.span.start,t.span.end)) += t.label
        }
      }
    }
    new GoldTagPolicy[L] {
      def isGoldTag(start: Int, end: Int, tag: Int) = {
        gold(TriangularArray.index(start,end)) contains tag
      }
    }
  }
}