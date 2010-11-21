package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingSpanScorer[C,F](indexedProjections: ProjectionIndexer[C,F], scorer: SpanScorer) extends SpanScorer {
  def scoreLexical(begin: Int, end: Int, tag: Int) = {
    scorer.scoreLexical(begin,end, indexedProjections.project(tag));
  }

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    scorer.scoreUnaryRule(begin,end,indexedProjections.project(parent), indexedProjections.project(child))
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    scorer.scoreBinaryRule(begin,split, end,indexedProjections.project(parent),
      indexedProjections.project(leftChild),
      indexedProjections.project(rightChild))
  }
}