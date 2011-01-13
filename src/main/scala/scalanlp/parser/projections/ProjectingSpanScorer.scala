package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingSpanScorer[C,F](indexedProjections: ProjectionIndexer[C,F], scorer: SpanScorer[C]) extends SpanScorer[F] {
  def scoreLexical(begin: Int, end: Int, tag: Int) = {
    val pTag = indexedProjections.project(tag)
    scorer.scoreLexical(begin,end, pTag) - math.log(indexedProjections.refinementsOf(pTag).length);
  }

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    val pParent = indexedProjections.project(parent)
    val pChild = indexedProjections.project(child)
    scorer.scoreUnaryRule(begin,end,pParent, pChild) - math.log(indexedProjections.refinementsOf(pParent).length);
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    val pParent = indexedProjections.project(parent)
    scorer.scoreBinaryRule(begin,split, end,pParent,
      indexedProjections.project(leftChild),
      indexedProjections.project(rightChild))- math.log(indexedProjections.refinementsOf(pParent).length);
  }
}

object ProjectingSpanScorer {
  def factory[C,F](indexedProjections:ProjectionIndexer[C,F])= new SpanScorer.Factory[F,C,Any] {
    def apply(scorer: SpanScorer[C]) = mkSpanScorer(Seq.empty,scorer)
    def mkSpanScorer(words: Seq[Any], scorer: SpanScorer[C]) = new ProjectingSpanScorer(indexedProjections,scorer);
  }
}