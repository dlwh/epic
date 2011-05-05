package scalanlp.parser
package projections

/**
 * 
 * @author dlwh
 */
class ProjectingSpanScorer[C,F](indexedProjections: ProjectionIndexer[C,F], val scorer: SpanScorer[C], downWeight: Boolean=true) extends SpanScorer[F] {

  private val projectionAdjustments = new Array[Double](indexedProjections.coarseIndex.size);
  if(downWeight)
    for(c <- 0 until indexedProjections.coarseIndex.size) {
      projectionAdjustments(c) = math.log(indexedProjections.refinementsOf(c).length);
    }

  def scoreLexical(begin: Int, end: Int, tag: Int) = {
    val pTag = indexedProjections.project(tag)
    scorer.scoreLexical(begin,end, pTag) - projectionAdjustments(pTag);
  }

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    val pParent = indexedProjections.project(parent)
    val pChild = indexedProjections.project(child)
    scorer.scoreUnaryRule(begin,end,pParent, pChild) - projectionAdjustments(pParent);
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    val pParent = indexedProjections.project(parent)
    scorer.scoreBinaryRule(begin,split, end,pParent,
      indexedProjections.project(leftChild),
      indexedProjections.project(rightChild)) - projectionAdjustments(pParent);
  }
}

object ProjectingSpanScorer {
  def factory[C,F](indexedProjections:ProjectionIndexer[C,F])= new SpanScorer.Factory[F,C,Any] {
    def apply(scorer: SpanScorer[C]) = mkSpanScorer(Seq.empty,scorer)
    def mkSpanScorer(words: Seq[Any], scorer: SpanScorer[C]) = new ProjectingSpanScorer(indexedProjections,scorer);
  }
}