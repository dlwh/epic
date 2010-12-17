package scalanlp.parser
package discrim

import ParseChart._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.math.Numerics
import projections._
import scalanlp.trees.BinarizedTree


/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
trait EPApproximator[C,F,W] {
  // TODO: add a type for the kind of span scorer this is.
  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer, goldTree: BinarizedTree[C]):SpanScorer
  def divideAndNormalize(num: SpanScorer, denom: SpanScorer, words: Seq[W]):SpanScorer
}


@serializable
@SerialVersionUID(1)
class AnchoredRuleApproximator[C,F,W](fineParser: ChartBuilder[LogProbabilityParseChart,F,W],
                                      coarseParser: ChartBuilder[LogProbabilityParseChart,C,W],
                                      projections: ProjectionIndexer[C,F], pruningThreshold: Double = Double.NegativeInfinity) extends EPApproximator[C,F,W] {
  val factory = new AnchoredRuleScorerFactory[C,F,W](fineParser,projections,pruningThreshold);

  val zeroGrammar = new ZeroGrammar(coarseParser.grammar);
  val zeroLexicon = new ZeroLexicon(coarseParser.lexicon);
  val zeroParser = new CKYChartBuilder[LogProbabilityParseChart,C,W](coarseParser.root, zeroLexicon,zeroGrammar,ParseChart.logProb);
  //val zeroFactory = new NonNormalizingAnchoredRuleScorerFactory[C,C,W](coarseParser,new ProjectionIndexer(coarseParser.grammar.index,coarseParser.grammar.index,identity[C]_),pruningThreshold)
  val zeroFactory = { error("TODO"); new AnchoredRuleScorerFactory[C,C,W](coarseParser,new ProjectionIndexer(coarseParser.grammar.index,coarseParser.grammar.index,identity[C]_),pruningThreshold) }

  def project(inside: ParseChart[F], outside: ParseChart[F], partition: Double, spanScorer: SpanScorer, tree: BinarizedTree[C]):SpanScorer = {
    factory.buildSpanScorer(inside,outside,  partition, spanScorer, tree);
  }

  def divideAndNormalize(num: SpanScorer, denom: SpanScorer, words: Seq[W]):SpanScorer ={
    val div = ScalingSpanScorer(num,denom,0.0,-1);
    zeroFactory.mkSpanScorer(words,div);
  }

  def updateCorrection(oldCorrection: SpanScorer, newF0: SpanScorer, rescaled: SpanScorer, len: Int, damping: Double):SpanScorer ={
    if(damping == 1.0)
      ScalingSpanScorer(newF0,rescaled,0.0,-1);
    else {
      val damped = new SpanScorer {
        def scoreLexical(begin: Int, end: Int, tag: Int) = {
          oldCorrection.scoreLexical(begin,end,tag) * (1-damping) + damping * newF0.scoreLexical(begin,end,tag) -
                  damping * rescaled.scoreLexical(begin,end,tag)
        }

        def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
          oldCorrection.scoreUnaryRule(begin,end,parent,child) * (1-damping) + damping * newF0.scoreUnaryRule(begin,end,parent,child) -
                  damping * rescaled.scoreUnaryRule(begin,end,parent,child)
        }

        def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
          oldCorrection.scoreBinaryRule(begin,split,end,parent,leftChild,rightChild) * (1-damping) + damping * newF0.scoreBinaryRule(begin,split,end,parent,leftChild,rightChild) -
                  damping * rescaled.scoreBinaryRule(begin,split,end,parent,leftChild,rightChild)
        }
      }

      damped;

    }
  }
}