package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.trees._
import scalanlp.optimize._

import InsideOutside._
import projections._
import scalanlp.trees.UnaryChainRemover.ChainReplacer;

import ParseChart.LogProbabilityParseChart;

import scalanlp.util._;
import logging._
import java.io._
import logging.ConfiguredLogging
import scalala.library.Library
import Library.sum
import scalala.tensor.{Counter,::}

/**
 * The objective function for log-linear parsers with no substates
 * @author dlwh
 */
class DiscrimObjective[L,W](feat: Featurizer[L,W],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L],
                            closedWords: Set[W])
        extends LatentDiscrimObjective[L,L,W](feat,trees,GrammarProjections.identity(coarseParser.grammar),coarseParser, openTags,closedWords) {

    override def treeToExpectedCounts(g: Grammar[L],
                           lexicon: Lexicon[L,W],
                           broker: BaseWeightedSpanBroker[L,L,W],
                           ti: TreeInstance[L,W],
                           spanScorer: SpanScorer[L] = SpanScorer.identity) = {
    val expectedCounts = new ExpectedCounts[W](g)
    val composite = SpanScorer.sum(ti.spanScorer,broker.spanForId(ti.id))
    val weights = DenseVector.zeros[Double](broker.numWeights)
    val visitor = broker.ecountsVisitor(ti.id,weights.data)
    var score = 0.0;
    for(t2 <- ti.tree.allChildren) {
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = g.index(BinaryRule(a,b,c))
          expectedCounts.ruleCounts(r) += 1
          score += ( g.ruleScore(r)
            + composite.scoreSpan(t2.span.start,t2.span.end,g.labelIndex(a))
            + composite.scoreBinaryRule(t2.span.start,bt.span.end,t2.span.end,r)
            )

          visitor.visitSpan(t2.span.start,t2.span.end,g.labelIndex(a), 1)
          visitor.visitBinaryRule(t2.span.start,bt.span.end,t2.span.end,r,1)
        case UnaryTree(a,Tree(b,_)) =>
          val r = g.index(UnaryRule(a,b))
          expectedCounts.ruleCounts(r) += 1
          visitor.visitUnaryRule(t2.span.start,t2.span.end,r,1)
          score += ( g.ruleScore(r)
            + composite.scoreUnaryRule(t2.span.start,t2.span.end,r)
            )
        case n@NullaryTree(a) =>
          val aI = g.labelIndex(a)
          val w = ti.words(n.span.start);
          expectedCounts.wordCounts.getOrElseUpdate(aI)(w) += 1
          visitor.visitSpan(t2.span.start,t2.span.end,aI, 1)
          score += lexicon.wordScore(g.labelIndex.get(aI), w) + ti.spanScorer.scoreSpan(t2.span.start,t2.span.end,aI)
      }
    }
    expectedCounts.logProb = score;
    expectedCounts -> weights
  }

}


import scalanlp.optimize.FirstOrderMinimizer._;
object DiscriminativePipeline extends ParserPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    oldWeights: File = null,
                    splitFactor:Int = 1);



  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params): Iterator[(String, SimpleChartParser[String, String, String])] = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    import params._;

    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.iterator.map(_._1) if initLexicon(t, ::).size > 50) yield t;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }

    val obj = new DiscrimObjective(featurizer, trainTrees.toIndexedSeq, xbarParser, openTags, closedWords) with ConfiguredLogging;

    // new LBFGS[Int,DenseVector[Double]](iterationsPerEval,5) with ConsoleLogging;
    val init = obj.initialWeightVector;
    val rand = new RandomizedGradientCheckingFunction(obj, 0.1);

    for( (state,iter) <- params.opt.iterations(new CachedBatchDiffFunction(obj),init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractParser(state.x);
       (iter + "", parser);
    }

  }
}

