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
import java.io._
import scalala.library.Library
import Library.sum
import scalala.tensor.{Counter,::}

/**
 * 
 * @author dlwh
 */
class DiscrimObjective[L,W](feat: Featurizer[L,W],
                            trees: IndexedSeq[TreeInstance[L,W]],
                            coarseParser: ChartBuilder[LogProbabilityParseChart, L, W],
                            openTags: Set[L],
                            closedWords: Set[W])
        extends LatentDiscrimObjective[L,L,W](feat,trees,ProjectionIndexer.simple(coarseParser.grammar.index),coarseParser, openTags,closedWords) {

  override def treeToExpectedCounts(g: Grammar[L],
                                    lexicon: Lexicon[L,W],
                                    lt: BinarizedTree[L],
                                    words: Seq[W],
                                    spanScorer: SpanScorer[L]):ExpectedCounts[W] = {
    val expectedCounts = new ExpectedCounts[W](g)
    val t = lt.map(indexedFeatures.labelIndex);
    var score = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          expectedCounts.binaryRuleCounts.getOrElseUpdate(a).getOrElseUpdate(b)(c) += 1
          score += g.binaryRuleScore(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          expectedCounts.unaryRuleCounts.getOrElseUpdate(a)(b) += 1
          score += g.unaryRuleScore(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          expectedCounts.wordCounts.getOrElseUpdate(a)(w) += 1
          score += lexicon.wordScore(g.index.get(a), w);
      }
    }
    expectedCounts.logProb = score;
    expectedCounts;
  }

}


import scalanlp.optimize.FirstOrderMinimizer._;
object DiscriminativeTrainer extends ParserTrainer {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser,
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    oldWeights: File = null,
                    splitFactor:Int = 1);



  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params): Iterator[(String, ChartParser[String, String, String])] = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = new GenerativeGrammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
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

    val obj = new DiscrimObjective(featurizer, trainTrees.toIndexedSeq, xbarParser, openTags, closedWords) with ConsoleLogging;
    val optimizer = new StochasticGradientDescent[DenseVector[Double]](opt.alpha,maxIterations, opt.batchSize)
              with AdaptiveGradientDescent.L2Regularization[DenseVector[Double]]
              with ConsoleLogging {
        override val lambda = params.opt.adjustedRegularization(trainTrees.length);
      }

    // new LBFGS[Int,DenseVector[Double]](iterationsPerEval,5) with ConsoleLogging;
    val init = obj.initialWeightVector;
    val rand = new RandomizedGradientCheckingFunction(obj, 0.1);

    val log = Log.globalLog;
    for( (state,iter) <- optimizer.iterations(obj,init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
       val parser = obj.extractParser(state.x);
       (iter + "", parser);
    }

  }
}
