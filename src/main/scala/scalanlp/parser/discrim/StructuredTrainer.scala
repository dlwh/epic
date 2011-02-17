package scalanlp.parser
package discrim

import scalanlp.trees.BinarizedTree
import scalanlp.trees.UnaryChainRemover.ChainReplacer

import scalanlp.util._
import scalanlp.trees._
import scalanlp.parser.InsideOutside.ExpectedCounts
;
import scalala.tensor.counters.LogCounters
import scalala.tensor.counters.Counters.DoubleCounter;
import scalanlp.parser.ParseChart.LogProbabilityParseChart
import projections.ProjectionIndexer
;
;

/**
 * 
 * @author dlwh
 */
object StructuredTrainer extends ParserTrainer {

  protected val paramManifest = manifest[DiscriminativeTrainer.Params]
  type Params = DiscriminativeTrainer.Params;

  def trainParser(trainTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer[String])],
                  devTrees: Seq[(BinarizedTree[String], Seq[String], SpanScorer[String])],
                  unaryReplacer: ChainReplacer[String], params: Params) = {
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees.iterator.map(tuple => (tuple._1,tuple._2)));

    val xbarParser = params.parser.optParser.getOrElse {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initBinaries),LogCounters.logNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);

    }
    val indexedProjections = ProjectionIndexer.simple(xbarParser.grammar.index);

    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);

    val openTags = Set.empty ++ {
      for(t <- initLexicon.activeKeys.map(_._1) if initLexicon(t).size > 50) yield t;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = DoubleCounter[String]();
      initLexicon.rows.foreach ( wordCounts += _._2 )
      wordCounts.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = new DiscrimObjective(featurizer, trainTrees.toIndexedSeq, xbarParser, openTags, closedWords);

    val initWeights = obj.initialWeightVector;

    val parser = obj.extractMaxParser(initWeights);
    for((goldTree,words,coarseFilter) <- trainTrees) {
      val guessTree = parser.bestParse(words,coarseFilter)
      val goldCounts = treeToExpectedCounts(parser,goldTree,words,coarseFilter);
      val guessCounts = treeToExpectedCounts(parser,guessTree,words,coarseFilter);

      val goldFeatures = expectedCountsToFeatureVector(obj.indexedFeatures, goldCounts);
      val guessFeatures = expectedCountsToFeatureVector(obj.indexedFeatures, guessCounts);


      for( (featureIndex, count) <- goldFeatures.activeElements) {
        // puts features elsewhere
      }



    }

    Iterator.single(("Structured",parser));


  }

  def treeToExpectedCounts[L,W](parser: ChartParser[L,L,W],
                                lt: BinarizedTree[L],
                                words: Seq[W],
                                spanScorer: SpanScorer[L]):ExpectedCounts[W] = {
    val g = parser.builder.grammar;
    val lexicon = parser.builder.lexicon;
    val expectedCounts = new ExpectedCounts[W](g)
    val t = lt.map(g.index);
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

  def expectedCountsToFeatureVector[L,W](indexedFeatures: FeatureIndexer[L,W], ecounts: ExpectedCounts[W]) = {
    import scalala.Scalala._;
    val result = indexedFeatures.mkSparseHashVector(0.0);

    // binaries
    for( (a,bvec) <- ecounts.binaryRuleCounts;
         (b,cvec) <- bvec;
         (c,v) <- cvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b,c) * v);
    }

    // unaries
    for( (a,bvec) <- ecounts.unaryRuleCounts;
         (b,v) <- bvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b) * v);
    }

    // lex
    for( (a,ctr) <- ecounts.wordCounts;
         (w,v) <- ctr) {
      result += (indexedFeatures.featuresFor(a,w) * v);
    }

    result;
  }


}
