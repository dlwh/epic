package scalanlp.parser.epic

import scalanlp.parser._
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import projections.GrammarProjections
import ParseChart.LogProbabilityParseChart
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import scalanlp.trees._

class KMModel[L,L3,W](featurizer: Featurizer[L3,W],
                      root: L3,
                      ann: (BinarizedTree[L],Seq[W])=>BinarizedTree[L3],
                      val projections: GrammarProjections[L,L3],
                      knownTagWords: Iterable[(L3,W)],
                      openTags: Set[L3],
                      closedWords: Set[W]) extends ParserModel[L,W] {
  type L2 = L3
  type Inference = DiscParserInference[L,L2, W]

  val indexedFeatures: FeatureIndexer[L2, W]  = FeatureIndexer(featurizer, knownTagWords, projections)

  def emptyCounts = ParserExpectedCounts[W](new TrueCounts(projections.rules.fineIndex.size,projections.labels.fineIndex.size))

  def numFeatures = indexedFeatures.index.size

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val grammar = FeaturizedGrammar(weights,indexedFeatures)
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures)
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb)

    new DiscParserInference(ann, parser, projections)
  }

  def extractParser(weights: DenseVector[Double]):ChartParser[L,L2,W] = {
    new SimpleChartParser(inferenceFromWeights(weights).builder,new MaxConstituentDecoder[L,L2,W](projections),projections)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    val counts = expectedCountsToFeatureVector(ecounts.trueCounts)
    (ecounts.loss,counts)
  }

  def expectedCountsToFeatureVector(ecounts: TrueCounts[W]) = {
    val result = indexedFeatures.mkDenseVector();

    def sumVectorIntoResults(vec: SparseVector[Double], v: Double) {
      var i = 0
      while (i < vec.nonzeroSize) {
        result(vec.data.indexAt(i)) += vec.data.valueAt(i) * v
        i += 1
      }
    }

    // rules
    for((r,v) <- ecounts.ruleCounts.pairsIteratorNonZero)
      sumVectorIntoResults(indexedFeatures.featuresFor(r),v)

    // lex
    for( (ctr,a) <- ecounts.wordCounts.iterator.zipWithIndex; (w,v) <- ctr.nonzero.pairs) {
      val vec = indexedFeatures.featuresFor(a,w)
      sumVectorIntoResults(vec, v)
    }

    result;
  }
}

case class DiscParserInference[L,L2,W](ann: (BinarizedTree[L],Seq[W])=>BinarizedTree[L2],
                                       builder: ChartBuilder[LogProbabilityParseChart,L2,W],
                                       projections: GrammarProjections[L,L2]) extends ParserInference[L,L2,W] {

  // E[T-z|T,params]
  def goldCounts(ti: TreeInstance[L,W], spanScorer: SpanScorer[L]) = {
    val tree = ti.tree
    val words = ti.words
    val g = builder.grammar
    val lexicon = builder.lexicon
    val annotated = ann(tree,words)

    val expectedCounts = new TrueCounts[W](g)
    var score = 0.0;
    for(t2 <- annotated.allChildren) {
      t2 match {
        case BinaryTree(a,bt@ Tree(b,_),Tree(c,_)) =>
          val r = g.index(BinaryRule(a,b,c))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
        case UnaryTree(a,Tree(b,_)) =>
          val r = g.index(UnaryRule(a,b))
          expectedCounts.ruleCounts(r) += 1
          score += g.ruleScore(r);
        case n@NullaryTree(a) =>
          val aI = g.labelIndex(a)
          val w = words(n.span.start);
          expectedCounts.wordCounts(aI)(w) += 1
          score += lexicon.wordScore(g.labelIndex.get(aI), w);
      }
    }
    expectedCounts.logProb = score;
    new ParserExpectedCounts(expectedCounts)
  }



}

case class KMModelFactory(parser: ParserParams.BaseParser[String],
                          pipeline: KMPipeline,
                          numStates: Int) extends ParserModelFactory[String, String] {
  type MyModel = KMModel[String,AnnotatedLabel,String]

  def make(trainTrees: IndexedSeq[TreeInstance[String, String]]):MyModel = {
    val transformed: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree,ti.words)
      TreeInstance(ti.id,t,ti.words)
    }.seq.toIndexedSeq

    val (initLexicon,initBinaries,initUnaries) = this.extractBasicCounts(transformed)

    val xbarParser = parser.optParser getOrElse {
      val (initLexicon,initBinaries,initUnaries) = this.extractBasicCounts(trainTrees)
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }
    val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
    val lexicon = new SignatureLexicon(initLexicon, EnglishWordClassGenerator, 5);
    val indexedProjections = GrammarProjections(xbarParser.grammar,grammar,{(_:AnnotatedLabel).label})

    val feat = new SumFeaturizer[AnnotatedLabel,String](new SimpleFeaturizer, new WordShapeFeaturizer(initLexicon))

    val identityProjections = GrammarProjections.identity(grammar)
    val openTags = determineOpenTags(initLexicon, identityProjections)
    val knownTagWords = determineKnownTags(lexicon, identityProjections)
    val closedWords = determineClosedWords(initLexicon)

    new KMModel[String,AnnotatedLabel,String](feat, transformed.head.label.label, pipeline, indexedProjections, knownTagWords, openTags, closedWords)
  }

}

