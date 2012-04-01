package scalanlp.parser.epic

import scalanlp.parser._
import features.{Feature, IndicatorFeature, WordShapeFeaturizer}
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import projections.GrammarProjections
import ParseChart.LogProbabilityParseChart
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import scalanlp.trees._
import java.io.File
import scalala.tensor.Counter
import scalala.tensor.mutable.Counter2

class KMModel[L,L3,W](featurizer: Featurizer[L3,W],
                      root: L3,
                      ann: (BinarizedTree[L],Seq[W])=>BinarizedTree[L3],
                      val projections: GrammarProjections[L,L3],
                      coarseBuilder: ChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                      knownTagWords: Iterable[(L3,W)],
                      openTags: Set[L3],
                      closedWords: Set[W],
                      initialFeatureVal: (Feature=>Option[Double]) = { _ => None}) extends ParserModel[L,W] {
  type L2 = L3
  type Inference = DiscParserInference[L,L2, W]

  val indexedFeatures: FeatureIndexer[L2, W]  = FeatureIndexer(featurizer, knownTagWords, projections)
  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def emptyCounts = ParserExpectedCounts[W](new TrueCounts(projections.rules.fineIndex.size,projections.labels.fineIndex.size))
  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val grammar = FeaturizedGrammar(weights,indexedFeatures)
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures)
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb)

    new DiscParserInference(ann, coarseBuilder, parser, projections)
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
                                       coarseBuilder: ChartBuilder[LogProbabilityParseChart,L,W],
                                       builder: ChartBuilder[LogProbabilityParseChart,L2,W],
                                       projections: GrammarProjections[L,L2]) extends ParserInference[L,L2,W] {

  // E[T-z|T,params]
  def goldCounts(ti: TreeInstance[L,W], spanScorer: SpanScorerFactor[L, W]) = {
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

case class KMModelFactory(baseParser: ParserParams.BaseParser,
                          pipeline: KMPipeline,
                          oldWeights: File = null) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = KMModel[AnnotatedLabel,AnnotatedLabel,String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]):MyModel = {
    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree,ti.words)
      TreeInstance(ti.id,t,ti.words)
    }.seq.toIndexedSeq

    val (initLexicon,initBinaries,initUnaries) = this.extractBasicCounts(transformed)

    val xbarParser = baseParser.xbarParser(trainTrees)
    val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
    val indexedProjections = GrammarProjections(xbarParser.grammar,grammar,{(_:AnnotatedLabel).baseAnnotatedLabel})

    val gen = new WordShapeFeaturizer(Library.sum(initLexicon))
    def labelFlattener(l: AnnotatedLabel) = {
      val basic = Seq(l, l.copy(features=Set.empty))
      basic map {IndicatorFeature(_)}
    }
    val feat = new SumFeaturizer[AnnotatedLabel,String](new RuleFeaturizer(labelFlattener _), new LexFeaturizer(gen, labelFlattener _))
    val xbarLexicon = Counter2[AnnotatedLabel, String, Double]()
    for( (t,w,v) <- initLexicon.triplesIterator) {
      xbarLexicon(t.baseAnnotatedLabel, w) += v
    }

    val openTags = determineOpenTags(xbarLexicon, indexedProjections)
    val knownTagWords = determineKnownTags(xbarParser.lexicon, indexedProjections)
    val closedWords = determineClosedWords(initLexicon)

    val featureCounter = if(oldWeights ne null) {
      scalanlp.util.readObject[Counter[Feature,Double]](oldWeights)
    } else {
      Counter[Feature,Double]()
    }
    new KMModel[AnnotatedLabel,AnnotatedLabel,String](feat, transformed.head.label.label, pipeline, indexedProjections, xbarParser, knownTagWords, openTags, closedWords, {featureCounter.get(_)})
  }

}

