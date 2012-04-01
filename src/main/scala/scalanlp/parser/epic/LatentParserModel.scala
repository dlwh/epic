package scalanlp.parser.epic

import scalanlp.parser._
import features.{Feature, IndicatorFeature, WordShapeFeaturizer}
import scalanlp.parser.InsideOutside.{ExpectedCounts=>TrueCounts}
import projections.{ProjectingSpanScorer, GrammarProjections}
import splitting.StateSplitting
import ParseChart.LogProbabilityParseChart
import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import java.io.File
import io.Source
import scalala.tensor.Counter
import scalanlp.trees.{BinarizedTree, AnnotatedLabel}

class LatentParserModel[L,L3,W](featurizer: Featurizer[L3,W],
                                root: L3,
                                reannotate: (BinarizedTree[L],Seq[W])=>BinarizedTree[L],
                                val projections: GrammarProjections[L,L3],
                                coarseBuilder: ChartBuilder[LogProbabilityParseChart, L, W],
                                knownTagWords: Iterable[(L3,W)],
                                openTags: Set[L3],
                                closedWords: Set[W],
                                initialFeatureVal: (Feature=>Option[Double]) = { _ => None}) extends ParserModel[L,W] {
  type L2 = L3
  type Inference = LatentParserInference[L,L2, W]

  val indexedFeatures: FeatureIndexer[L2, W]  = FeatureIndexer(featurizer, knownTagWords, projections)
  def featureIndex = indexedFeatures.index

  override def initialValueForFeature(f: Feature) = {
    initialFeatureVal(f) getOrElse (math.random * 1E-3)
  }

  def emptyCounts = ParserExpectedCounts[W](new TrueCounts(projections.rules.fineIndex.size,projections.labels.fineIndex.size))

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val grammar = FeaturizedGrammar(weights,indexedFeatures)
    val lexicon = new FeaturizedLexicon(openTags, closedWords, weights, indexedFeatures)
    val parser = new CKYChartBuilder[LogProbabilityParseChart,L2,W](root, lexicon, grammar, ParseChart.logProb)

    new LatentParserInference(reannotate, coarseBuilder, parser, projections)
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

case class LatentParserInference[L,L2,W](reannotate: (BinarizedTree[L],Seq[W])=>BinarizedTree[L],
                                         coarseBuilder: ChartBuilder[LogProbabilityParseChart, L, W],
                                         builder: ChartBuilder[LogProbabilityParseChart,L2,W],
                                         projections: GrammarProjections[L,L2]) extends ParserInference[L,L2,W] {

  // E[T-z|T,params]
  def goldCounts(ti: TreeInstance[L,W], spanScorer: SpanScorerFactor[L, W]) = {
    val reannotated = reannotate(ti.tree, ti.words)
    val projected = new ProjectingSpanScorer(projections,spanScorer.scorer)
    val ecounts = new StateSplitting(
      builder.grammar,
      builder.lexicon).expectedCounts(reannotated.map(projections.labels.refinementsOf _),ti.words,projected)

    new ParserExpectedCounts(ecounts)
  }

}

case class LatentParserModelFactory(baseParser: ParserParams.BaseParser,
                                    substates: File = null,
                                    numStates: Int = 2,
                                    oldWeights: File = null,
                                    splitFactor: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = LatentParserModel[AnnotatedLabel,(AnnotatedLabel,Int),String]

  def split(x: AnnotatedLabel, counts: Map[AnnotatedLabel,Int], numStates: Int) = {
    for(i <- 0 until counts.getOrElse(x,numStates)) yield (x,i)
  }

  def unsplit(x: (AnnotatedLabel,Int)) = x._1

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val (xbarLexicon,xbarBinaries,xbarUnaries) = this.extractBasicCounts(trainTrees.map(_.mapLabels(_.baseAnnotatedLabel)))

    val xbarParser = baseParser.xbarParser(trainTrees)

    val substateMap = if(substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for( line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarParser.root -> 1)
    } else {
      Map(xbarParser.root -> 1)
    }

    val gen = new WordShapeFeaturizer(Library.sum(xbarLexicon))
    def labelFlattener(l: (AnnotatedLabel,Int)) = {
      val basic = Seq(l)
      basic map(IndicatorFeature)
    }
    val feat = new SumFeaturizer[(AnnotatedLabel,Int),String](new RuleFeaturizer(labelFlattener _), new LexFeaturizer(gen, labelFlattener _))
    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:AnnotatedLabel,substateMap, numStates), unsplit)

    val openTags = determineOpenTags(xbarLexicon, indexedProjections)
    val knownTagWords = determineKnownTags(xbarParser.lexicon, indexedProjections)
    val closedWords = determineClosedWords(xbarLexicon)

    val featureCounter = if(oldWeights ne null) {
      val baseCounter = scalanlp.util.readObject[Counter[Feature,Double]](oldWeights)
      baseCounter
    } else {
      Counter[Feature,Double]()
    }

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    new LatentParserModel[AnnotatedLabel,(AnnotatedLabel,Int),String](feat,
                                                                      AnnotatedLabel.TOP -> 0,
                                                                      reannotate,
                                                                      indexedProjections,
                                                                      xbarParser,
                                                                      knownTagWords,
                                                                      openTags,
                                                                      closedWords,
                                                                      {featureCounter.get(_)})
  }
}



