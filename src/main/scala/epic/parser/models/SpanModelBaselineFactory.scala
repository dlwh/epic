package epic.parser
package models

import java.io.File
import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.{IdentityTransform, AffineTransform, Transform}
import epic.features.SurfaceFeaturizer.SingleWordSpanFeaturizer
import epic.features._
import epic.framework.Feature
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import epic.util.{LRUCache, NotProvided, Optional}
import scala.io.Source
import scala.collection.mutable.HashSet

case class SpanModelBaselineFactory(annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                                    oldWeights: File = null,
                                    dummyFeats: Double = 0.5,
                                    commonWordThreshold: Int = 100,
                                    useSparseLfsuf: Boolean = true,
                                    useSparseBrown: Boolean = false,
                                    useMostSparseIndicators: Boolean = false) extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = SpanModel[AnnotatedLabel, AnnotatedLabel, String]

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    println("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = topology
    val xbarLexicon = lexicon

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)
    
//    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq
//    
//    val prodFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen=labelFeaturizer, rGen=ruleFeaturizer)
    
    
    var wf = SpanModelFactory.defaultPOSFeaturizer(annWords, useBrown = useSparseBrown)
    var span = SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false, useLfsuf = useSparseLfsuf, useBrown = useSparseBrown, useMostSparseIndicators = useMostSparseIndicators)
    span += new SingleWordSpanFeaturizer[String](wf)
    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words}, deduplicateFeatures = false)
    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)
    
    def sparseLabelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def sparseRuleFeaturizer(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq
    val sparseProdFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen=sparseLabelFeaturizer, rGen=sparseRuleFeaturizer)
    val sparseFeaturizer = IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
        indexedSurface,
        sparseProdFeaturizer,
        new ZeroRuleAndSpansFeaturizer(),
        annotator.latent,
        indexedRefinements,
        xbarGrammar,
        if(dummyFeats < 0) HashFeature.Absolute(-dummyFeats.toInt) else HashFeature.Relative(dummyFeats),
        filterUnseenFeatures = false,
        minFeatCount = 1,
        trainTrees)
    
    val featureCounter = this.readWeights(oldWeights)
    new SpanModel(sparseFeaturizer,
                  sparseFeaturizer.index,
                  annotator.latent,
                  constrainer,
                  topology,
                  lexicon,
                  refGrammar,
                  indexedRefinements)
  }
  
}