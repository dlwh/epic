package epic.parser.models

import breeze.collection.mutable.TriangularArray
import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import epic.dense._
import epic.features.SplitSpanFeaturizer.ZeroSplitSpanFeaturizer
import epic.features.SurfaceFeaturizer.SingleWordSpanFeaturizer
import epic.features._
import epic.framework.{StandardExpectedCounts, Feature}
import epic.lexicon.Lexicon
import epic.parser
import epic.parser._
import epic.trees._
import epic.trees.annotations.{Xbarize, TreeAnnotator}
import java.io.File
import epic.parser.projections.GrammarRefinements
import epic.constraints.ChartConstraints.Factory
import epic.constraints.ChartConstraints
import epic.util.Optional

case class NeuralModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses no annotations.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = Xbarize(),
                            @Help(text="Old weights to initialize with. Optional")
                            commonWordThreshold: Int = 100,
                            oldWeights: File = null,
                            numOutputs: Int = 100,
                            numHidden: Int = 100,
                            posFeaturizer: Optional[WordFeaturizer[String]] = None,
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = None,
                            useIdentitySurfaceFeatures: Boolean = false) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = TransformModel[AnnotatedLabel, AnnotatedLabel, String]


  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String], constrainer: Factory[AnnotatedLabel, String]): MyModel = {

    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    println("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = topology
    val xbarLexicon = lexicon

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)

    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))
    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))

    span += new SingleWordSpanFeaturizer[String](wf)

    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    val transform = new AffineTransform(
      featurizer.index.size,
      numOutputs,
      new TanhTransform(
        new AffineTransform(numOutputs, numHidden,
        new TanhTransform[FeatureVector](numHidden, indexedSurface.featureIndex.size, true)))
      )

    new TransformModel(
      annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar, indexedRefinements,
      featurizer, indexedSurface,
      transform
    )
  }
}