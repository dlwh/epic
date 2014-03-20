package epic.parser.models

import breeze.collection.mutable.TriangularArray
import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import epic.dense._
import epic.features.SplitSpanFeaturizer.ZeroSplitSpanFeaturizer
import epic.features._
import epic.framework.{StandardExpectedCounts, Feature}
import epic.lexicon.Lexicon
import epic.parser._
import epic.trees._
import epic.trees.annotations.{Xbarize, TreeAnnotator}
import java.io.File
import epic.parser.projections.GrammarRefinements

/**
 * The neural model is really just a
 *
 * @author dlwh
 *
 */
class NeuralModel[L, L2, W](baseModel: SpanModel[L, L2, W],
                            labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                            surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                            transform: Transform[FeatureVector, DenseVector[Double]],
                            numOutputs: Int,
                            initialFeatureVal: (Feature => Option[Double]) = { _ => None })  extends StandardExpectedCounts.Model[TreeInstance[L, W]] with ParserExtractable[L, W] {

  def baseGrammar: BaseGrammar[L] = baseModel.baseGrammar
  def lexicon: Lexicon[L, W] = baseModel.lexicon


  type Scorer = RefinedAnchoring[L, W]
  type Marginal = ParseMarginal[L, W]
  type Inference = NeuralInference[L, L2, W]


  def extractParser(weights: DenseVector[Double]) = {
    val inf = inferenceFromWeights(weights)
    Parser(inf.baseMeasure, inf.grammar, ChartDecoder[L, W]())
  }

  val featureIndex = SegmentedIndex( baseModel.featureIndex,
    AffineTransform(labelFeaturizer.index.size, numOutputs, includeBias = false).index,
    transform.index)

  def initialValueForFeature(f: Feature): Double = initialFeatureVal(f).getOrElse(baseModel.initialValueForFeature(f) + math.random * 1E-5)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val Seq(baseWeights: DenseVector[Double], outputWeights: DenseVector[Double], transWeights: DenseVector[Double]) = featureIndex.shardWeights(weights)

    val baseInf = baseModel.inferenceFromWeights(baseWeights)
    val output = outputWeights.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)
    val layer = transform.extractLayer(transWeights)
    new NeuralInference(baseInf, labelFeaturizer, surfaceFeaturizer, output, layer)
  }


  def accumulateCounts(anchoring: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    val Seq(baseDerivatives: DenseVector[Double], outputDerivatives: DenseVector[Double], inputDerivatives: DenseVector[Double]) = featureIndex.shardWeights(accum.counts)
    m.visit(new NeuralModel.ExpectedCountsVisitor(anchoring.asInstanceOf[NeuralModel.Anchoring[L, W]], scale, baseDerivatives,
      inputDerivatives, outputDerivatives.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)))
    accum.loss += scale * m.logPartition
  }

  override def expectedCountsToObjective(ecounts: NeuralModel[L, L2, W]#ExpectedCounts): (Double, DenseVector[Double]) = {
    super.expectedCountsToObjective(ecounts)
  }
}

case class NeuralInference[L, L2, W](baseInference: LatentParserInference[L, L2, W],
                                 labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                 lastLayerWeights: DenseMatrix[Double],
                                 layer: Transform[FeatureVector, DenseVector[Double]]#Layer) extends ParserInference[L, W] {
  override def forTesting = copy(baseInference.forTesting, labelFeaturizer.forTesting)


  def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]): Marginal = {
    import ti._

    val annotated = baseInference.annotator(tree, words).map(_.map(baseInference.projections.labels.localize))

    val product = AugmentedAnchoring.fromRefined(grammar.anchor(words))
    LatentTreeMarginal(product, annotated)
  }

  def baseMeasure: CoreGrammar[L, W] = baseInference.baseMeasure

  val grammar = new NeuralModel.Grammar(baseInference.grammar, baseInference.featurizer, labelFeaturizer, surfaceFeaturizer, lastLayerWeights, layer)
}

object NeuralModel {


  class Grammar[L, W](base: RefinedGrammar[L, W], baseFeaturizer: RefinedFeaturizer[L, W, Feature],
                      labelFeaturizer: RefinedFeaturizer[L, W, Feature], surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                      lastLayerWeights: DenseMatrix[Double], layer: Transform[FeatureVector, DenseVector[Double]]#Layer) extends RefinedGrammar[L, W] {
    def grammar: BaseGrammar[L] = base.grammar

    def lexicon: Lexicon[L, W] = base.lexicon

    def anchor(words: IndexedSeq[W]) = {
      new Anchoring(base.anchor(words),
      baseFeaturizer.anchor(words),
      labelFeaturizer.anchor(words),
      surfaceFeaturizer.anchor(words),
      lastLayerWeights, layer)
    }
  }


  /** Not thread safe; make a difference anchoring for each thread */
  class Anchoring[L, W](val baseAnchoring: RefinedAnchoring[L, W],
                        val baseFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val labelFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val surfaceFeaturizer: IndexedSplitSpanFeatureAnchoring[W],
                        val lastLayer: DenseMatrix[Double],
                        val layer: Transform[FeatureVector, DenseVector[Double]]#Layer) extends RefinedAnchoring.StructureDelegatingAnchoring[L, W] {
    override def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreSpan(begin, end, label, ref)
      if(lastLayer.size != 0 && base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForSpan(begin, end, label, ref),
          surfaceFeaturizer.featuresForSpan(begin, end),
          begin, begin, end)
      }
      base
    }

    override def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreBinaryRule(begin, split, end, rule, ref)
      if(lastLayer.size != 0 && base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref),
          surfaceFeaturizer.featuresForSplit(begin, split, end),
          begin, split, end)
      }
      base
    }

    override def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreUnaryRule(begin, end, rule, ref)
      if(lastLayer.size != 0 && base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForUnaryRule(begin, end, rule, ref),
          surfaceFeaturizer.featuresForSpan(begin, end),
          begin, begin, end)
      }
      base
    }

    // if split == begin, then we're just scoring the span.
    def score(labelFeatures: Array[Int], surfaceFeatures: Array[Int], begin: Int, split: Int, end: Int):Double = {
      val activations = cachedActivations(surfaceFeatures, begin, split, end)
      val labelCombination = lastLayer.t * new FeatureVector(labelFeatures)
      activations dot labelCombination
    }

    // if split == begin, then we're just scoring the span.
    private[NeuralModel] def cachedActivations(surfaceFeatures: Array[Int], begin: Int, split: Int, end: Int) = {
      if(TriangularArray.index(begin, end) != lastCell) {
        java.util.Arrays.fill(_cachedActivations.asInstanceOf[Array[AnyRef]], null)
        lastCell = TriangularArray.index(begin, end)
      }


      val off = split - begin
      var res  = _cachedActivations(off)
      if(res eq null) {
        res = layer.activations(new FeatureVector(surfaceFeatures))
        _cachedActivations(off) = res
      }

      res
    }

    private var lastCell = -1
    private val _cachedActivations = new Array[DenseVector[Double]](words.length)
  }

  private class ExpectedCountsVisitor[L, W](anchoring: NeuralModel.Anchoring[L, W],
                                            scale: Double,
                                            baseDeriv: DenseVector[Double],
                                            inputDerivatives: DenseVector[Double],
                                            outputDerivatives: DenseMatrix[Double]) extends AnchoredVisitor[L] {
    import anchoring._


    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSplit(begin, split, end)
      val baseFeatures = baseFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), baseDeriv)
      if(lastLayer.size != 0)
      tallyDerivative(labelFeatures, surfFeats, score, begin, split, end)
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), baseDeriv)
      if(lastLayer.size != 0)
      tallyDerivative(labelFeatures, surfFeats, score, begin, begin, end)
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForSpan(begin, end, tag, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForSpan(begin, end, tag, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), baseDeriv)
      if(lastLayer.size != 0)
      tallyDerivative(labelFeatures, surfFeats, score, begin, begin, end)
    }


    // for the neural features, the neural part of the activation is given by
    // (output.t * labelFeatures) dot layer(surfaceFeatures; \theta)
    // == (labelFeatures.t * output) * layer(surfaceFeatures)
    // (\nabla output)(lf, ::) = labelFeatures(lf) * layer(surfaceFeatures)
    // (\nabla \theta) = (output.t * labelFeatures) * \nabla layer(surfaceFeatures)
    // everything is scaled by score * scale
    def tallyDerivative(labelFeats: Array[Int], surfFeats: Array[Int], score: Double, begin: Int, split: Int, end: Int): Unit =  {
      val activations = anchoring.cachedActivations(surfFeats, begin, split, end)
      val labelCombination = lastLayer.t * new FeatureVector(labelFeats)

      for(lf <- labelFeats) {
        axpy(score * scale, activations, outputDerivatives.t(::, lf))
      }

      layer.tallyDerivative(inputDerivatives, labelCombination *= (score * scale), new FeatureVector(surfFeats))

    }


  }
}


case class NeuralModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses no annotations.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = Xbarize(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                            dummyFeats: Double = 0.0,
                            numOutputs: Int = 100,
                            numHidden: Int = 100,
                            useIdentitySurfaceFeatures: Boolean = false) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = NeuralModel[AnnotatedLabel, AnnotatedLabel, String]


  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String]) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val xbarGrammar = constrainer.grammar
    val xbarLexicon = constrainer.lexicon
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)


    val wf = {//WordFeaturizer.goodPOSTagFeaturizer(annWords)
      val dsl = new WordFeaturizer.DSL(annWords)
      import dsl._
      unigrams(word, 1) + suffixes() + prefixes()
    }


    val span:SplitSpanFeaturizer[String] = {
      val dsl = new WordFeaturizer.DSL(annWords) with SurfaceFeaturizer.DSL with SplitSpanFeaturizer.DSL
      import dsl._

      val baseCat = lfsuf

      val leftOfSplit = ((baseCat)(-1)apply (split))

      (baseCat(begin-1) + baseCat(begin) + leftOfSplit + baseCat(split) + baseCat(end-1) + baseCat(end))
    }
    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words})
    val surface = IndexedSplitSpanFeaturizer.fromData(span, annTrees)

    def labelFeatGen(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
        def ruleFeatGen(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq
    def parentRuleFeatGen(r: Rule[AnnotatedLabel]) = r match {
      case BinaryRule(a,b,c) => Set(a, a.baseAnnotatedLabel).toSeq
      case UnaryRule(a, b, _) => Set(a, a.baseAnnotatedLabel).toSeq
    }

    val featureCounter = readWeights(oldWeights)

    val base = if(useIdentitySurfaceFeatures) {
      val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, labelFeatGen, ruleFeatGen)
      val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
        surface,
        featurizer,
        new ZeroRuleAndSpansFeaturizer,
        annotator.latent,
        indexedRefinements,
        xbarGrammar,
        HashFeature.Relative(dummyFeats),
        trees)
      new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator.latent, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get)
    } else {
      val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, labelFeatGen, ruleFeatGen)
      val surf0 = IndexedSplitSpanFeaturizer.fromData(new ZeroSplitSpanFeaturizer, annTrees)
      val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
        surf0,
        featurizer,
        new ZeroRuleAndSpansFeaturizer,
        annotator.latent,
        indexedRefinements,
        xbarGrammar,
        HashFeature.Relative(dummyFeats),
        trees)
      new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator.latent, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get)

    }

    val labelFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements):RefinedFeaturizer[AnnotatedLabel, String, Feature]


    val transform = new TanhTransform(new AffineTransform(numOutputs, numHidden, new TanhTransform[FeatureVector](numHidden, surface.featureIndex.size, true)))
    new NeuralModel(base, labelFeaturizer, surface, transform, numOutputs)
  }
}