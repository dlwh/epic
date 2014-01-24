package epic.parser.models

import epic.framework.{StandardExpectedCounts, Feature}
import breeze.util.Index
import epic.features._
import epic.parser.models.NeuralModel._
import breeze.linalg._
import epic.parser._
import epic.lexicon.Lexicon
import epic.trees.{Rule, AnnotatedLabel, TreeInstance}
import breeze.numerics.sigmoid
import breeze.features.FeatureVector
import breeze.config.Help
import epic.trees.annotations.{Xbarize, TreeAnnotator, FilterAnnotations}
import java.io.File
import epic.util.CacheBroker
import epic.parser.projections.GrammarRefinements
import scala.runtime.ScalaRunTime
import epic.dense.{AffineTransform, Transform, SigmoidTransform}

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
                            initialFeatureVal: (Feature => Option[Double]) = { _ => None })  extends ParserModel[L, W] {

  def baseGrammar: BaseGrammar[L] = baseModel.baseGrammar
  def lexicon: Lexicon[L, W] = baseModel.lexicon

  type Inference = NeuralInference[L, W]

  val featureIndex = SegmentedIndex( baseModel.featureIndex,
    AffineTransform(labelFeaturizer.index.size, numOutputs, includeBias = false).index,
    transform.index)
  private val layerOffset = featureIndex.componentOffset(2)
  private val lastLayerOffset = featureIndex.componentOffset(1)

  println(featureIndex.indices.map(_.size))

  def initialValueForFeature(f: Feature): Double = initialFeatureVal(f).getOrElse(baseModel.initialValueForFeature(f) + math.random * 1E-5)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val Seq(baseWeights: DenseVector[Double], outputWeights: DenseVector[Double], transWeights: DenseVector[Double]) = featureIndex.shardWeights(weights)

    val baseInf = baseModel.inferenceFromWeights(baseWeights)
    val output = outputWeights.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)
    val layer = transform.extractLayer(transWeights)
    new NeuralInference(baseInf, labelFeaturizer, surfaceFeaturizer, output, layer)
  }



  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    val anchoring: Anchoring[L, W] = m.anchoring.refined.asInstanceOf[Anchoring[L, W]]
    val inputDerivatives = accum.counts(layerOffset until (layerOffset + transform.index.size))
    val outputDerivatives = accum.counts(lastLayerOffset until (lastLayerOffset + anchoring.lastLayer.size))
    m.visit(new NeuralModel.ExpectedCountsVisitor(anchoring, scale, accum.counts(0 until baseModel.featureIndex.size),
      inputDerivatives, outputDerivatives.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)))
    accum.loss += scale * m.logPartition
  }

  override def expectedCountsToObjective(ecounts: NeuralModel[L, L2, W]#ExpectedCounts): (Double, DenseVector[Double]) = {
    super.expectedCountsToObjective(ecounts)
  }
}

case class NeuralInference[L, W](baseInference: AnnotatedParserInference[L, W],
                                 labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                 lastLayerWeights: DenseMatrix[Double],
                                 layer: Transform[FeatureVector, DenseVector[Double]]#Layer) extends ParserInference[L, W] {
  def goldMarginal(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]): Marginal = {
    import ti._
    val annotated = baseInference.annotator(tree, words)
    TreeMarginal(AugmentedGrammar.fromRefined(grammar), words, annotated)
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


  class Anchoring[L, W](val baseAnchoring: RefinedAnchoring[L, W],
                        val baseFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val labelFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val surfaceFeaturizer: IndexedSplitSpanFeatureAnchoring[W],
                        val lastLayer: DenseMatrix[Double],
                        val layer: Transform[FeatureVector, DenseVector[Double]]#Layer) extends RefinedAnchoring.StructureDelegatingAnchoring[L, W] {
    override def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreSpan(begin, end, label, ref)
      if(base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForSpan(begin, end, label, ref), surfaceFeaturizer.featuresForSpan(begin, end))
      }
      base
    }

    override def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreBinaryRule(begin, split, end, rule, ref)
      if(base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref), surfaceFeaturizer.featuresForSplit(begin, split, end))
      }
      base
    }

    override def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = baseAnchoring.scoreUnaryRule(begin, end, rule, ref)
      if(base != Double.NegativeInfinity) {
        base += score(labelFeaturizer.featuresForUnaryRule(begin, end, rule, ref), surfaceFeaturizer.featuresForSpan(begin, end))
      }
      base
    }

    def score(labelFeatures: Array[Int], surfaceFeatures: Array[Int]):Double = {
      val activations = layer.activations(new FeatureVector(surfaceFeatures))
      val labelCombination = lastLayer.t * new FeatureVector(labelFeatures)
      activations dot labelCombination
    }
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
      tallyDerivative(labelFeatures, surfFeats, score)
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), baseDeriv)
      tallyDerivative(labelFeatures, surfFeats, score)
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForSpan(begin, end, tag, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForSpan(begin, end, tag, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), baseDeriv)
      tallyDerivative(labelFeatures, surfFeats, score)
    }


    // for the neural features, the neural part of the activation is given by
    // (output.t * labelFeatures) dot layer(surfaceFeatures; \theta)
    // == (labelFeatures.t * output) * layer(surfaceFeatures)
    // (\nabla output)(lf, ::) = labelFeatures(lf) * layer(surfaceFeatures)
    // (\nabla \theta) = (output.t * labelFeatures) * \nabla layer(surfaceFeatures)
    // everything is scaled by score * scale
    def tallyDerivative(labelFeats: Array[Int], surfFeats: Array[Int], score: Double): Unit =  {
      val activations = layer.activations(new FeatureVector(surfFeats))
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
                            numHidden: Int = 100) extends ParserModelFactory[AnnotatedLabel, String] {
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

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, labelFeatGen, ruleFeatGen)
    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
      featurizer,
      new ZeroRuleAndSpansFeaturizer,
      annotator,
      indexedRefinements,
      xbarGrammar,
      HashFeature.Relative(dummyFeats),
      trees)
    val labelFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements):RefinedFeaturizer[AnnotatedLabel, String, Feature]

    val featureCounter = readWeights(oldWeights)

    val base = new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get)
    val transform = new SigmoidTransform(new AffineTransform(numOutputs, numHidden, new SigmoidTransform[FeatureVector](numHidden, surface.featureIndex.size, true)))
    new NeuralModel(base, labelFeaturizer, surface, transform, numOutputs)
  }
}