package epic.parser.models

import epic.framework.{StandardExpectedCounts, Feature}
import breeze.util.Index
import epic.features._
import epic.parser.models.NeuralModel._
import breeze.linalg._
import epic.parser._
import epic.lexicon.Lexicon
import epic.trees.AnnotatedLabel
import breeze.numerics.sigmoid
import breeze.features.FeatureVector
import breeze.config.Help
import epic.trees.annotations.{Xbarize, TreeAnnotator, FilterAnnotations}
import java.io.File
import epic.util.CacheBroker
import epic.parser.projections.GrammarRefinements
import epic.trees.TreeInstance
import scala.runtime.ScalaRunTime
import breeze.linalg.operators.{CanAxpy, BinaryOp, OpMulMatrix}
import epic.dense.SigmoidTransform

/**
 * The neural model is really just a
 *
 * @author dlwh
 */
class NeuralModel[L, L2, W](baseModel: SpanModel[L, L2, W],
                            labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                            surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                            numOutputs: Int = 50,
                            initialFeatureVal: (Feature => Option[Double]) = { _ => None })  extends ParserModel[L, W] {

  def baseGrammar: BaseGrammar[L] = baseModel.baseGrammar
  def lexicon: Lexicon[L, W] = baseModel.lexicon

  type Inference = NeuralInference[L, W]

  val featureIndex = SegmentedIndex(
    baseModel.featureIndex,
    new SigmoidTransform(labelFeaturizer.index.size, numOutputs),
    new SigmoidTransform(numOutputs, surfaceFeaturizer.featureIndex.size),
    new SigmoidTransform(numOutputs, 1))

  println(featureIndex.indices.map(_.size))

  def initialValueForFeature(f: Feature): Double = initialFeatureVal(f).getOrElse(baseModel.initialValueForFeature(f) + math.random * 1E-5)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val Seq(_baseWeights: DenseVector[Double], outputWeights: DenseVector[Double], inputWeights: DenseVector[Double], inputBias: DenseVector[Double]) = featureIndex.shardWeights(weights)

    var baseWeights = _baseWeights
    if(iter >= 820) {
      zeroOutBaseModel =false
    } else {
      baseWeights = DenseVector.zeros[Double](_baseWeights.length)
    }
    iter += 1

    val baseInf = baseModel.inferenceFromWeights(baseWeights)
    val input = inputWeights.asDenseMatrix.reshape(numOutputs, surfaceFeaturizer.featureIndex.size)
    val output = outputWeights.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)
    new NeuralInference(baseInf, labelFeaturizer, surfaceFeaturizer, output, input, inputBias)

  }
  var iter = 0

  var zeroOutBaseModel = true


  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    val anchoring: Anchoring[L, W] = m.anchoring.refined.asInstanceOf[Anchoring[L, W]]
    m.visit(new NeuralModel.ExpectedCountsVisitor(anchoring, accum, scale, featureIndex.componentOffset(1), featureIndex.componentOffset(2), featureIndex.componentOffset(3)))
    accum.loss += scale * m.logPartition
  }

  override def expectedCountsToObjective(ecounts: NeuralModel[L, L2, W]#ExpectedCounts): (Double, DenseVector[Double]) = {
    if(zeroOutBaseModel)
      ecounts.counts(0 until baseModel.featureIndex.size) := 0.0

    super.expectedCountsToObjective(ecounts)
  }
}

case class NeuralInference[L, W](baseInference: AnnotatedParserInference[L, W],
                                 labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                 outputWeights: DenseMatrix[Double],
                                 inputWeights: DenseMatrix[Double],
                                 inputBias: DenseVector[Double]) extends ParserInference[L, W] {
  def goldMarginal(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]): Marginal = {
    import ti._
    val annotated = baseInference.annotator(tree, words)
    TreeMarginal(AugmentedGrammar.fromRefined(grammar), words, annotated)
  }

  def baseMeasure: CoreGrammar[L, W] = baseInference.baseMeasure

  val grammar = new NeuralModel.Grammar(baseInference.grammar, baseInference.featurizer, labelFeaturizer, surfaceFeaturizer, outputWeights, inputWeights, inputBias)
}

object NeuralModel {




  class Grammar[L, W](base: RefinedGrammar[L, W], baseFeaturizer: RefinedFeaturizer[L, W, Feature],
                      labelFeaturizer: RefinedFeaturizer[L, W, Feature], surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                      outputWeights: DenseMatrix[Double], inputWeights: DenseMatrix[Double],
                       inputBias: DenseVector[Double]) extends RefinedGrammar[L, W] {
    def grammar: BaseGrammar[L] = base.grammar

    def lexicon: Lexicon[L, W] = base.lexicon

    def anchor(words: IndexedSeq[W]) = {
      new Anchoring(base.anchor(words),
      baseFeaturizer.anchor(words),
      labelFeaturizer.anchor(words),
      surfaceFeaturizer.anchor(words),
      outputWeights, inputWeights, inputBias)
    }
  }


  class Anchoring[L, W](val baseAnchoring: RefinedAnchoring[L, W],
                        val baseFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val labelFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val surfaceFeaturizer: IndexedSplitSpanFeatureAnchoring[W],
                        val output: DenseMatrix[Double],
                        val input: DenseMatrix[Double],
                        val inputBias: DenseVector[Double]) extends RefinedAnchoring.StructureDelegatingAnchoring[L, W] {
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
      val act = input * new FeatureVector(surfaceFeatures)
//      act += inputBias
      sigmoid.inPlace(act)
      var score = 0.0
      var i = 0
      while(i < labelFeatures.length) {
        score += output.t(::, labelFeatures(i)) dot act
        i += 1
      }
      score
    }
  }

  private class ExpectedCountsVisitor[L, W](anchoring: NeuralModel.Anchoring[L, W],
                                            accum: StandardExpectedCounts[Feature],
                                            scale: Double,
                                            outputOffset: Int,
                                            inputOffset: Int,
                                            inputBiasOffset: Int) extends AnchoredVisitor[L] {
    import anchoring._

    val inputDerivatives = {
      accum.counts(inputOffset until (inputOffset + input.size))
        .asDenseMatrix
        .reshape(input.rows, input.cols, view = View.Require)
    }

    val outputDerivatives = {
      accum.counts(outputOffset until (outputOffset + output.size))
        .asDenseMatrix
        .reshape(output.rows, output.cols, view = View.Require)
    }

    val inputBiasDerivative = accum.counts(inputBiasOffset until (inputBiasOffset + inputBias.size))

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSplit(begin, split, end)
      val baseFeatures = baseFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), accum.counts)
      tallyDerivative(labelFeatures, surfFeats, score)
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), accum.counts)
      tallyDerivative(labelFeatures, surfFeats, score)
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
      val labelFeatures: Array[Int] = labelFeaturizer.featuresForSpan(begin, end, tag, ref)
      val surfFeats: Array[Int] = surfaceFeaturizer.featuresForSpan(begin, end)
      val baseFeatures = baseFeaturizer.featuresForSpan(begin, end, tag, ref)
      axpy(score * scale, new FeatureVector(baseFeatures), accum.counts)
      tallyDerivative(labelFeatures, surfFeats, score)
    }


    // for the neural features, the neural part of the activation is given by
    // (output * labelFeatures) dot sigmoid(input * surfaceFeatures + bias)
    // nabla output(::, lf) = sigmoid'(input * features + b) = sigmoid(input * features + b) :* (1-sigmoid(input * features+b))
    // d/d input(i, j) = (output(i) dot y) :* sigmoid'(input * features + b)(i)) * features(j)
    // d/d input(i, ::) = (\sum_lf output(i, lf)) dot (sigmoid'(input * features + b)(i)) * features)
    // d/d inputBias(i) = (output(i) dot y) :* sigmoid'(input * features + b)(i))
    def tallyDerivative(labelFeats: Array[Int], surfFeats: Array[Int], score: Double) {
      val surfaceFeatures = new FeatureVector(surfFeats)
      val act: DenseVector[Double] = input * surfaceFeatures
//      act += inputBias
      dsigmoidInPlace(act)
      // act is currently sigmoid'(input * features)
      val outputSum = DenseVector.zeros[Double](act.size)
      for(lf <- labelFeats) {
        axpy(score * scale, act, outputDerivatives.t(::, lf))
        outputSum += output.t(::, lf)
      }
      for (i <- 0 until input.rows) {
        val a: Double = score * scale * (outputSum(i) * act(i))
        axpy(a, surfaceFeatures, inputDerivatives.t(::, i))
//        inputBiasDerivative(i) += a
      }
    }

    def dsigmoidInPlace(act: DenseVector[Double]) {
      sigmoid.inPlace(act)
      act :*= (act - 1.0)
      act :*= -1.0
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
                            dummyFeats: Double = 0.5) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = NeuralModel[AnnotatedLabel, AnnotatedLabel, String]


  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String])(implicit broker: CacheBroker) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val xbarGrammar = constrainer.grammar
    val xbarLexicon = constrainer.lexicon
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val wf = WordFeaturizer.goodPOSTagFeaturizer(annWords)
    val span:SplitSpanFeaturizer[String] = {
      val dsl = new WordFeaturizer.DSL(annWords) with SurfaceFeaturizer.DSL with SplitSpanFeaturizer.DSL
      import dsl._

      ( clss(split)
        + distance[String](begin, end)
        + distance[String](begin, split)
        + distance[String](split, end)
//        + distance[String](begin, split) * distance[String](split,end)
        + clss(begin) + clss(end)
        + spanShape + clss(begin-1) + clss(end-1)
        + length
        + sent
//        + clss(begin-1) * clss(end) // word edges
//        +  clss(begin-1) * clss(end) * length
        )
    }
    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words})
    val surface = IndexedSplitSpanFeaturizer.fromData(span, annTrees)

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen={(x: AnnotatedLabel) => if(x.isIntermediate) Seq(x, SyntheticFeature) else Seq(x)})
    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
    featurizer,
      annotator,
      indexedRefinements,
      xbarGrammar,
      HashFeature.Relative(dummyFeats),
      trees)
    val labelFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements):RefinedFeaturizer[AnnotatedLabel, String, Feature]

    val featureCounter = readWeights(oldWeights)

    val base = new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get)
    new NeuralModel(base, labelFeaturizer, surface, 10)
  }
}