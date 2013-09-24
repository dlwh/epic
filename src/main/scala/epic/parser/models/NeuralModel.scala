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
import epic.trees.annotations.TreeAnnotator
import java.io.File
import epic.util.CacheBroker
import epic.parser.projections.GrammarRefinements
import epic.trees.TreeInstance
import epic.trees.annotations.FilterAnnotations

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
    new NeuralLayerFeatureIndex(labelFeaturizer.index.size, numOutputs),
    new NeuralLayerFeatureIndex(numOutputs, surfaceFeaturizer.featureIndex.size))

  println(featureIndex.indices.map(_.size))

  def initialValueForFeature(f: Feature): Double = initialFeatureVal(f).getOrElse(baseModel.initialValueForFeature(f) + math.random * 1E-5)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val Seq(baseWeights: DenseVector[Double], outputWeights: DenseVector[Double], inputWeights: DenseVector[Double]) = featureIndex.shardWeights(weights)
    val baseInf = baseModel.inferenceFromWeights(baseWeights)
    val input = inputWeights.asDenseMatrix.reshape(numOutputs, surfaceFeaturizer.featureIndex.size)
    val output = outputWeights.asDenseMatrix.reshape(labelFeaturizer.index.size, numOutputs)
    new NeuralInference(baseInf, labelFeaturizer, surfaceFeaturizer, output, input)

  }

  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    val anchoring: Anchoring[L, W] = m.anchoring.refined.asInstanceOf[Anchoring[L, W]]
    m.visit(new NeuralModel.ExpectedCountsVisitor(anchoring, accum, scale, featureIndex.componentOffset(1), featureIndex.componentOffset(2)))
    accum.loss += scale * m.logPartition
  }
}

case class NeuralInference[L, W](baseInference: AnnotatedParserInference[L, W],
                                 labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                 outputWeights: DenseMatrix[Double],
                                 inputWeights: DenseMatrix[Double]) extends ParserInference[L, W] {
  def goldMarginal(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]): Marginal = {
    import ti._
    val annotated = baseInference.annotator(tree, words)
    TreeMarginal(AugmentedGrammar.fromRefined(grammar), words, annotated)
  }

  def baseMeasure: CoreGrammar[L, W] = baseInference.baseMeasure

  val grammar = new NeuralModel.Grammar(baseInference.grammar, baseInference.featurizer, labelFeaturizer, surfaceFeaturizer, outputWeights, inputWeights)
}

object NeuralModel {
  case class NeuralFeature(output: Int, input: Int) extends Feature
  class NeuralLayerFeatureIndex(numOutputs: Int, numInputs: Int) extends Index[Feature] {
    def apply(t: Feature): Int = t match {
      case NeuralFeature(output, input) if output < numOutputs && input < numInputs && output > 0 && input > 0 =>
        output * numInputs + input
      case _ => -1
    }

    def unapply(i: Int): Option[Feature] = {
      if (i < 0 || i >= size) {
        None
      } else {
        Some(NeuralFeature(i/numInputs, i % numInputs))
      }
    }

    def pairs: Iterator[(Feature, Int)] = iterator zipWithIndex

    def iterator: Iterator[Feature] = Iterator.range(0, size) map (unapply) map (_.get)

    override def size: Int = numOutputs * numInputs

  }

  class Grammar[L, W](base: RefinedGrammar[L, W], baseFeaturizer: RefinedFeaturizer[L, W, Feature], labelFeaturizer: RefinedFeaturizer[L, W, Feature], surfaceFeaturizer: IndexedSplitSpanFeaturizer[W], outputWeights: DenseMatrix[Double], inputWeights: DenseMatrix[Double]) extends RefinedGrammar[L, W] {
    def grammar: BaseGrammar[L] = base.grammar

    def lexicon: Lexicon[L, W] = base.lexicon

    def anchor(words: IndexedSeq[W]) = {
      new Anchoring(base.anchor(words),
      baseFeaturizer.anchor(words),
      labelFeaturizer.anchor(words),
      surfaceFeaturizer.anchor(words),
      outputWeights, inputWeights)
    }
  }


  class Anchoring[L, W](val baseAnchoring: RefinedAnchoring[L, W],
                        val baseFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val labelFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val surfaceFeaturizer: IndexedSplitSpanFeatureAnchoring[W],
                        val output: DenseMatrix[Double],
                        val input: DenseMatrix[Double]) extends RefinedAnchoring.StructureDelegatingAnchoring[L, W] {
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
                                            inputOffset: Int) extends AnchoredVisitor[L] {
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
    // (output * labelFeatures) dot sigmoid(input * surfaceFeatures)
    // nabla output(::, lf) = sigmoid'(input * features) = sigmoid(input * features) :* (1-sigmoid(input * features))
    // d/d input(i, j) = (output(i) dot y) :* sigmoid'(input * features)(i)) * features(j)
    // d/d input(i, ::) = (\sum_lf output(i, lf)) dot (sigmoid'(input * features)(i)) * features)
    def tallyDerivative(labelFeats: Array[Int], surfFeats: Array[Int], score: Double) {
      val surfaceFeatures = new FeatureVector(surfFeats)
      val act: DenseVector[Double] = input * surfaceFeatures
      dsigmoidInPlace(act)
      // act is currently sigmoid'(input * features)
      val outputSum = DenseVector.zeros[Double](act.size)
      for(lf <- labelFeats) {
        axpy(score * scale, act, outputDerivatives.t(::, lf))
        outputSum += output.t(::, lf)
      }
      for (i <- 0 until input.rows) {
        axpy(score * scale * (outputSum(i) * act(i)), surfaceFeatures, inputDerivatives.t(::, i))
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
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
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
        + distance[String](begin, split)
        + distance[String](split, end)
        + distance[String](begin, split) * distance[String](split,end)
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

    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
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