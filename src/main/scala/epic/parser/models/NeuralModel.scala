package epic.parser.models

import epic.framework.{StandardExpectedCounts, Feature}
import breeze.util.Index
import epic.features._
import epic.parser.models.NeuralModel._
import breeze.linalg._
import epic.parser._
import epic.lexicon.Lexicon
import epic.trees.{AnnotatedLabel, TreeInstance}
import breeze.numerics.sigmoid
import breeze.features.FeatureVector
import breeze.config.Help
import epic.trees.annotations.{FilterAnnotations, TreeAnnotator}
import java.io.File
import epic.util.CacheBroker
import epic.parser.projections.GrammarRefinements
import epic.trees.TreeInstance
import scala.Some
import epic.parser.models.NeuralModel.NeuralFeature
import epic.parser.ExpectedCounts
import epic.parser.models.AnnotatedParserInference
import epic.trees.annotations.FilterAnnotations
import epic.parser.models.NeuralInference

/**
 * The neural model is really just a
 *
 * @author dlwh
 */
class NeuralModel[L, L2, W](baseModel: SpanModel[L, L2, W],
                            numOutputs: Int = 50,
                            initialFeatureVal: (Feature => Option[Double]) = { _ => None })  extends ParserModel[L, W] {

  def baseGrammar: BaseGrammar[L] = baseModel.baseGrammar
  def lexicon: Lexicon[L, W] = baseModel.lexicon

  type Inference = NeuralInference[L, W]

  val featureIndex = SegmentedIndex(baseModel.featureIndex, new NeuralLayerFeatureIndex(1, numOutputs), new NeuralLayerFeatureIndex(numOutputs, baseModel.featureIndex.size))

  def initialValueForFeature(f: Feature): Double = initialFeatureVal(f).getOrElse(baseModel.initialValueForFeature(f) + math.random * 1E-5)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val Seq(baseWeights, outputWeights, inputWeights: DenseVector[Double]) = featureIndex.shardWeights(weights)
    val baseInf = baseModel.inferenceFromWeights(baseWeights)
    new NeuralInference(baseInf, outputWeights, inputWeights.asDenseMatrix.reshape(numOutputs, baseModel.featureIndex.size))

  }

  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    val anchoring: Anchoring[L, W] = m.anchoring.refined.asInstanceOf[Anchoring[L, W]]
    m.visit(new NeuralModel.ExpectedCountsVisitor(anchoring, accum, scale, featureIndex.componentOffset(1), featureIndex.componentOffset(2)))
    accum.loss += scale * m.logPartition
  }
}

case class NeuralInference[L, W](baseInference: AnnotatedParserInference[L, W], outputWeights: DenseVector[Double], inputWeights: DenseMatrix[Double]) extends ParserInference[L, W] {
  def goldMarginal(ti: TreeInstance[L, W], aug: CoreAnchoring[L, W]): Marginal = {
    import ti._
    val annotated = baseInference.annotator(tree, words)
    TreeMarginal(AugmentedGrammar.fromRefined(grammar), words, annotated)
  }

  def baseMeasure: CoreGrammar[L, W] = baseInference.baseMeasure

  val grammar = new NeuralModel.Grammar(baseInference.grammar, baseInference.featurizer, outputWeights, inputWeights)
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

  class Grammar[L, W](base: RefinedGrammar[L, W], baseFeaturizer: RefinedFeaturizer[L, W, Feature], outputWeights: DenseVector[Double], inputWeights: DenseMatrix[Double]) extends RefinedGrammar[L, W] {
    def grammar: BaseGrammar[L] = base.grammar

    def lexicon: Lexicon[L, W] = base.lexicon

    def anchor(words: IndexedSeq[W]) = new Anchoring(base.anchor(words), baseFeaturizer.anchor(words), outputWeights, inputWeights)
  }


  class Anchoring[L, W](val baseAnchoring: RefinedAnchoring[L, W],
                        val baseFeaturizer: RefinedFeaturizer[L, W, Feature]#Anchoring,
                        val output: DenseVector[Double],
                        val input: DenseMatrix[Double]) extends RefinedAnchoring.StructureDelegatingAnchoring[L, W] {
    override def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
      var base = super.scoreSpan(begin, end, label, ref)
      if(base != Double.NegativeInfinity) {
        base += score(baseFeaturizer.featuresForSpan(begin, end, label, ref))
      }
      base
    }

    override def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = super.scoreBinaryRule(begin, split, end, rule, ref)
      if(base != Double.NegativeInfinity) {
        base += score(baseFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref))
      }
      base
    }

    override def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
      var base = super.scoreUnaryRule(begin, end, rule, ref)
      if(base != Double.NegativeInfinity) {
        base += score(baseFeaturizer.featuresForUnaryRule(begin, end, rule, ref))
      }
      base
    }

    def score(features: Array[Int]):Double = {
      val act = input * new FeatureVector(features)
      sigmoid.inPlace(act)
      output dot act
    }
  }

  private class ExpectedCountsVisitor[L, W](anchoring: NeuralModel.Anchoring[L, W],
                                            accum: StandardExpectedCounts[Feature],
                                            scale: Double,
                                            outputOffset: Int,
                                            inputOffset: Int) extends AnchoredVisitor[L] {
    import anchoring._

    val rowsOfInputDerivative = Array.tabulate(output.size){ i =>
      val startOffset = inputOffset + input.cols * i
      val endOffset = startOffset + input.cols
      accum.counts(startOffset until endOffset)
    }

    val outputDerivative = accum.counts(outputOffset until (outputOffset + output.size))

    def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val feats: Array[Int] = baseFeaturizer.featuresForBinaryRule(begin, split, end, rule, ref)
      tallyDerivative(feats, score)
    }

    def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
      val feats: Array[Int] = anchoring.baseFeaturizer.featuresForUnaryRule(begin, end, rule, ref)
      tallyDerivative(feats, score)
    }

    def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
      val feats: Array[Int] = anchoring.baseFeaturizer.featuresForSpan(begin, end, tag, ref)
      tallyDerivative(feats, score)
    }


    // for the neural features, the neural part of the activation is given by
    // output dot sigmoid(input * features)
    // nabla output = sigmoid'(input * features) = sigmoid(input * features) :* (1-sigmoid(input * features))
    // d/d input(i, j) = (output(i) * sigmoid'(input * features)(i)) * features(j)
    // d/d input(i, ::) = (output(i) * sigmoid'(input * features)(i)) * features
    // d/d input = (output :* sigmoid'(input * features)) * features.t
    def tallyDerivative(feats: Array[Int], score: Double) {
      val features = new FeatureVector(feats)
      axpy(score * scale, features, accum.counts)
      val act: DenseVector[Double] = input * features
      dsigmoidInPlace(act)
      // act is currently sigmoid'(input * features)
      axpy(score * scale, act, outputDerivative)
      for (i <- 0 until output.size) {
        axpy(score * scale * (output(i) * act(i)), features, rowsOfInputDerivative(i))
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

    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
      annotator,
      indexedRefinements,
      xbarGrammar,
      HashFeature.Relative(dummyFeats),
      trees)

    val featureCounter = readWeights(oldWeights)

    val base = new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get(_))
    new NeuralModel(base, 20)
  }
}