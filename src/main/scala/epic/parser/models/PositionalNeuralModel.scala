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
import epic.dense.TanhTransform
import epic.dense.ReluTransform
import epic.dense.CubeTransform
import epic.dense.AffineTransformDense
import edu.berkeley.nlp.nn.Word2Vec
import scala.collection.mutable.HashMap
import epic.dense.Word2VecSurfaceFeaturizer
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.dense.CachingLookupAndAffineTransformDense

/**
 * TODO
 *
 * @author dlwh
 **/
case class PositionalNeuralModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
                            dummyFeats: Double = 0.5,
                            commonWordThreshold: Int = 100,
                            ngramCountThreshold: Int = 5,
                            useGrammar: Boolean = true,
                            useChildFeats: Boolean = false,
                            useSparseFeatures: Boolean = false,
                            nonLinType: String = "tanh",
                            useCube: Boolean = false,
                            numHidden: Int = 100,
                            numHiddenLayers: Int = 1,
                            posFeaturizer: Optional[WordFeaturizer[String]] = NotProvided,
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = NotProvided,
                            useNewVectorLoading: Boolean = false,
                            word2vecPath: String = "../cnnkim/data/GoogleNews-vectors-negative300.bin") extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = PositionalTransformModel[AnnotatedLabel, AnnotatedLabel, String]



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

    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) {
      if (useChildFeats && r.isInstanceOf[BinaryRule[AnnotatedLabel]]) {
        Set(r,
            r.map(_.baseAnnotatedLabel),
            new LeftChildFeature(r.asInstanceOf[BinaryRule[AnnotatedLabel]].left),
            new RightChildFeature(r.asInstanceOf[BinaryRule[AnnotatedLabel]].right)).toSeq
      } else {
        Set(r, r.map(_.baseAnnotatedLabel)).toSeq
      }
    } else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) {
      Set(r.parent, r.parent.baseAnnotatedLabel).toSeq
    } else {
      Seq.empty
    }

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)
      
    val word2vec = if (useNewVectorLoading) {
      Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), summedWordCounts.keySet.toSet[String].map(str => Word2Vec.convertWord(str)), true)
    } else {
      Word2Vec.loadVectorsForVocabulary(word2vecPath, summedWordCounts.keySet.toSet[String].map(str => Word2Vec.convertWord(str)), true)
    }
    // Convert Array[Float] values to DenseVector[Double] values
    val word2vecDoubleVect = word2vec.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
//    val word2vecDoubleVect = word2vec.map(keyValue => (keyValue._1 -> new DenseVector[Double](keyValue._2.map(_.toDouble))))
    
//    val surfaceFeaturizer = new Word2VecSurfaceFeaturizer(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str))
//    val transform = if (useNonlinearity) {
//      // Affine transform of word embeddings, tanh, affine transform to output layer
//      new AffineTransformDense(featurizer.index.size, numHidden, new TanhTransform(new AffineTransformDense(numHidden, surfaceFeaturizer.vectorSize, new IdentityTransform[DenseVector[Double]]())))
//    } else {
//      new AffineTransformDense(featurizer.index.size, surfaceFeaturizer.vectorSize, new IdentityTransform[DenseVector[Double]]())
//    }
    
    val surfaceFeaturizer = Word2VecSurfaceFeaturizerIndexed(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str))
    val transform = PositionalNeuralModelFactory.buildNet(surfaceFeaturizer, numHidden, numHiddenLayers, featurizer.index.size, nonLinType)
    
//    val baseTransformLayer = new CachingLookupAndAffineTransformDense(numHidden, surfaceFeaturizer.vectorSize, surfaceFeaturizer)
//    var currLayer: Transform[Array[Int],DenseVector[Double]] = if (useRelu) new ReluTransform(baseTransformLayer) else new TanhTransform(baseTransformLayer)
//    for (i <- 1 until numHiddenLayers) {
//      val tmpLayer = new AffineTransformDense(numHidden, numHidden, currLayer)
//      currLayer = if (nonLinType == "relu") new ReluTransform(tmpLayer) else if (nonLinType == "cube") new CubeTransform(tmpLayer) else new TanhTransform(tmpLayer)
//    }
//    var transform = new AffineTransformDense(featurizer.index.size, numHidden, currLayer)
    
    println(surfaceFeaturizer.vectorSize + " x (" + numHidden + ")^" + numHiddenLayers + " x " + featurizer.index.size + " neural net")
    
    val maybeSparseFeaturizer = if (useSparseFeatures) {
      var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))
      var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))
      span += new SingleWordSpanFeaturizer[String](wf)
      val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words}, deduplicateFeatures = false)
      val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)
      val indexed = IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
        indexedSurface,
        featurizer,
        new ZeroRuleAndSpansFeaturizer(),
        annotator.latent,
        indexedRefinements,
        xbarGrammar,
        if(dummyFeats < 0) HashFeature.Absolute(-dummyFeats.toInt) else HashFeature.Relative(dummyFeats),
        filterUnseenFeatures = false,
        minFeatCount = 1,
        trainTrees)
      Option(indexed)
    } else {
      None
    }
    
    new PositionalTransformModel(annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar,
      indexedRefinements,
      featurizer,
      surfaceFeaturizer,
      transform,
      maybeSparseFeaturizer
      )
  }
}

object PositionalNeuralModelFactory {
  
  def buildNet(surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[String], numHidden: Int, numHiddenLayers: Int, outputSize: Int, nonLinType: String) = {
    val baseTransformLayer = new CachingLookupAndAffineTransformDense(numHidden, surfaceFeaturizer.vectorSize, surfaceFeaturizer)
    var currLayer: Transform[Array[Int],DenseVector[Double]] = addNonlinearLayer(baseTransformLayer, nonLinType)
    for (i <- 1 until numHiddenLayers) {
      val tmpLayer = new AffineTransformDense(numHidden, numHidden, currLayer)
      currLayer = addNonlinearLayer(tmpLayer, nonLinType)
    }
    var transform = new AffineTransformDense(outputSize, numHidden, currLayer)
    transform
  }
  
  def addNonlinearLayer(currNet: Transform[Array[Int],DenseVector[Double]], nonLinType: String) = {
    if (nonLinType == "relu") {
      new ReluTransform(currNet)
    } else if (nonLinType == "cube") {
      new CubeTransform(currNet)
    } else if (nonLinType == "tanh") {
      new TanhTransform(currNet)
    } else {
      throw new RuntimeException("Unknown nonlinearity type: " + nonLinType)
    }
  }
}

case class LeftChildFeature(f: Feature) extends Feature;
case class RightChildFeature(f: Feature) extends Feature;
