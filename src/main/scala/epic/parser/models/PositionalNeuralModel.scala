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
import epic.dense.Transform
import epic.dense.TanhTransform
import epic.dense.OutputTransform
import epic.dense.AffineOutputTransform
import epic.dense.OutputEmbeddingTransform
import epic.dense.Word2Vec
import scala.collection.mutable.HashMap
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.dense.Word2VecDepFeaturizerIndexed
import epic.dense.Word2VecIndexed
import epic.dense.FrequencyTagger
import epic.dense.CachingLookupTransform
import epic.dense.CachingLookupAndAffineTransformDense
import epic.dense.EmbeddingsTransform
import epic.dense.NonlinearTransform
import scala.io.Source
import scala.collection.mutable.HashSet
import epic.dense.BatchNormalizationTransform

/**
 * Entry point for instantiating a neural CRF parser. Parameters specify neural
 * net parameters, word vectors, and sparse features to use.
 *
 * @author gdurrett
 **/

case class ExtraPNMParams(useSparseLfsuf: Boolean = true,
                          useSparseBrown: Boolean = false,
                          useMostSparseIndicators: Boolean = false,
                          vectorRescaling: Double = 1.0,
                          outputEmbedding: Boolean = false,
                          outputEmbeddingDim: Int = 20,
                          coarsenByRoot: Boolean = false, // should we coarsen for initialization by root symbol?
                          surfaceFeatureSpec: String = "",
                          decoupleTransforms: Boolean = false,
                          treebankVocFile: String = "",
                          batchNormalization: Boolean = false,
                          useRootLabel: Boolean = false,
                          lowercasedVectors: Boolean = false,
                          randomizeUnks: Boolean = false)

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
                            useSparseFeatures: Boolean = false,
                            @Help(text="Options: tanh, relu, cube")
                            nonLinType: String = "tanh",
                            @Help(text="Options: normal, random, trivial, normalpos")
                            embeddingType: String = "normal",
                            backpropIntoEmbeddings: Boolean = false,
                            dropoutRate: Double = 0.0,
                            numHidden: Int = 100,
                            numHiddenLayers: Int = 1,
                            useDeps: Boolean = false,
                            word2vecPath: String = "../cnnkim/data/GoogleNews-vectors-negative300.bin",
                            extraPNMParams: ExtraPNMParams = ExtraPNMParams()) extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = PositionalTransformModel[AnnotatedLabel, AnnotatedLabel, String]



  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
    import extraPNMParams._
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    println("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = topology
    val xbarLexicon = lexicon

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (useRootLabel) {
      Set(r, r.map(_.baseAnnotatedLabel), ParentFeature(r.parent)).toSeq
    } else {
      Set(r, r.map(_.baseAnnotatedLabel)).toSeq
    }

    val prodFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen=labelFeaturizer, rGen=ruleFeaturizer)
    
//    val presentWords = Word2Vec.readBansalEmbeddings(word2vecPath, summedWordCounts.keySet.toSet[String], false)
//    var displayCount = 0
//    var displayCount2 = 0
//    for (word <- summedWordCounts.keySet.toSeq.sortBy(word => -summedWordCounts(word))) {
//      if (!presentWords.contains(word) && displayCount < 100) {
//        println(word + ": " + summedWordCounts(word))
//        displayCount += 1
//      } else if (presentWords.contains(word) && displayCount2 < 100) {
//        println("PRESENT: " + summedWordCounts(word))
//        displayCount2 += 1
//      }
//    }
//    System.exit(0)

    
    ///////////////////////
    // READ IN WORD VECTORS
    val tagCountsLexicon = TagSpanShapeGenerator.makeStandardLexicon(annTrees)
    val freqTagger = new FrequencyTagger(tagCountsLexicon)

    val voc = new HashSet[String]()
    // Add words in the training set
    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    voc ++= summedWordCounts.keySet.toSet[String].map(str => Word2Vec.convertWord(str, lowercasedVectors))
    // Read in a file of words in the treebank; this allows us to load words that are
    // in the dev or test sets but not in train
    voc ++= (if (treebankVocFile != "") Source.fromFile(treebankVocFile).getLines().map(str => Word2Vec.convertWord(str, lowercasedVectors)).toSet else Set[String]())
    val word2vec = if (embeddingType == "trivial") {
      Word2Vec.makeRandomVectorsForVocabulary(voc.toSet, 0, true)
    } else if (embeddingType == "random") {
      Word2Vec.makeRandomVectorsForVocabulary(voc.toSet, 50, true)
    } else {
      Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), voc.toSet, summedWordCounts, if (embeddingType == "trivial") 1 else Int.MaxValue, true, randomizeUnks)
    }
    // Convert Array[Float] values to Array[Double] values and rescale them
    val word2vecDoubleVect = word2vec.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble * vectorRescaling)))
//    val word2vecDoubleVect = word2vec.map(keyValue => (keyValue._1 -> new DenseVector[Double](keyValue._2.map(_.toDouble))))
    val word2vecIndexed: Word2VecIndexed[String] = if (embeddingType == "normalpos") {
      Word2VecIndexed(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str, lowercasedVectors)).augment(freqTagger.tagTypesIdx.size, freqTagger.convertToFeaturizer)
    } else {
      Word2VecIndexed(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str, lowercasedVectors))
    }
    //////////////////////
    
    val surfaceFeaturizer = new Word2VecSurfaceFeaturizerIndexed(word2vecIndexed, surfaceFeatureSpec)
    val depFeaturizer = new Word2VecDepFeaturizerIndexed(word2vecIndexed, freqTagger, topology)
    
    
//    val baseTransformLayer = new CachingLookupAndAffineTransformDense(numHidden, surfaceFeaturizer.vectorSize, surfaceFeaturizer)
//    var currLayer: Transform[Array[Int],DenseVector[Double]] = if (useRelu) new ReluTransform(baseTransformLayer) else new TanhTransform(baseTransformLayer)
//    for (i <- 1 until numHiddenLayers) {
//      val tmpLayer = new AffineTransformDense(numHidden, numHidden, currLayer)
//      currLayer = if (nonLinType == "relu") new ReluTransform(tmpLayer) else if (nonLinType == "cube") new CubeTransform(tmpLayer) else new TanhTransform(tmpLayer)
//    }
//    var transform = new AffineTransformDense(featurizer.index.size, numHidden, currLayer)
    
    
    val transforms = if (decoupleTransforms) {
      IndexedSeq[AffineOutputTransform[Array[Int]]]()
    } else {
      val inputSize = surfaceFeaturizer.splitInputSize
      val transform = if (outputEmbedding) {
        val coarsenerForInitialization = if (coarsenByRoot) {
          Option(PositionalNeuralModelFactory.getRuleToParentMapping(prodFeaturizer.index)) 
        } else {
          None
        }
        PositionalNeuralModelFactory.buildNetOutputEmbedding(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings, batchNormalization, outputEmbeddingDim, coarsenerForInitialization)
      } else {
        // THIS IS THE STANDARD CODE PATH
        println(inputSize + " x (" + numHidden + ")^" + numHiddenLayers + " x " + prodFeaturizer.index.size + " neural net")
        PositionalNeuralModelFactory.buildNet(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings, batchNormalization)
      }
      IndexedSeq(transform)
    }
    val depTransforms: IndexedSeq[AffineOutputTransform[Array[Int]]] = if (useDeps) {
      throw new RuntimeException("Dependency NNs removed temporarily")
//      println("Deps: " + word2vecIndexed.vectorSize + " x (" + numHidden + ")^" + numHiddenLayers + " x " + prodFeaturizer.index.size + " neural net")
//      val depTransform = PositionalNeuralModelFactory.buildNet(word2vecIndexed, numHidden, numHiddenLayers, 1, nonLinType, backpropIntoEmbeddings)
//      IndexedSeq(depTransform)
    } else {
      IndexedSeq()
    }
    val decoupledTransforms = if (decoupleTransforms) {
      // Span and unary use the reduced input (no split point features), whereas surface uses the split point features
      val inputSizes = Seq(surfaceFeaturizer.reducedInputSize, surfaceFeaturizer.reducedInputSize, surfaceFeaturizer.splitInputSize)
      inputSizes.map(inputSize => PositionalNeuralModelFactory.buildNet(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings, batchNormalization))
    } else {
      IndexedSeq[AffineOutputTransform[Array[Int]]]()
    }
    
    println(transforms.size + " transforms, " + transforms.map(_.index.size).toSeq + " parameters for each")
    println(depTransforms.size + " dep transforms, " + depTransforms.map(_.index.size).toSeq + " parameters for each")
    println(decoupledTransforms.size + " decoupled transforms, " + decoupledTransforms.map(_.index.size).toSeq + " parameters for each")
    
    
    
    val maybeSparseFeaturizer = if (useSparseFeatures) {
      var wf = SpanModelFactory.defaultPOSFeaturizer(annWords, useBrown = useSparseBrown)
      var span = SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false, useLfsuf = useSparseLfsuf, useBrown = useSparseBrown, useMostSparseIndicators = useMostSparseIndicators)
      span += new SingleWordSpanFeaturizer[String](wf)
      val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words}, deduplicateFeatures = false)
      val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)
      
      def sparseLabelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
//      def sparseRuleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
      def sparseRuleFeaturizer(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq
      val sparseProdFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen=sparseLabelFeaturizer, rGen=sparseRuleFeaturizer)
      
      
      val indexed = IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
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
      Option(indexed)
    } else {
      None
    }
    
    new PositionalTransformModel(annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar,
      indexedRefinements,
      prodFeaturizer,
      surfaceFeaturizer,
      depFeaturizer,
      transforms,
      maybeSparseFeaturizer,
      depTransforms,
      decoupledTransforms,
      batchNormalization)
  }
}

object PositionalNeuralModelFactory {
  
  def buildNetInnerTransforms(word2vecIndexed: Word2VecIndexed[String],
                              inputSize: Int,
                              numHidden: Int,
                              numHiddenLayers: Int,
                              nonLinType: String,
                              dropoutRate: Double,
                              backpropIntoEmbeddings: Boolean,
                              batchNormalization: Boolean): Transform[Array[Int],DenseVector[Double]] = {
    if (numHiddenLayers == 0) {
      new CachingLookupTransform(word2vecIndexed)
    } else {
      val baseTransformLayer = if (backpropIntoEmbeddings) {
        new EmbeddingsTransform(numHidden, inputSize, word2vecIndexed)
      } else {
        new CachingLookupAndAffineTransformDense(numHidden, inputSize, word2vecIndexed)
      }
      var currLayer = addNonlinearity(nonLinType, numHidden, dropoutRate, batchNormalization, baseTransformLayer)
      for (i <- 1 until numHiddenLayers) {
        currLayer = addNonlinearity(nonLinType, numHidden, dropoutRate, batchNormalization, new AffineTransform(numHidden, numHidden, currLayer))
      }
      currLayer
    }
  }
  
  def buildNet(word2vecIndexed: Word2VecIndexed[String],
               inputSize: Int,
               numHidden: Int,
               numHiddenLayers: Int,
               outputSize: Int,
               nonLinType: String,
               dropoutRate: Double,
               backpropIntoEmbeddings: Boolean,
               batchNormalization: Boolean): AffineOutputTransform[Array[Int]] = {
    val innerTransform = buildNetInnerTransforms(word2vecIndexed, inputSize, numHidden, numHiddenLayers, nonLinType, dropoutRate, backpropIntoEmbeddings, batchNormalization)
    new AffineOutputTransform(outputSize, if (numHiddenLayers >= 1) numHidden else inputSize, innerTransform)
//    if (numHiddenLayers == 0) {
//      new AffineOutputTransform(outputSize, word2vecIndexed.vectorSize, new CachingLookupTransform(word2vecIndexed))
//    } else {
//      val baseTransformLayer = if (backpropIntoEmbeddings) {
//        new EmbeddingsTransform(numHidden, word2vecIndexed.vectorSize, word2vecIndexed)
//      } else {
//        new CachingLookupAndAffineTransformDense(numHidden, word2vecIndexed.vectorSize, word2vecIndexed)
//      }
//      var currLayer = addNonlinearity(nonLinType, numHidden, useDropout, baseTransformLayer)
//      for (i <- 1 until numHiddenLayers) {
//        currLayer = addNonlinearity(nonLinType, numHidden, useDropout, new AffineTransform(numHidden, numHidden, currLayer))
//      }
//      var transform = new AffineOutputTransform(outputSize, numHidden, currLayer)
//      transform
//    }
  }
  
  
  def buildNetOutputEmbedding(word2vecIndexed: Word2VecIndexed[String],
                              inputSize: Int,
                              numHidden: Int,
                              numHiddenLayers: Int,
                              outputSize: Int,
                              nonLinType: String,
                              dropoutRate: Double,
                              backpropIntoEmbeddings: Boolean,
                              batchNormalization: Boolean,
//                              outputEmbeddingDim: Int): AffineOutputEmbeddingTransform[Array[Int]] = {
                              outputEmbeddingDim: Int,
                              coarsenerForInitialization: Option[Int => Int]): OutputTransform[Array[Int],DenseVector[Double]] = {
    val innerTransform = buildNetInnerTransforms(word2vecIndexed, inputSize, numHidden, numHiddenLayers, nonLinType, dropoutRate, backpropIntoEmbeddings, batchNormalization)
    
    val innerTransformLastLayer = new AffineTransform(outputEmbeddingDim, if (numHiddenLayers >= 1) numHidden else inputSize, innerTransform)
    new OutputEmbeddingTransform(outputSize, outputEmbeddingDim, innerTransformLastLayer, coarsenerForInitialization)
    
//    new AffineOutputEmbeddingTransform(outputSize, if (numHiddenLayers >= 1) numHidden else word2vecIndexed.vectorSize, outputEmbeddingDim, innerTransform)
  }
  
  def addNonlinearity(nonLinType: String, numHidden: Int, dropoutRate: Double, batchNormalization: Boolean, currLayer: Transform[Array[Int],DenseVector[Double]]) = {
    val useDropout = dropoutRate > 1e-8
    require(!useDropout || !batchNormalization, "Shouldn't use dropout and batch normalization together, they may interact weirdly")
    var tmpLayer = currLayer
    if (batchNormalization) {
      tmpLayer = new BatchNormalizationTransform(numHidden, useBias = true, tmpLayer)
    }
    tmpLayer = new NonlinearTransform(nonLinType, numHidden, tmpLayer)
    if (useDropout) {
      tmpLayer = new NonlinearTransform("dropout", numHidden, tmpLayer, dropoutRate)
    }
    tmpLayer
  }
  
  def getRuleToParentMapping(index: Index[Feature]): Int => Int = {
    (i: Int) => {
      if (index.get(i).isInstanceOf[Rule[AnnotatedLabel]]) {
        val parentIdx = index(index.get(i).asInstanceOf[Rule[AnnotatedLabel]].parent)
        if (parentIdx == -1) {
//          println("-1!")
          0
        } else {
//          println("Mapping " + index.get(i) + " to " + index.get(parentIdx))
          parentIdx
        }
      } else {
        i
      }
    }
  }
}

case class ParentFeature(f: Feature) extends Feature;
case class LeftChildFeature(f: Feature) extends Feature;
case class RightChildFeature(f: Feature) extends Feature;
