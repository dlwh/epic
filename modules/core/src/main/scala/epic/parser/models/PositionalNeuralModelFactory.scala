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
import epic.util.{LRUCache, Optional}
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


/**
 * Less-used parameters
 */
case class ExtraPNMParams(@Help(text="Used for ablations with random word embeddings; don't change this. Options: normal, random, trivial, normalpos")
                          embeddingType: String = "normal",
                          @Help(text="Use longest frequent suffix (standard representation) for sparse feats")
                          useSparseLfsuf: Boolean = true,
                          @Help(text="Use sparse Brown cluster features")
                          useSparseBrown: Boolean = false,
                          @Help(text="Use expanded set of sparse surface features (doesn't help)")
                          useMostSparseIndicators: Boolean = false,
                          @Help(text="Scaling factor for all input vectors")
                          vectorRescaling: Double = 1.0,
                          @Help(text="Use the output embedding model (Figure 4b in the neural CRF paper)")
                          outputEmbedding: Boolean = false,
                          @Help(text="Dimension of the output embedding model")
                          outputEmbeddingDim: Int = 20,
                          @Help(text="When initializing the output embedding model, initialize based on root symbols")
                          coarsenByRoot: Boolean = false,
                          @Help(text="Use separate neural net parameters for span/unary/binary settings. Doesn't help.")
                          decoupleTransforms: Boolean = false,
                          @Help(text="Extract additional output features based on root label.")
                          useRootLabel: Boolean = false,
                          @Help(text="Set unknown word vectors to be random rather than 0")
                          randomizeUnks: Boolean = false)
                          
case class ExtraPNMSparseParams(@Help(text="Use n-gram features in the sparse featurizer (good for sentiment)")
                                useNGrams: Boolean = false,
                                @Help(text="Max order of n-grams to use in these features")
                                maxNGramOrder:Int = 2,
                                @Help(text="Count threshold for firing n-gram features")
                                ngramCountThreshold: Int = 1,
                                @Help(text="Additional span shape features based on tags")
                                useTagSpanShape: Boolean = false)
                          
case class PositionalNeuralModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
                            dummyFeats: Double = 0.5,
                            @Help(text="Sparse features only fire on suffixes seen at lease this many times. Lower than 100 doesn't seem to do better.")
                            commonWordThreshold: Int = 100,
                            @Help(text="Combine the neural net features with sparse features. The NN does well on its own but sparse helps by >1 F1.")
                            useSparseFeatures: Boolean = true,
                            @Help(text="Nonlinearity to use. Options: tanh, relu, cube")
                            nonLinType: String = "relu",
                            @Help(text="Backpropagate into word embeddings (tune them during training). Doesn't help.")
                            backpropIntoEmbeddings: Boolean = false,
                            @Help(text="Dropout rate; 0.0 won't instantiate any dropout units, higher rates will but it doesn't seem to help.")
                            dropoutRate: Double = 0.0,
                            @Help(text="Width of hidden layer to use.")
                            numHidden: Int = 200,
                            @Help(text="Number of hidden layers to use. More than 1 slows down dramatically and doesn't help.")
                            numHiddenLayers: Int = 1,
                            @Help(text="How much surface context should we use as input to the neural network? Default is +/-2 words around begin/end/split. See Word2VecSurfaceFeaturizer for options")
                            neuralSurfaceWordsToUse: String = "most",
                            @Help(text="Path to word vectors. Can either be .bin like Mikolov et al.'s or .txt like Bansal et al.'s")
                            word2vecPath: String = "",
                            @Help(text="Load additional word vectors into the model rather than just those in the training set. Doesn't help.")
                            vocFile: String = "",
                            @Help(text="Set to true if your word vectors are all lowercase. Otherwise true case is used.")
                            lowercasedVectors: Boolean = false,
                            extraPNMParams: ExtraPNMParams = ExtraPNMParams(),
                            extraPNMSparseParams: ExtraPNMSparseParams = ExtraPNMSparseParams()) extends ParserModelFactory[AnnotatedLabel, String] {
  
  type MyModel = PositionalNeuralModel[AnnotatedLabel, AnnotatedLabel, String]

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
    import extraPNMParams._
    import extraPNMSparseParams._
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
    voc ++= (if (vocFile != "") Source.fromFile(vocFile).getLines().map(str => Word2Vec.convertWord(str, lowercasedVectors)).toSet else Set[String]())
    val word2vec = if (embeddingType == "trivial") {
      Word2Vec.makeRandomVectorsForVocabulary(voc.toSet, 0, true)
    } else if (embeddingType == "random") {
      Word2Vec.makeRandomVectorsForVocabulary(voc.toSet, 50, true)
    } else {
      Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), voc.toSet, summedWordCounts, if (embeddingType == "trivial") 1 else Int.MaxValue, true, randomizeUnks)
    }
    // Convert Array[Float] values to Array[Double] values and rescale them
    val word2vecDoubleVect = word2vec.map(keyValue => keyValue._1 -> keyValue._2.map(_.toDouble * vectorRescaling))
    // val word2vecDoubleVect = word2vec.map(keyValue => (keyValue._1 -> new DenseVector[Double](keyValue._2.map(_.toDouble))))
    val word2vecIndexed: Word2VecIndexed[String] = if (embeddingType == "normalpos") {
      Word2VecIndexed(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str, lowercasedVectors)).augment(freqTagger.tagTypesIdx.size, freqTagger.convertToFeaturizer)
    } else {
      Word2VecIndexed(word2vecDoubleVect, (str: String) => Word2Vec.convertWord(str, lowercasedVectors))
    }
    //////////////////////
    
    val surfaceFeaturizer = new Word2VecSurfaceFeaturizerIndexed(word2vecIndexed, neuralSurfaceWordsToUse)
    val depFeaturizer = new Word2VecDepFeaturizerIndexed(word2vecIndexed, freqTagger, topology)
    
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
        PositionalNeuralModelFactory.buildNetOutputEmbedding(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings, outputEmbeddingDim, coarsenerForInitialization)
      } else {
        // THIS IS THE STANDARD CODE PATH
        println(inputSize + " x (" + numHidden + ")^" + numHiddenLayers + " x " + prodFeaturizer.index.size + " neural net")
        PositionalNeuralModelFactory.buildNet(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings)
      }
      IndexedSeq(transform)
    }
    val depTransforms: IndexedSeq[AffineOutputTransform[Array[Int]]] = IndexedSeq()
    val decoupledTransforms = if (decoupleTransforms) {
      // Span and unary use the reduced input (no split point features), whereas surface uses the split point features
      val inputSizes = Seq(surfaceFeaturizer.reducedInputSize, surfaceFeaturizer.reducedInputSize, surfaceFeaturizer.splitInputSize)
      inputSizes.map(inputSize => PositionalNeuralModelFactory.buildNet(word2vecIndexed, inputSize, numHidden, numHiddenLayers, prodFeaturizer.index.size, nonLinType, dropoutRate, backpropIntoEmbeddings))
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
      if (useNGrams) {
        span += new NGramSpanFeaturizer(summedWordCounts, NGramSpanFeaturizer.countBigrams(annTrees), annTrees.map(_.words), ngramCountThreshold, maxNGramOrder, useNot = false)
      }
      if (useTagSpanShape) {
        span += new TagSpanShapeFeaturizer(TagSpanShapeGenerator.makeBaseLexicon(trainTrees))
      }
      val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words}, deduplicateFeatures = false)
      val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)
      
      def sparseLabelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
      def sparseRuleFeaturizer(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq
      val sparseProdFeaturizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements, lGen=sparseLabelFeaturizer, rGen=sparseRuleFeaturizer)
      
      val indexed = IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
        indexedSurface,
        sparseProdFeaturizer,
        new ZeroRuleAndSpansFeaturizer(),
        annotator.latent,
        indexedRefinements,
        xbarGrammar,
        if (dummyFeats < 0) HashFeature.Absolute(-dummyFeats.toInt) else HashFeature.Relative(dummyFeats),
        filterUnseenFeatures = false,
        minFeatCount = 1,
        trainTrees)
      Option(indexed)
    } else {
      None
    }
    
    new PositionalNeuralModel(annotator.latent,
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
      decoupledTransforms)
  }
}

object PositionalNeuralModelFactory {
  
  def buildNetInnerTransforms(word2vecIndexed: Word2VecIndexed[String],
                              inputSize: Int,
                              numHidden: Int,
                              numHiddenLayers: Int,
                              nonLinType: String,
                              dropoutRate: Double,
                              backpropIntoEmbeddings: Boolean): Transform[Array[Int],DenseVector[Double]] = {
    if (numHiddenLayers == 0) {
      new CachingLookupTransform(word2vecIndexed)
    } else {
      val baseTransformLayer = if (backpropIntoEmbeddings) {
        new EmbeddingsTransform(numHidden, inputSize, word2vecIndexed)
      } else {
        new CachingLookupAndAffineTransformDense(numHidden, inputSize, word2vecIndexed)
      }
      var currLayer = addNonlinearity(nonLinType, numHidden, dropoutRate, baseTransformLayer)
      for (i <- 1 until numHiddenLayers) {
        currLayer = addNonlinearity(nonLinType, numHidden, dropoutRate, new AffineTransform(numHidden, numHidden, currLayer))
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
               backpropIntoEmbeddings: Boolean): AffineOutputTransform[Array[Int]] = {
    val innerTransform = buildNetInnerTransforms(word2vecIndexed, inputSize, numHidden, numHiddenLayers, nonLinType, dropoutRate, backpropIntoEmbeddings)
    new AffineOutputTransform(outputSize, if (numHiddenLayers >= 1) numHidden else inputSize, innerTransform)
  }

  def buildNetOutputEmbedding(word2vecIndexed: Word2VecIndexed[String],
                              inputSize: Int,
                              numHidden: Int,
                              numHiddenLayers: Int,
                              outputSize: Int,
                              nonLinType: String,
                              dropoutRate: Double,
                              backpropIntoEmbeddings: Boolean,
                              outputEmbeddingDim: Int,
                              coarsenerForInitialization: Option[Int => Int]): OutputTransform[Array[Int],DenseVector[Double]] = {
    val innerTransform = buildNetInnerTransforms(word2vecIndexed, inputSize, numHidden, numHiddenLayers, nonLinType, dropoutRate, backpropIntoEmbeddings)
    
    val innerTransformLastLayer = new AffineTransform(outputEmbeddingDim, if (numHiddenLayers >= 1) numHidden else inputSize, innerTransform)
    new OutputEmbeddingTransform(outputSize, outputEmbeddingDim, innerTransformLastLayer, coarsenerForInitialization)
  }
  
  def addNonlinearity(nonLinType: String, numHidden: Int, dropoutRate: Double, currLayer: Transform[Array[Int],DenseVector[Double]]) = {
    val useDropout = dropoutRate > 1e-8
    var tmpLayer = currLayer
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
          0
        } else {
          parentIdx
        }
      } else {
        i
      }
    }
  }
}

case class ParentFeature(f: Feature) extends Feature
case class LeftChildFeature(f: Feature) extends Feature
case class RightChildFeature(f: Feature) extends Feature
