package epic.corefdense

import breeze.linalg.DenseVector
import edu.berkeley.nlp.entity.ner.NEEvaluator
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.sem.BrownClusterInterface
import edu.berkeley.nlp.futile.LightRunner
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import epic.dense.AffineTransform
import epic.dense.AffineOutputTransform
import epic.dense.OutputEmbeddingTransform
import epic.dense.LowRankQuadraticTransform
import epic.dense.IdentityTransform
import epic.dense.NonlinearTransform
import epic.dense.Transform
import epic.dense.Word2VecIndexed
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.ner.NerPrunerFromModel
import scala.util.Random
import scala.collection.mutable.HashSet

object DenseNerDriver {
  
  val useAdadelta = false
  val numItrs = 30
  val eta = 1.0F
  val reg = 1e-8
  val initWeightsScale = 0.01
  val initializerSpec = ""
  val batchSize = 100
  
  val parallel = true
  
  val useSparseFeatures = false;
  val featureSetSpec = ""
  
  val word2vecPath = ""
  val brownPath = ""
  
  val nonLinear = true;
  val hiddenSize = 100
  val numHiddenLayers = 1
  val nonLinType = "relu"
  val lrqt = false;
  val lrqtRanks = 20
  val outputEmbedding = false;
  val outputEmbeddingDim = 20;
  val clipEmbeddingNorms = false;
  
  val useDropout = false
  val stagedTraining = false
  
  val trainPath = "";
  val trainSize = -1;
  val testPath = "";
  val testSize = -1;
  
  val checkEmpiricalGradient = false
  
  val pruningModelPath = "models/ner.ser.gz"
//  val pruningModelPath = ""
  val negPruningThreshold = 5;
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(DenseNerDriver.getClass())
    LightRunner.populateScala(DenseNerDriver.getClass(), args)
    
    // Load preliminaries
    val labelIndexer = NerSystemLabeled.StdLabelIndexer;
    val maybeBrownClusters = if (brownPath != "") Some(BrownClusterInterface.loadBrownClusters(brownPath, 0)) else None;
    val maybeWikipediaDB = None;
    val featureIndexer = new Indexer[String]();
    
    // Instantiate and featurize training data
    val trainDocs = NerSystemLabeled.loadDocs(trainPath, trainSize, true)
    Logger.logss("Loading pruner from " + pruningModelPath + " (not loading if path is empty)");
    val exampleLoader = new DenseNerExampleLoader(if (pruningModelPath == "") None else Option(new NerPrunerFromModel(GUtil.load(pruningModelPath).asInstanceOf[NerSystemLabeled], -negPruningThreshold)), labelIndexer)
    Logger.startTrack("Extracting training examples");
//    val trainExamples = exampleLoader.extractNerExsFromConll(trainDocs, filterPrunedExs = true)
    val trainExamples = new Random(0).shuffle(exampleLoader.extractNerExsFromConll(trainDocs, filterPrunedExs = true))
    Logger.endTrack();
    val nerFeaturizer = NerFeaturizer(featureSetSpec.split(":").toSet, featureIndexer, labelIndexer, trainExamples.map(_.ex.words), maybeWikipediaDB, maybeBrownClusters, 1, 10, 2);
    // Featurize transitions and then examples
    val featurizedTransitionMatrix = Array.tabulate(labelIndexer.size, labelIndexer.size)((prev, curr) => {
      nerFeaturizer.featurizeTransition(labelIndexer.getObject(prev), labelIndexer.getObject(curr), true);
    });
    Logger.startTrack("Featurizing");
    val trainSequenceExs = for (i <- 0 until trainExamples.size) yield {
      if (i % 100 == 0) {
        Logger.logss("Featurizing train example " + i);
      }
      val ex = trainExamples(i);
      new NerExampleWithFeatures(ex.ex, nerFeaturizer.featurize(ex.ex, true), ex.allowedStates);
    };
    Logger.endTrack();
    Logger.logss(featureIndexer.size + " features");
    
    // Figure out the vocabulary and load word2vec
    val trainVoc = trainSequenceExs.flatMap(ex => (0 until ex.size).flatMap(i => DenseNerSystem.extractRelevantWords(ex.ex.words, i)).toSet).toSet
    val word2vecRaw = Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), trainVoc.map(str => Word2Vec.convertWord(str)), maxVectorLen = Int.MaxValue, inputVectorBias = true)
    val word2vecRawDoubleVect = word2vecRaw.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
    val word2vecIndexed = Word2VecIndexed(word2vecRawDoubleVect, (str: String) => Word2Vec.convertWord(str))
    val vecSize = word2vecIndexed.wordRepSize * DenseNerSystem.extractRelevantWords(trainSequenceExs.head.ex.words, 0).size
    
    // Build the net and system
    val transform = if (lrqt) {
      new LowRankQuadraticTransform(labelIndexer.size, lrqtRanks, vecSize, vecSize, new IdentityTransform[DenseVector[Double]]())
    } else if (outputEmbedding) {
//      new AffineOutputEmbeddingTransform(labelIndexer.size, if (numHiddenLayers == 0) vecSize else hiddenSize, outputEmbeddingDim, buildBasicNNLayers(vecSize, numHiddenLayers))
      // Not 100% equivalent but quite close, and the latter is way faster
      new OutputEmbeddingTransform(labelIndexer.size, outputEmbeddingDim, new AffineTransform(outputEmbeddingDim, if (numHiddenLayers == 0) vecSize else hiddenSize, buildBasicNNLayers(vecSize, numHiddenLayers)))
    } else if (nonLinear) {
      new AffineOutputTransform(labelIndexer.size, if (numHiddenLayers == 0) vecSize else hiddenSize, buildBasicNNLayers(vecSize, numHiddenLayers))
    } else {
      new AffineOutputTransform(labelIndexer.size, vecSize, new IdentityTransform[DenseVector[Double]]())
    }
    val denseNerSystem = new DenseNerSystem(labelIndexer, word2vecIndexed, transform, featurizedTransitionMatrix, nerFeaturizer, useSparseFeatures, clipEmbeddingNorms)
    
    // Train
    val initialWeights = denseNerSystem.getInitialWeights(initWeightsScale, initializerSpec);
    if (checkEmpiricalGradient) {
      val indices = HashSet[Int]();
      for (i <- 0 until initialWeights.size by initialWeights.size/20) {
        indices += i
      }
//      indices ++= 0 until 5
//      indices ++= ((vecSize + 1) * hiddenSize) until ((vecSize + 1) * hiddenSize + 5)
//      indices ++= transform.index.size until transform.index.size + 5
      GeneralTrainer.checkGradientFromPoint(trainSequenceExs, denseNerSystem, initialWeights, Array.tabulate(initialWeights.size)(i => 0.0), indices.toSet, verbose = true)
    }
    var weights = if (useAdadelta) {
      new GeneralTrainer(parallel).trainAdadelta(trainSequenceExs, denseNerSystem, 0.95, batchSize, numItrs, initialWeights, verbose = true);
    } else {
      new GeneralTrainer(parallel).trainAdagrad(trainSequenceExs, denseNerSystem, eta, reg, batchSize, numItrs, initialWeights, verbose = true);
    }
    
    // If staged training, do another stage
    val (finalSystem, finalWeights) = if (stagedTraining) {
      Logger.logss("Running staged training; doing another round")
      val newTransform = new AffineOutputTransform(labelIndexer.size, hiddenSize, buildBasicNNLayers(vecSize, numHiddenLayers + 1))
      val newDenseNerSystem = new DenseNerSystem(labelIndexer, word2vecIndexed, newTransform, featurizedTransitionMatrix, nerFeaturizer, useSparseFeatures, clipEmbeddingNorms)
      val newInitialWeights = graftGetInitialWeights(transform.asInstanceOf[AffineOutputTransform[DenseVector[Double]]], newTransform, DenseVector(weights), initWeightsScale, new Random(0), initializerSpec)
      // Train for twice as long on the new system
      val newWeights = new GeneralTrainer(parallel).trainAdadelta(trainSequenceExs, newDenseNerSystem, 0.95, batchSize, 2 * numItrs, newInitialWeights.data, verbose = true);
      (newDenseNerSystem, newWeights)
    } else {
      (denseNerSystem, weights)
    }
    
    // Extract test examples and run the model
    val testDocs = NerSystemLabeled.loadDocs(testPath, testSize, true)
    val testExamples = exampleLoader.extractNerExsFromConll(testDocs, filterPrunedExs = false);
    val testSequenceExs = testExamples.map(ex => new NerExampleWithFeatures(ex.ex, nerFeaturizer.featurize(ex.ex, false), ex.allowedStates));
    val testGoldChunks = testSequenceExs.map(ex => NerSystemLabeled.convertToLabeledChunks(ex.ex.goldLabels));
    Logger.startTrack("Decoding dev");
    val testPredChunks = testSequenceExs.map(ex => NerSystemLabeled.convertToLabeledChunks(finalSystem.decode(ex, finalWeights)))
    Logger.endTrack();
    NEEvaluator.evaluateChunksBySent(testGoldChunks, testPredChunks);
    
    LightRunner.finalizeOutput()
  }
  
  def buildBasicNNLayers(inputSize: Int, numHiddenLayersThisNet: Int) = {
    var innerTransform: Transform[DenseVector[Double],DenseVector[Double]] = new IdentityTransform[DenseVector[Double]]();
    var nextLayerInputSize = inputSize
    for (i <- 0 until numHiddenLayersThisNet) {
      innerTransform = new NonlinearTransform(nonLinType, hiddenSize, new AffineTransform(hiddenSize, nextLayerInputSize, innerTransform))
      nextLayerInputSize = hiddenSize
      innerTransform = if (useDropout) {
        new NonlinearTransform("dropout", hiddenSize, innerTransform)
      } else {
        innerTransform
      }
    }
    innerTransform
  }
  
  
  def graftGetInitialWeights[FV](baseTransform: AffineOutputTransform[FV],
                                 newTransform: AffineOutputTransform[FV],
                                 weights: DenseVector[Double],
                                 initWeightsScale: Double,
                                 rng: Random,
                                 initializerSpec: String) = {
    val newTransformInitialWeights = newTransform.initialWeightVector(initWeightsScale, rng, true, initializerSpec)
    // Discard the output layer from the first transform
    val oldStart = baseTransform.index.componentOffset(1)
    val oldEnd = weights.size
    val numWeightsToKeep = baseTransform.index.size - oldStart
    val newStart = newTransformInitialWeights.size - numWeightsToKeep
    val newEnd = newTransformInitialWeights.size
    newTransformInitialWeights(newStart until newEnd) := weights(oldStart until oldEnd)
    Logger.logss("Rewriting weights from " + newStart + " until " + newEnd + " with old weights from " + oldStart + " until " + oldEnd)
    newTransformInitialWeights
  }
}