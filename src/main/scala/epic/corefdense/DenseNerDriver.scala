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
import epic.dense.AffineTransformDense
import epic.dense.IdentityTransform
import epic.dense.NonlinearTransform
import epic.dense.Word2VecIndexed
import edu.berkeley.nlp.entity.ner.NerPruner
import edu.berkeley.nlp.entity.ner.NerPrunerFromModel

object DenseNerDriver {

  val hiddenSize = 100
  
  val useAdadelta = false
  val numItrs = 30
  val eta = 1.0F
  val reg = 1e-8
  val initWeightsScale = 0.01
  val batchSize = 100
  
  val parallel = true
  
  val useSparseFeatures = false;
  val featureSetSpec = ""
  
  val word2vecPath = ""
  val brownPath = ""
  
  val nonLinear = true;
  val nonLinType = "relu"
  
  val trainPath = "";
  val trainSize = -1;
  val testPath = "";
  val testSize = -1;
  
  val checkEmpiricalGradient = false
  
  val pruningModelPath = "models/ner.ser.gz"
//  val pruningModelPath = ""
  val pruningThreshold = -5;
  
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
    val exampleLoader = new DenseNerExampleLoader(if (pruningModelPath == "") None else Option(new NerPrunerFromModel(GUtil.load(pruningModelPath).asInstanceOf[NerSystemLabeled], -5)), labelIndexer)
    Logger.startTrack("Extracting training examples");
    val trainExamples = exampleLoader.extractNerExsFromConll(trainDocs)
    Logger.endTrack();
//    val trainExamples = if (false) {
//      // TODO: Load pruner and prune
//      val pruner = new NerPrunerFromModel(GUtil.load(pruningModelPath).asInstanceOf[NerSystemLabeled], -5)
//      DenseNerSystem.extractNerChunksFromConllAndPrune(trainDocs, labelIndexer, pruner)
//    } else {
//      NerSystemLabeled.extractNerChunksFromConll(trainDocs).map(ex => new NerExamplePruned(ex, Array.tabulate(ex.words.size, labelIndexer.size)((j, k) => true)));
//    }
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
    val word2vecRaw = Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), trainVoc.map(str => Word2Vec.convertWord(str)), Int.MaxValue, true)
    val word2vecRawDoubleVect = word2vecRaw.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
    val word2vecIndexed = Word2VecIndexed(word2vecRawDoubleVect, (str: String) => Word2Vec.convertWord(str))
    val vecSize = word2vecIndexed.wordRepSize * DenseNerSystem.extractRelevantWords(trainSequenceExs.head.ex.words, 0).size
    
    // Build the net and system
    val transform = if (nonLinear) {
      new AffineTransformDense(labelIndexer.size, hiddenSize, new NonlinearTransform(nonLinType, new AffineTransform(hiddenSize, vecSize, new IdentityTransform[DenseVector[Double]]())))
    } else {
      new AffineTransformDense(labelIndexer.size, vecSize, new IdentityTransform[DenseVector[Double]]())
    }
    val denseNerSystem = new DenseNerSystem(labelIndexer, word2vecIndexed, transform, featurizedTransitionMatrix, nerFeaturizer, useSparseFeatures)
    
    // Train
    val initialWeights = denseNerSystem.getInitialWeights(initWeightsScale);
//    if (checkEmpiricalGradient) {
//      val indices = HashSet[Int]();
//      indices ++= 0 until 5
////      indices ++= ((vecSize + 1) * hiddenSize) until ((vecSize + 1) * hiddenSize + 5)
//      indices ++= transform.index.size until transform.index.size + 5
//      GeneralTrainer.checkGradientFromPoint(trainDocGraphs, corefNeuralModel, initialWeights, Array.tabulate(initialWeights.size)(i => 0.0), indices.toSet, verbose = true)
//    }
    val weights = if (useAdadelta) {
      new GeneralTrainer(parallel).trainAdadelta(trainSequenceExs, denseNerSystem, 0.95, batchSize, numItrs, initialWeights, verbose = true);
    } else {
      new GeneralTrainer(parallel).trainAdagrad(trainSequenceExs, denseNerSystem, eta, reg, batchSize, numItrs, initialWeights, verbose = true);
    }
    
    // Extract test examples and run the model
    val testDocs = NerSystemLabeled.loadDocs(testPath, testSize, true)
//    val testExamples = NerSystemLabeled.extractNerChunksFromConll(testDocs);
    val testExamples = exampleLoader.extractNerExsFromConll(testDocs);
    val testSequenceExs = testExamples.map(ex => new NerExampleWithFeatures(ex.ex, nerFeaturizer.featurize(ex.ex, false), ex.allowedStates));
    val testGoldChunks = testSequenceExs.map(ex => NerSystemLabeled.convertToLabeledChunks(ex.ex.goldLabels));
    Logger.startTrack("Decoding dev");
    val testPredChunks = testSequenceExs.map(ex => NerSystemLabeled.convertToLabeledChunks(denseNerSystem.decode(ex, weights)))
    Logger.endTrack();
    NEEvaluator.evaluateChunksBySent(testGoldChunks, testPredChunks);
    
    LightRunner.finalizeOutput()
  }
}