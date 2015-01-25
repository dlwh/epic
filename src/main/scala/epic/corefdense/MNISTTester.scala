package epic.corefdense

import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import breeze.linalg.DenseVector
import scala.util.Random

object MNISTTester {
  
  def readMNIST(labelsFile: String, dataFile: String, maxToRead: Int = -1): Array[NNExample[Byte]] = {
    val mnistReader = new MNISTReader()
    val exs = mnistReader.readMNIST(labelsFile, dataFile, maxToRead)
    exs.asScala.map(pair => new NNExampleImpl(pair.getFirst.map(value => value.toDouble), pair.getSecond.byteValue)).toArray 
  }
  
  def main(args: Array[String]) {
     
    val trainSize = 1000
    val testSize = 1000
    val trainSamples: Array[NNExample[Byte]] = readMNIST("data/mnist/train-labels-idx1-ubyte", "data/mnist/train-images-idx3-ubyte", trainSize)
    val testSamples = readMNIST("data/mnist/t10k-labels-idx1-ubyte", "data/mnist/t10k-images-idx3-ubyte", testSize)
    
    val vacuousIndexer = new Indexer[Byte];
    (0 to 9).map(idx => vacuousIndexer.add(idx.toByte))
    
//    val hiddenSize = 100
    val hiddenSize = 200
//    val hiddenSize = 500
    
//  APPROXIMATE OBJECTIVE: -8.950063626269229
//  1000 / 1000
//  640 / 1000
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, 200, 2, vacuousIndexer.size, "tanh")
    
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 2, vacuousIndexer.size, "tanh")
    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 1, vacuousIndexer.size, "tanh")
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 1, vacuousIndexer.size, "relu")
//  APPROXIMATE OBJECTIVE: -1.9972034553071694
//  1000 / 1000
//  824 / 1000
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, 500, 1, vacuousIndexer.size, "tanh")
//  APPROXIMATE OBJECTIVE: -0.5572691610234091
//  1000 / 1000
//  825 / 1000
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, 784, 1, vacuousIndexer.size, "tanh")
//  APPROXIMATE OBJECTIVE: -4.8510921594965107E-4
//  1000 / 1000
//  821 / 1000
//  APPROXIMATE OBJECTIVE: -1305932.345400486
//  54610 / 60000
//  883 / 1000 (Yann LeCun reports 12% on the full dataset)
//    val transform = SimpleNNEpic.makeLinearTransform(trainSamples.head.input.size, vacuousIndexer.size)
    // 824 correct
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, 500, 1, vacuousIndexer.size, "tanh")
    val nn: SimpleNNEpic[Byte] = new SimpleNNEpic(transform, vacuousIndexer)
    val initialWeights = nn.getInitialWeights(1.0);
    Logger.logss(initialWeights.slice(initialWeights.size - 1000, initialWeights.size).toSeq)
    Logger.logss(trainSamples(0))
    
    
    
//    val checkGradient = true
    val checkGradient = false
    if (checkGradient) {
      // Perturb the output layer to make it interesting
      val rng = new Random(0)
      for (i <- 0 until hiddenSize * vacuousIndexer.size) {
        initialWeights(i) += rng.nextDouble * 0.01 - 0.005
      }
//      GeneralTrainer.checkGradient(trainSamples.slice(0, 1), nn, initialWeights.size, verbose = true)
      GeneralTrainer.checkGradientFromPoint(trainSamples.slice(0, 1), nn, initialWeights, Array.tabulate(initialWeights.size)(i => 0.0), ((0 until 10) ++ (initialWeights.size - 1000 until initialWeights.size)).toSet, verbose = true)
//      GeneralTrainer.checkGradientFromPoint(trainSamples.slice(0, 1), nn, Array.tabulate(initialWeights.size)(i => rng.nextDouble * 0.0001 - 0.00005), Array.tabulate(initialWeights.size)(i => 0.0), ((0 until 10) ++ (initialWeights.size - 1000 until initialWeights.size)).toSet, verbose = true)
      System.exit(0); 
    }
    
    val eta = 0.1
//    val eta = 0.01
//    val eta = 0.1
    val batchSize = 1000
//    val batchSize = 1000
    val iters = 100;
    val weights = new GeneralTrainer().train(trainSamples, nn, eta, 0.0000001, batchSize, iters, initialWeights, verbose = false);
//    val weights = new GeneralTrainer().trainLBFGS(trainSamples, nn, 0.0000001, 0.001, iters, initialWeights, verbose = false);
//    val weightsOA = new OffsetArray(weights, 0)
//    var nanoTime = System.nanoTime();
//    for (i <- 0 until 10000) {
//      nn.forward(trainSamples(0).input, weightsOA)
//    }
//    Logger.logss((System.nanoTime() - nanoTime) / 1000 + " micros")
//    nanoTime = System.nanoTime()
//    val firstLayerWeightsMat = new FloatMatrix(nnBlas.hiddenSize, nnBlas.inputSize, weights.slice(0, nnBlas.firstLayerMatSize):_*)
//    val secondLayerWeightsMat = new FloatMatrix(nnBlas.outputSize, nnBlas.hiddenSize, weights.slice(nnBlas.firstLayerMatSize, nnBlas.firstLayerMatSize + nnBlas.secondLayerMatSize):_*)
//    for (i <- 0 until 10000) {
//      nnBlas.forward(trainSamples(0).input, weights)
//    }
//    Logger.logss((System.nanoTime() - nanoTime) / 1000 + " micros")
//    Logger.logss(nn.outputProbs.toSeq)
//    Logger.logss(nnBlas.outputProbs.toSeq)
    
    getAccuracy(nn, trainSamples, weights)
    getAccuracy(nn, testSamples, weights)
    
  }
  
  def getAccuracy[T](nn: SimpleNNEpic[T], exs: Seq[NNExample[T]], weights: Array[Double]) {
    var correct = 0;
    for (i <- 0 until exs.size) {
      if (nn.predict(exs(i).input, DenseVector(weights)) == exs(i).getLabel) {
        correct += 1;
      }
    }
    Logger.logss(correct + " / " + exs.size);
  }
}