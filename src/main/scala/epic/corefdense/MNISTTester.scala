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
    
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 2, vacuousIndexer.size, "tanh")
    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 2, vacuousIndexer.size, "relu")
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 2, vacuousIndexer.size, "cube")
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 1, vacuousIndexer.size, "tanh")
//    val transform = SimpleNNEpic.makeDeepTransform(trainSamples.head.input.size, hiddenSize, 1, vacuousIndexer.size, "relu")
    val nn: SimpleNNEpic[Byte] = new SimpleNNEpic(transform, vacuousIndexer)
//    val initialWeights = nn.getInitialWeights(1.0);
    val initialWeights = nn.getInitialWeights(0.1);
//    val initialWeights = nn.getInitialWeights(0.01);
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
    
//    val weightProjector = (weights: Array[Double]) => {}
    val weightProjector = (weights: Array[Double]) => {
      transform.clipHiddenWeightVectors(new DenseVector(weights), 9, true)
    }
    
//    val eta = 1.0
    val eta = 0.1
//    val eta = 0.01
    val batchSize = 100
//    val batchSize = 1000
    val iters = 100;
    val weights = new GeneralTrainer().trainAdadelta(trainSamples, nn, 0.95, batchSize, iters, initialWeights, weightPostprocessor = weightProjector, verbose = false);
//    val weights = new GeneralTrainer().trainAdagrad(trainSamples, nn, eta, 0.0000001, batchSize, iters, initialWeights, weightPostprocessor = weightProjector, verbose = false);
//    val weights = new GeneralTrainer().trainLBFGS(trainSamples, nn, 0.0000001, 0.001, iters, initialWeights, verbose = false);
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