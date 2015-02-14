package epic.corefdense

import scala.collection.JavaConverters._
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.futile.util.Logger
import breeze.linalg.DenseVector
import scala.util.Random
import epic.dense.IdentityTransform
import epic.dense.LowRankQuadraticTransform

object MNISTTester {
  
  def readMNIST(labelsFile: String, dataFile: String, maxToRead: Int = -1, normalize: Boolean = false): Array[NNExample[Byte]] = {
    val mnistReader = new MNISTReader()
    val exs = mnistReader.readMNIST(labelsFile, dataFile, maxToRead)
    exs.asScala.map(pair => new NNExampleImpl(pair.getFirst.map(value => if (normalize) value.toDouble/255.0 else value.toDouble), pair.getSecond.byteValue)).toArray 
  }
  
  def main(args: Array[String]) {
     
    val trainSize = 1000
    val testSize = 1000
//    val trainSamples= readMNIST("data/mnist/train-labels-idx1-ubyte", "data/mnist/train-images-idx3-ubyte", trainSize)
//    val testSamples = readMNIST("data/mnist/t10k-labels-idx1-ubyte", "data/mnist/t10k-images-idx3-ubyte", testSize)
    val trainSamples= readMNIST("data/mnist/train-labels-idx1-ubyte", "data/mnist/train-images-idx3-ubyte", trainSize, true)
    val testSamples = readMNIST("data/mnist/t10k-labels-idx1-ubyte", "data/mnist/t10k-images-idx3-ubyte", testSize, true)
    
    val vacuousIndexer = new Indexer[Byte];
    (0 to 9).map(idx => vacuousIndexer.add(idx.toByte))
    
    val inputSize = trainSamples.head.input.size
    val outputSize = vacuousIndexer.size
    Logger.logss(inputSize + " inputs, " + outputSize + " outputs")
    
//    val hiddenSize = 100
    val hiddenSize = 200
//    val hiddenSize = 500
    
//    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 2, outputSize, "tanh")
//    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 2, outputSize, "relu")
//    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 2, outputSize, "cube")
//    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 1, outputSize, "tanh")
    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 1, outputSize, "relu")
//    val transform = SimpleNNEpic.makeDeepTransform(inputSize, hiddenSize, 1, outputSize, "cube")
    
    // N.B. MAKE SURE TO NORMALIZE TO [0, 1] RATHER THAN [0, 255] FOR THIS!
//    val transform = new LowRankQuadraticTransform(outputSize, 10, inputSize, inputSize, new IdentityTransform[DenseVector[Double]]())
    
    val nn: SimpleNNEpic[Byte] = new SimpleNNEpic(transform, vacuousIndexer)
//    val initialWeights = nn.getInitialWeights(1.0);
//    val initialWeights = nn.getInitialWeights(0.1);
    val initialWeights = nn.getInitialWeights(0.01);
//    val initialWeights = nn.getInitialWeights(1e-8);
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
    
//    val iters = 100;
//    val iters = 50;
    val iters = 25;
    
//    val weights = new GeneralTrainer().trainSGDMomentum(trainSamples, nn, 0.001, 0.9, 1e-3, batchSize, iters, initialWeights, verbose = false);
    val weights = new GeneralTrainer().trainSGDMomentum(trainSamples, nn, 0.01, 0, 1e-3, batchSize, iters, initialWeights, verbose = false);
    
//    val weights = new GeneralTrainer().trainAdadelta(trainSamples, nn, 0.95, batchSize, iters, initialWeights, verbose = false);
//    val weights = new GeneralTrainer().trainAdadelta(trainSamples, nn, 0.95, batchSize, iters, initialWeights, weightPostprocessor = weightProjector, verbose = false);
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