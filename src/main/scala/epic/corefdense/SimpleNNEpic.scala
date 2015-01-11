package epic.corefdense

import edu.berkeley.nlp.futile.fig.basic.Indexer
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.dense.CachingLookupAndAffineTransformDense
import epic.dense.EmbeddingsTransform
import epic.dense.AffineTransformDense
import epic.dense.Transform
import breeze.linalg._
import epic.dense.TanhTransform
import epic.dense.ReluTransform
import epic.dense.CubeTransform
import epic.parser.models.PositionalNeuralModelFactory
import scala.util.Random
import edu.berkeley.nlp.futile.util.Logger
import epic.dense.IdentityTransform
import epic.dense.AffineTransform

class SimpleNNEpic[T](val inputSize: Int,
                      val hiddenSize: Int,
                      val outputSize: Int,
                      val labelIndexer: Indexer[T]) extends LikelihoodAndGradientComputer[NNExample[T]] {
  
  val rng = new Random(0)
  val transform = new AffineTransform(outputSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, inputSize, new IdentityTransform[DenseVector[Double]]())))
//  val layers = 
  
  def getInitialWeights = transform.initialWeightVector(1.0, rng, true).data
  
  def accumulateGradientAndComputeObjective(ex: NNExample[T], weights: Array[Double], gradient: Array[Double]): Double = {
    val layer = transform.extractLayer(DenseVector(weights))
    val logProbs = layer.activations(DenseVector(ex.input)).data
    CorefNNEpic.softmaxi(logProbs)
    val trueLabelIdx = labelIndexer.getIndex(ex.getLabel)
    val tallyInputs = Array.tabulate(labelIndexer.size)(i => (if (i == trueLabelIdx) 1.0 else 0.0) - Math.exp(logProbs(i)))
    layer.tallyDerivative(DenseVector(gradient), DenseVector(tallyInputs), DenseVector(ex.input))
    logProbs(labelIndexer.indexOf(ex.getLabel))
  }
  
  def predict(input: Array[Double], weights: DenseVector[Double]): T = {
    val layer = transform.extractLayer(weights)
    val logProbs = layer.activations(DenseVector(input)).data
    labelIndexer.getObject(CorefNNEpic.argMaxIdx(logProbs))
  }
  
  def computeObjective(ex: NNExample[T], weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
}

object SimpleNNEpic {
  
  def buildNet(surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[String],
               numHidden: Int,
               numHiddenLayers: Int,
               outputSize: Int,
               nonLinType: String,
               backpropIntoEmbeddings: Boolean) = {
    val baseTransformLayer = if (backpropIntoEmbeddings) {
      new EmbeddingsTransform(numHidden, surfaceFeaturizer.vectorSize, surfaceFeaturizer)
    } else {
      new CachingLookupAndAffineTransformDense(numHidden, surfaceFeaturizer.vectorSize, surfaceFeaturizer)
    }
    var currLayer: Transform[Array[Int],DenseVector[Double]] = PositionalNeuralModelFactory.addNonlinearLayer(baseTransformLayer, nonLinType)
    for (i <- 1 until numHiddenLayers) {
      val tmpLayer = new AffineTransformDense(numHidden, numHidden, currLayer)
      currLayer = PositionalNeuralModelFactory.addNonlinearLayer(tmpLayer, nonLinType)
    }
    var transform = new AffineTransformDense(outputSize, numHidden, currLayer)
    transform
  }
  
  
  def generateGaussianData(numSamples: Int): Array[NNExample[Int]] = {
    val rng = new Random(0);
    // Generate data in a square, decision is whether
    // it's inside or outside of the unit circle
    // Square has sides of length rt(2pi)
    Array.tabulate(numSamples)(i => {
      val x = (rng.nextDouble() - 0.5) * Math.sqrt(2 * Math.PI);
      val y = (rng.nextDouble() - 0.5) * Math.sqrt(2 * Math.PI);
      val decision = if (x * x + y * y < 1) 0 else 1;
//      val decision = if (2 * x + y < 1) 0 else 1;
      NNExampleImpl(Array(x, y, 1.0), decision)
    });
  }
  
  def main(args: Array[String]) {
    val trainSamples = generateGaussianData(1000);
    Logger.logss(trainSamples.map(_.getLabel).reduce(_ + _) + " total 1s")
    val testSamples = generateGaussianData(100);
    val vacuousIndexer = new Indexer[Int];
    vacuousIndexer.add(0)
    vacuousIndexer.add(1)
    val nn = new SimpleNNEpic(3, 20, 2, vacuousIndexer)
//    val nnBlas = new SimpleNNBest(3, 20, 2)
    Logger.logss(trainSamples(0))
//    GeneralTrainer.checkGradient(trainSamples.slice(0, 1), nn, nn.numFeats, verbose = true)
//    System.exit(0);
    val initialWeights = nn.getInitialWeights;
//    val initialWeights = Array.fill(nn.numFeats)(0.0F);
    val weights = new GeneralTrainer().train(trainSamples, nn, 1.0, 0.0000001, 10, 100, initialWeights, verbose = false);
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
    var correct = 0;
    for (i <- 0 until testSamples.size) {
      if (nn.predict(testSamples(i).input, DenseVector(weights)) == testSamples(i).getLabel) {
        correct += 1;
      }
    }
    Logger.logss(correct + " / " + testSamples.size);
  }
  
}