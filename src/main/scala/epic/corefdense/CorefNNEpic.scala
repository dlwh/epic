package epic.corefdense

import scala.util.Random
import breeze.linalg._
import edu.berkeley.nlp.futile.util.Logger
import epic.dense.AffineTransform
import epic.dense.IdentityTransform
import epic.dense.TanhTransform
import edu.berkeley.nlp.futile.math.SloppyMath

case class CorefEx(val antecedents: Array[Array[Double]],
                   val anaphor: Array[Double],
                   val correctIdx: Int) {
  
  override def toString() = {
    anaphor.toSeq.toString + "\n  " + antecedents.map(_.toSeq.toString).reduce(_ + "\n  " + _)
  }
}

class CorefNNEpic(val inputSize: Int,
                  val hiddenSize: Int,
                  val outputSize: Int,
                  val embedBoth: Boolean,
                  val hiddenLayer: Boolean) extends LikelihoodAndGradientComputer[CorefEx] {
  val rng = new Random(0)
  val transform = if (hiddenLayer) {
    new AffineTransform(outputSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, inputSize, new IdentityTransform[DenseVector[Double]]())))
  } else {
    new AffineTransform(outputSize, inputSize, new IdentityTransform[DenseVector[Double]]())
  }
  
  def getInitialWeights(initWeightsScale: Double) = transform.initialWeightVector(initWeightsScale, rng, false, "").data
//  def getInitialWeights = transform.initialWeightVector(1.0, rng, true).data
  
  def accumulateGradientAndComputeObjective(ex: CorefEx, weights: Array[Double], gradient: Array[Double]): Double = {
    val layer = transform.extractLayer(DenseVector(weights))
//    if (hiddenLayer) {
//      CorefNNEpic.displayMat(DenseVector(weights)(0 until (outputSize * hiddenSize)).asDenseMatrix.reshape(outputSize, hiddenSize, view = View.Require))
//    } else {
//      CorefNNEpic.displayMat(DenseVector(weights)(0 until (outputSize * inputSize)).asDenseMatrix.reshape(outputSize, inputSize, view = View.Require))
//    }
    
    val anaphorVec = layer.activations(DenseVector(ex.anaphor))
//    val antecedentVecs = ex.antecedents.map(inputVec => layer.activations(DenseVector(inputVec)))
    val antecedentVecs = if (embedBoth) {
      ex.antecedents.map(inputVec => layer.activations(DenseVector(inputVec)))
    } else {
      ex.antecedents.map(inputVec => DenseVector(inputVec))
    }
//    Logger.logss("EMBEDDINGS")
//    Logger.logss("  " + anaphorVec.data.toSeq)
//    for (antecedentVec <- antecedentVecs) {
//      Logger.logss("  " + antecedentVec.data.toSeq)
//    }
//    System.exit(0)
    val antecedentScores = antecedentVecs.map(vec => vec.dot(anaphorVec))
    CorefNNEpic.softmaxi(antecedentScores)
    val antecedentProbs = antecedentScores.map(Math.exp(_))
    
    val deriv = DenseVector(weights)
    // Update dobj/dv_ana
    val tallyVec = antecedentVecs(ex.correctIdx) - (0 until antecedentProbs.size).map(i => antecedentVecs(i) * antecedentProbs(i)).reduce(_ + _)
    layer.tallyDerivative(deriv, tallyVec, DenseVector(ex.anaphor))
    if (embedBoth) {
      for (i <- 0 until antecedentVecs.size) {
        val tallyAntVec = anaphorVec * ((if (i == ex.correctIdx) 1 else 0) - antecedentProbs(i)) 
        layer.tallyDerivative(deriv, tallyAntVec, DenseVector(ex.antecedents(i)))
      }
    }
    antecedentScores(ex.correctIdx)
  }
  
  def predict(antecedents: Array[Array[Double]], anaphor: Array[Double], weights: DenseVector[Double]): Int = {
    val layer = transform.extractLayer(weights)
    // Take dot products
    val anaphorVec = layer.activations(DenseVector(anaphor))
    val antecedentDots = if (embedBoth) {
      antecedents.map(inputVec => anaphorVec.dot(layer.activations(DenseVector(inputVec))))
    } else {
      antecedents.map(inputVec => anaphorVec.dot(DenseVector(inputVec)))
    }
    CorefNNEpic.argMaxIdx(antecedentDots)
  }
  
  def computeObjective(ex: CorefEx, weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
}





class CorefNNEpicDistinctEmbeddings(val inputSize: Int,
                                    val hiddenSize: Int,
                                    val outputSize: Int,
                                    val hiddenLayer: Boolean) extends LikelihoodAndGradientComputer[CorefEx] {
  val rng = new Random(0)
  val transform = if (hiddenLayer) {
    new AffineTransform(outputSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, inputSize, new IdentityTransform[DenseVector[Double]]())))
  } else {
    new AffineTransform(outputSize, inputSize, new IdentityTransform[DenseVector[Double]]())
  }
  val antTransform = if (hiddenLayer) {
    new AffineTransform(outputSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, inputSize, new IdentityTransform[DenseVector[Double]]())))
  } else {
    new AffineTransform(outputSize, inputSize, new IdentityTransform[DenseVector[Double]]())
  }
  
  def getInitialWeights(initWeightsScale: Double) = DenseVector.vertcat(transform.initialWeightVector(initWeightsScale, rng, false, ""), antTransform.initialWeightVector(initWeightsScale, rng, false, "")).data
//  def getInitialWeights = transform.initialWeightVector(1.0, rng, true).data
  
  def accumulateGradientAndComputeObjective(ex: CorefEx, weights: Array[Double], gradient: Array[Double]): Double = {
    val layer = transform.extractLayer(DenseVector(weights)(0 until transform.index.size))
    val anaphorVec = layer.activations(DenseVector(ex.anaphor))
    
    val antLayer = antTransform.extractLayer(DenseVector(weights)(transform.index.size to -1))
    val antecedentVecs = ex.antecedents.map(inputVec => antLayer.activations(DenseVector(inputVec)))
    
    val antecedentScores = antecedentVecs.map(vec => vec.dot(anaphorVec))
    CorefNNEpic.softmaxi(antecedentScores)
    val antecedentProbs = antecedentScores.map(Math.exp(_))
    
    val deriv = DenseVector(weights)(0 until transform.index.size)
    val antDeriv = DenseVector(weights)(transform.index.size to -1)
    // Update dobj/dv_ana
    val tallyVec = antecedentVecs(ex.correctIdx) - (0 until antecedentProbs.size).map(i => antecedentVecs(i) * antecedentProbs(i)).reduce(_ + _)
    layer.tallyDerivative(deriv, tallyVec, DenseVector(ex.anaphor))
    for (i <- 0 until antecedentVecs.size) {
      val tallyAntVec = anaphorVec * ((if (i == ex.correctIdx) 1 else 0) - antecedentProbs(i))
      antLayer.tallyDerivative(antDeriv, tallyAntVec, DenseVector(ex.antecedents(i)))
    }
    antecedentScores(ex.correctIdx)
  }
  
  def predict(antecedents: Array[Array[Double]], anaphor: Array[Double], weights: DenseVector[Double]): Int = {
    val layer = transform.extractLayer(weights(0 until transform.index.size))
    val antLayer = antTransform.extractLayer(weights(transform.index.size to -1))
    // Take dot products
    val anaphorVec = layer.activations(DenseVector(anaphor))
    val antecedentDots = antecedents.map(inputVec => anaphorVec.dot(antLayer.activations(DenseVector(inputVec))))
    CorefNNEpic.argMaxIdx(antecedentDots)
  }
  
  def computeObjective(ex: CorefEx, weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
}




object CorefNNEpic {
  
  def softmaxi(values: Array[Double]) {
    val total = SloppyMath.logAdd(values)
    for (i <- 0 until values.size) {
      values(i) = values(i) - total
    }
  }
  
  def argMaxIdx(values: Array[Double]) = {
    var max = Double.NegativeInfinity;
    var selected = -1
    for (i <- 0 until values.size) {
      if (selected == -1 || values(i) > max) {
        max = values(i)
        selected = i
      }
    }
    selected
  }
  
  def argMaxIdxFloat(values: Array[Float]) = {
    var max = Float.NegativeInfinity;
    var selected = -1
    for (i <- 0 until values.size) {
      if (selected == -1 || values(i) > max) {
        max = values(i)
        selected = i
      }
    }
    selected
  }
  
  def displayMat(mat: DenseMatrix[Double]) {
    for (i <- 0 until mat.rows) {
      for (j <- 0 until mat.cols) {
        print(GUtil.fmt(mat.apply(i, j)) + "\t")
      }
      println()
    }
  }

  def randVect(numDim: Int, rng: Random): Array[Double] = {
    val vector = Array.tabulate(numDim)(i => rng.nextGaussian())
    val norm = Math.sqrt(vector.map(x => x * x).reduce(_ + _))
    (0 until vector.size).foreach(i => vector(i) = vector(i)/norm);
    vector
  }
  
  /**
   * Constructs numExamples examples with numAntecedents antecedents each, with each having
   * numDim dimensions. The first numDim/4 coordinates are fixed to be the same between
   * the anaphor and antecedent (so that hopefully the projection will focus on these)
   */
  def randExamples(numExamples: Int, numAntecedents: Int, numDim: Int): Array[CorefEx] = {
    val rng = new Random(0)
    Array.tabulate(numExamples)(i => {
      val anaphorVect = randVect(numDim, rng)
      val firstAntecedentVect = randVect(numDim, rng)
      (0 until numDim/4).foreach(j => firstAntecedentVect(j) = anaphorVect(j))
      new CorefEx(Array.tabulate(numAntecedents)(j => if (j == 0) firstAntecedentVect else randVect(numDim, rng)), anaphorVect, 0)
    })
  }
  
  def main(args: Array[String]) {
    val hiddenLayer = false
    val embedBoth = true
    val distinctEmbeddings = true
    val trainExs = randExamples(1000, 5, 20)
    val testExs = randExamples(1000, 5, 20)
    val inputSize = trainExs.head.anaphor.size
    val hiddenSize = 10
    val outputSize = trainExs.head.antecedents.head.size
//    val corefNN = new CorefNNEpic(inputSize, hiddenSize, outputSize, embedBoth, hiddenLayer)
    val corefNN = new CorefNNEpicDistinctEmbeddings(inputSize, hiddenSize, outputSize, hiddenLayer)
//    val nnBlas = new SimpleNNBest(3, 20, 2)
//    Logger.logss(trainExs(0))
//    GeneralTrainer.checkGradient(trainSamples.slice(0, 1), nn, nn.numFeats, verbose = true)
//    System.exit(0);
    val initialWeights = corefNN.getInitialWeights(1.0);
//    val numItrs = 100
    val numItrs = 50
    val eta = 1.0F
    val reg = 0.0000001F
//    val eta = 1.0F
//    val reg = 1.0F
    val weights = new GeneralTrainer().train(trainExs, corefNN, eta, reg, 10, numItrs, initialWeights, verbose = false);
    
    
    if (hiddenLayer) {
      CorefNNEpic.displayMat(DenseVector(weights)(0 until (outputSize * hiddenSize)).asDenseMatrix.reshape(outputSize, hiddenSize, view = View.Require))
    } else {
      println("ANAPHOR MAT")
      CorefNNEpic.displayMat(DenseVector(weights)(0 until (outputSize * inputSize)).asDenseMatrix.reshape(outputSize, inputSize, view = View.Require))
      val startOfSecondMat = corefNN.transform.index.size
      println("ANT MAT")
      CorefNNEpic.displayMat(DenseVector(weights)(startOfSecondMat until startOfSecondMat + (outputSize * inputSize)).asDenseMatrix.reshape(outputSize, inputSize, view = View.Require))
    }
    
    var correct = 0;
    for (i <- 0 until testExs.size) {
      if (corefNN.predict(testExs(i).antecedents, testExs(i).anaphor, DenseVector(weights)) == testExs(i).correctIdx) {
        correct += 1;
      }
    }
    Logger.logss(correct + " / " + testExs.size);
  }
  
}