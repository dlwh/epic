package epic.corefdense

import scala.util.Random
import breeze.linalg.DenseVector
import breeze.linalg.max
import edu.berkeley.nlp.entity.ner.NerExample
import edu.berkeley.nlp.entity.ner.NerFeaturizer
import edu.berkeley.nlp.entity.ner.NerSystemLabeled
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.math.SloppyMath
import epic.dense.AffineTransform
import epic.dense.Word2VecIndexed
import edu.berkeley.nlp.futile.util.Logger

case class NerExampleWithFeatures(val ex: NerExample, val featsPerState: Array[Array[Array[Int]]]) {
  def size = ex.words.size
}

@SerialVersionUID(1L)
class DenseNerSystem(val labelIndexer: Indexer[String],
                     val word2vec: Word2VecIndexed[String],
                     val transform: AffineTransform[DenseVector[Double],DenseVector[Double]],
                     val featurizedTransitionMatrix: Array[Array[Array[Int]]],
                     val featurizer: NerFeaturizer) extends LikelihoodAndGradientComputer[NerExampleWithFeatures] {
  
  val numStates = labelIndexer.size
  val reducedLabelSetSize = NerSystemLabeled.LabelSetReduced.size;
  
  // Caches and structures that are used during inference
  val MaxSize = 200
  
  val cachedEmissionScores = Array.tabulate(MaxSize, labelIndexer.size)((i, j) => 0.0)
  val cachedTransitionScores = Array.tabulate(labelIndexer.size, labelIndexer.size)((i, j) => 0.0)
  val cachedForwardLogProbs = Array.tabulate(MaxSize, labelIndexer.size)((i, j) => 0.0)
  val cachedBackwardLogProbs = Array.tabulate(MaxSize, labelIndexer.size)((i, j) => 0.0)
  val permittedTransitions = Array.tabulate(labelIndexer.size, labelIndexer.size)((i, j) => {
    NerFeaturizer.isLegalTransition(labelIndexer.getObject(i), labelIndexer.getObject(j))
  })
  val transitionFeatures: Array[Array[Array[Int]]] = Array.tabulate(labelIndexer.size, labelIndexer.size)((i, j) => {
    if (permittedTransitions(i)(j)) {
      featurizer.featurizeTransition(labelIndexer.getObject(i), labelIndexer.getObject(j), true)
    } else {
      null
    }
  })
  val tempLogProbVector = Array.tabulate(labelIndexer.size)(i => 0.0)
  
  val rng = new Random(0)
  
  private def formVector(words: Seq[String], idx: Int) = {
    word2vec.convertToVector(DenseNerSystem.extractRelevantWords(words, idx).map(word2vec.wordIndex(_)))
  }
  
  private def cacheEmissionScores(ex: NerExampleWithFeatures, weights: Array[Double]) {
    val layer = transform.extractLayer(DenseVector(weights)(0 until transform.index.size))
    for (i <- 0 until ex.size) {
      // Dense scores
      // XXX
      val denseScores = layer.activations(DenseVector(formVector(ex.ex.words, i)))
//      val denseScores = Array.tabulate(labelIndexer.size)(i => 0.0)
      // Sparse scores
      var j = 0
      while (j < numStates) {
        if (ex.featsPerState(i)(j) == null) {
          cachedEmissionScores(i)(j) = Double.NegativeInfinity
        } else {
          require(!denseScores(j).isNaN, denseScores.data.toSeq)
          cachedEmissionScores(i)(j) = denseScores(j) + DeepUtils.dotProductOffset(ex.featsPerState(i)(j), weights, transform.index.size)
        }
        require(!cachedEmissionScores(i)(j).isNaN, denseScores(j) + " " + DeepUtils.dotProductOffset(ex.featsPerState(i)(j), weights, transform.index.size))
        j += 1
      }
    }
//    Logger.logss("Cached emission scores")
  }
  
  private def cacheTransitionScores(weights: Array[Double]) {
    for (i <- 0 until numStates) {
      for (j <- 0 until numStates) {
        if (transitionFeatures(i)(j) == null) {
          cachedTransitionScores(i)(j) = Double.NegativeInfinity;
        } else {
          cachedTransitionScores(i)(j) = DeepUtils.dotProductOffset(transitionFeatures(i)(j), weights, transform.index.size)
        }
        require(!cachedTransitionScores(i)(j).isNaN)
      }
    }
  }
  
  private def computeForwardBackwardProbs(ex: NerExampleWithFeatures, weights: Array[Double], sum: Boolean) {
    val numTokens = ex.size
    cacheEmissionScores(ex, weights)
    cacheTransitionScores(weights)
    for (i <- 0 until numTokens) {
      var j = 0
      while (j < numStates) {
        if (i == 0) {
          cachedForwardLogProbs(i)(j) = cachedEmissionScores(i)(j)
        } else {
          var k = 0
          while (k < numStates) {
            tempLogProbVector(k) = cachedForwardLogProbs(i-1)(k) + cachedTransitionScores(k)(j)
            k += 1
          }
          val transitionAndPastScore = (if (sum) SloppyMath.logAdd(tempLogProbVector) else max(tempLogProbVector)) + cachedEmissionScores(i)(j);
          cachedForwardLogProbs(i)(j) = transitionAndPastScore;
        }
        j += 1
      }
    }
    for (i <- (numTokens-1) to 0 by -1) {
      var j = 0
      while (j < numStates) {
        if (i == numTokens - 1) {
          cachedBackwardLogProbs(i)(j) = 0;
        } else {
          var k = 0;
          while (k < numStates) {
            tempLogProbVector(k) = cachedBackwardLogProbs(i+1)(k) + cachedEmissionScores(i+1)(k) + cachedTransitionScores(j)(k);
            k += 1
          }
          val transitionAndFutureScore = if (sum) SloppyMath.logAdd(tempLogProbVector) else max(tempLogProbVector)
          cachedBackwardLogProbs(i)(j) = transitionAndFutureScore
        }
        j += 1
      }
    }
  }
  
  private def computeLogNormalizer(ex: NerExampleWithFeatures) = SloppyMath.logAdd(cachedForwardLogProbs(ex.size - 1))
  
  private def computeLogLikelihood(ex: NerExampleWithFeatures) = {
    var score = 0.0
    val goldLabels = ex.ex.goldLabels.map(label => labelIndexer.getIndex(label))
    for (i <- 0 until ex.size) {
      if (i < ex.size - 1) {
        score += cachedTransitionScores(goldLabels(i))(goldLabels(i+1))
      }
      score += cachedEmissionScores(i)(goldLabels(i))
    }
    val logNormalizer = computeLogNormalizer(ex)
    require(!score.isNaN())
    require(!logNormalizer.isNaN())
    score - logNormalizer
  }
  
  def getInitialWeights(initialWeightsScale: Double): Array[Double] = {
    DenseVector.vertcat(transform.initialWeightVector(initialWeightsScale, rng, true, ""),
                        DenseVector.zeros[Double](featurizer.featureIndexer.size)).data
  }
  
  /**
   * Accumulates the gradient on this example into gradient and returns the log likelihood
   * of this example
   */
  def accumulateGradientAndComputeObjective(ex: NerExampleWithFeatures, weights: Array[Double], gradient: Array[Double]): Double = {
    computeForwardBackwardProbs(ex, weights, true)
    accumulateDenseFeatureGradient(ex, weights, gradient)
    accumulateSparseFeatureGradient(ex, gradient)
    // Return the log likelihood
    computeLogLikelihood(ex)
  }
  
  private def accumulateDenseFeatureGradient(ex: NerExampleWithFeatures, weights: Array[Double], gradient: Array[Double]) {
    val numTokens = ex.size
    val goldLabels = ex.ex.goldLabels.map(label => labelIndexer.getIndex(label))
    val layer = transform.extractLayer(DenseVector(weights)(0 until transform.index.size))
    val logNormalizer = computeLogNormalizer(ex)
    for (i <- 0 until numTokens) {
      val scale = DenseVector(Array.tabulate(numStates)(j => {
        (if (j == goldLabels(i)) 1 else 0) - Math.exp(cachedForwardLogProbs(i)(j) + cachedBackwardLogProbs(i)(j) - logNormalizer)
      }))
      layer.tallyDerivative(DenseVector(gradient), scale, DenseVector(formVector(ex.ex.words, i)))
    }
  }
  
  private def accumulateSparseFeatureGradient(ex: NerExampleWithFeatures, gradient: Array[Double]) {
    val numTokens = ex.size
    val goldLabels = ex.ex.goldLabels.map(labelIndexer.getIndex(_))
    val logNormalizer = computeLogNormalizer(ex)
    // Update emission features
    for (i <- 0 until numTokens) {
      for (j <- 0 until numStates) {
        val thisFeatureVector = ex.featsPerState(i)(j);
        if (thisFeatureVector != null) {
          val logProb = cachedForwardLogProbs(i)(j) + cachedBackwardLogProbs(i)(j) - logNormalizer;
          val prob = Math.exp(logProb);
          require(!prob.isNaN)
          DeepUtils.addToGradient(thisFeatureVector, (if (j == goldLabels(i)) 1 else 0) - prob, gradient, transform.index.size)
//          var weightIdx = 0
//          while (weightIdx < thisFeatureVector.size) {
//            if (j == goldLabels(i)) {
//              gradient(thisFeatureVector(weightIdx + transform.index.size)) += 1;
//            }
////            gradient[thisFeatureVector[weightIdx)) -= prob;
//            weightIdx += 1
//          }
        }
      }
    }
    // Update transition features
    for (i <- 0 until numTokens - 1) {
      for (j <- 0 until numStates) {
        val beforeScore = cachedForwardLogProbs(i)(j);
        for (k <- 0 until numStates) {
          val thisFeatureVector = transitionFeatures(j)(k);
          if (thisFeatureVector != null) {
            val afterScore = cachedBackwardLogProbs(i+1)(k);
            val logProb = beforeScore + cachedTransitionScores(j)(k) + cachedEmissionScores(i+1)(k) + afterScore - logNormalizer;
            val prob = Math.exp(logProb);
            require(!prob.isNaN)
            DeepUtils.addToGradient(thisFeatureVector, (if (j == goldLabels(i) && k == goldLabels(i+1)) 1 else 0) - prob, gradient, transform.index.size)
//            while (weightIdx < thisFeatureVector.size) {
//              var weightIdx = 0
//              // TODO: Write gradient appropriately
////              if (j == goldLabels[i] && k == goldLabels[i+1]) {
////                gradient[thisFeatureVector[weightIdx]] += 1;
////              }
////              gradient[thisFeatureVector[weightIdx]] -= prob;
//              weightIdx += 1
//            }
          }
        }
      }
    }
  }
  
  def computeObjective(ex: NerExampleWithFeatures, weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
  
  def decode(ex: NerExampleWithFeatures, weights: Array[Double]): Array[String] = {
    computeForwardBackwardProbs(ex, weights, false)
    val numTokens = ex.size
    var bestState = -1;
    var bestScore = Double.NegativeInfinity;
    val prediction = Array.tabulate(numTokens)(i => 0);
    for (j <- 0 until numStates) {
      if (cachedForwardLogProbs(numTokens-1)(j) > bestScore) {
        bestState = j;
        bestScore = cachedForwardLogProbs(numTokens-1)(j);
      }
    }
    prediction(numTokens-1) = bestState;
    for (i <- numTokens - 2 to 0 by -1) {
      bestScore = Double.NegativeInfinity;
      var j = 0;
      while (j < numStates) {
        val score = cachedForwardLogProbs(i)(j) + cachedTransitionScores(j)(prediction(i+1)) +
            cachedEmissionScores(i+1)(prediction(i+1)) + cachedBackwardLogProbs(i+1)(prediction(i+1));
        if (score > bestScore) {
          bestScore = score;
          prediction(i) = j;
        }
        j += 1
      }
    }
    return prediction.map(labelIndexer.getObject(_));
  }
}

object DenseNerSystem {
  
  private def accessWord(words: Seq[String], idx: Int) = {
    if (idx < 0) "<S>" else if (idx >= words.size) "</S>" else words(idx)
  }
  
  def extractRelevantWords(words: Seq[String], posn: Int) = {
    Array(accessWord(words, posn - 2), accessWord(words, posn - 1), accessWord(words, posn), accessWord(words, posn + 1), accessWord(words, posn + 2))
  }
}
