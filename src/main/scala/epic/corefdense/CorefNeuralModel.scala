package epic.corefdense

import edu.berkeley.nlp.entity.coref.DocumentGraph
import breeze.linalg.DenseVector
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizer
import scala.util.Random
import epic.dense.Transform
import epic.dense.Word2VecIndexed
import edu.berkeley.nlp.entity.coref.Mention
import edu.berkeley.nlp.entity.coref.OrderedClustering
import epic.dense.AffineTransform
import edu.berkeley.nlp.futile.math.SloppyMath
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.futile.util.Logger

trait CorefPredictor extends LikelihoodAndGradientComputer[DocumentGraph] {
  def predictAllFormClusterings(inputs: Seq[DocumentGraph], weights: Array[Double]): (Array[Array[Int]], Array[OrderedClustering]);
}

object CorefNeuralModel {
  
  val NullPlaceholder = "<NULL>"
  
  // Note that the placeholders here <s> 
  def extractRelevantMentionWords(ment: Mention, surfaceFeats: String = "") = {
    if (surfaceFeats == "most") {
      Array(ment.contextWordOrPlaceholder(-2),
            ment.contextWordOrPlaceholder(-1),
            ment.contextWordOrPlaceholder(0),
            if (ment.words.size == 1) NullPlaceholder else ment.contextWordOrPlaceholder(1),
            if (ment.headIdx - ment.startIdx == 0) NullPlaceholder else ment.contextWordOrPlaceholder(ment.headIdx - ment.startIdx - 1),
            ment.contextWordOrPlaceholder(ment.headIdx - ment.startIdx),
            if (ment.headIdx == ment.endIdx - 1) NullPlaceholder else ment.contextWordOrPlaceholder(ment.headIdx - ment.startIdx + 1),
            if (ment.words.size == 1) NullPlaceholder else ment.contextWordOrPlaceholder(ment.endIdx - 2 - ment.startIdx),
            ment.contextWordOrPlaceholder(ment.endIdx - 1 - ment.startIdx),
            ment.contextWordOrPlaceholder(ment.endIdx - ment.startIdx),
            ment.contextWordOrPlaceholder(ment.endIdx - ment.startIdx + 1))
    } else {
      Array(ment.contextWordOrPlaceholder(-1),
            ment.contextWordOrPlaceholder(0),
            ment.contextWordOrPlaceholder(ment.headIdx - ment.startIdx),
            ment.contextWordOrPlaceholder(ment.endIdx - 1 - ment.startIdx),
            ment.contextWordOrPlaceholder(ment.endIdx - ment.startIdx))
    }
  }
}

//
//class CorefNeuralModel(val sparseFeaturizer: PairwiseIndexingFeaturizer,
//                       val transform: AffineTransform[DenseVector[Double],DenseVector[Double]],
//                       val word2vec: Word2VecIndexed[String],
//                       val lossFcn: (CorefDoc, Int, Int) => Float) extends CorefPredictor {
//  
////  val transform = new 
//  val sparseIndexer = sparseFeaturizer.getIndexer()
//  
//  val rng = new Random(0)
//  
//  val MaxSize = 2000
//  val cachedVectors = new Array[DenseVector[Double]](MaxSize)
//  val cachedActivations = new Array[DenseVector[Double]](MaxSize)
//  
//  def getInitialWeights(initialWeightsScale: Double) = transform.initialWeightVector(initialWeightsScale, rng, true, "").data ++ Array.tabulate(sparseIndexer.size)(i => 0.0)
//  
//  def vectorSize = 5 * word2vec.wordRepSize
//  
//  private def formVector(ment: Mention) = {
//    word2vec.convertToVector(CorefNeuralModel.extractRelevantMentionWords(ment).map(word2vec.indexWord(_)))
//  }
//  
//  private def cacheActivations(ex: DocumentGraph, layer: AffineTransform[DenseVector[Double],DenseVector[Double]]#Layer) {
//    for (i <- 0 until ex.getMentions().size) {
//      cachedVectors(i) = DenseVector(formVector(ex.getMention(i)))
////      Logger.logss(cachedVectors(i).size)
//      cachedActivations(i) = layer.activations(cachedVectors(i))
//    }
//  }
//  
//  def computeCacheLogProbs(input: DocumentGraph,
//                           weights: Array[Double],
//                           layer: AffineTransform[DenseVector[Double],DenseVector[Double]]#Layer,
//                           lossAugment: Boolean) {
//    cacheActivations(input, layer)
//    val featsChart = input.featurizeIndexNonPrunedUseCache(sparseFeaturizer)
//    for (mentIdx <- 0 until input.size) {
//      val scores = input.cachedScoreMatrix(mentIdx)
//      for (antIdx <- 0 to mentIdx) {
//        val sparseScore = DeepUtils.dotProductOffset(featsChart(mentIdx)(antIdx), weights, transform.index.size)
//        val denseScore = if (antIdx == mentIdx) {
//          // Score the new cluster
//          0.0F
//        } else {
//          // Score the antecedent choice
//          cachedActivations(mentIdx).dot(cachedVectors(antIdx))
//        }
////        if (!lossAugment) {
////          Logger.logss(denseScore + " " + sparseScore)
////        }
//        scores(antIdx) = (denseScore + sparseScore + (if (lossAugment) lossFcn(input.corefDoc, mentIdx, antIdx) else 0.0)).toFloat
//      }
//    }
//  }
//  
//  def accumulateGradientAndComputeObjective(input: DocumentGraph, weights: Array[Double], gradient: Array[Double]): Double = {
//    require(input.size < MaxSize)
//    val layer = transform.extractLayer(DenseVector(weights), true)
//    computeCacheLogProbs(input, weights, layer, true)
//    val featsChart = input.featurizeIndexNonPrunedUseCache(sparseFeaturizer)
//    var ll = 0.0
//    for (mentIdx <- 0 until input.size) {
//      val scores = input.cachedScoreMatrix(mentIdx)
//      val goldAnts = input.getGoldAntecedentsUnderCurrentPruning(mentIdx)
//      var allNormalizer = Double.NegativeInfinity
//      var goldNormalizer = Double.NegativeInfinity
//      var antIdx = 0
//      while (antIdx <= mentIdx) {
//        if (!input.isPruned(mentIdx, antIdx)) {
//          if (goldAnts.contains(antIdx)) {
//            goldNormalizer = SloppyMath.logAdd(goldNormalizer, scores(antIdx))
//          }
//          allNormalizer = SloppyMath.logAdd(allNormalizer, scores(antIdx))
//        }
//        antIdx += 1
//      }
//      ll += goldNormalizer - allNormalizer;
//      antIdx =  0
//      val partialsVec = DenseVector.zeros[Double](vectorSize)
//      while (antIdx <= mentIdx) {
//        if (!input.isPruned(mentIdx, antIdx)) {
//          // Update sparse features
//          val isGold = goldAnts.contains(antIdx)
//          val delta = (if (isGold) Math.exp(scores(antIdx) - goldNormalizer) else 0) - Math.exp(scores(antIdx) - allNormalizer) 
//          DeepUtils.addToGradient(featsChart(mentIdx)(antIdx), delta, gradient, transform.index.size);
//          // Update the partials vector for dense features
//          if (antIdx != mentIdx) {
//            partialsVec += cachedVectors(antIdx) * delta
//          }
//        }
//        antIdx += 1
//      }
//      // Update dense features
//      layer.tallyDerivative(DenseVector(gradient), partialsVec, cachedVectors(mentIdx))
//    }
//    ll
////    val layer = transform.extractLayer(DenseVector(weights))
////    val logProbs = layer.activations(DenseVector(ex.input)).data
////    ClassifyUtils.softmaxi(logProbs)
////    val trueLabelIdx = labelIndexer.getIndex(ex.getLabel)
////    // Sparse features gradient
////    
////    val tallyInputs = Array.tabulate(labelIndexer.size)(i => (if (i == trueLabelIdx) 1.0 else 0.0) - Math.exp(logProbs(i)))
////    layer.tallyDerivative(DenseVector(gradient), DenseVector(tallyInputs), DenseVector(ex.input))
////    logProbs(labelIndexer.indexOf(ex.getLabel))
//  }
//  
//  def predict(input: DocumentGraph, weights: Array[Double]) = {
//    require(input.size < MaxSize)
//    val layer = transform.extractLayer(DenseVector(weights), false)
//    computeCacheLogProbs(input, weights, layer, false)
//    (0 until input.size).toArray.map(mentIdx => {
//      val scores = input.cachedScoreMatrix(mentIdx)
//      CorefNNEpic.argMaxIdxFloat(scores)
//    })
//  }
//  
//  def predictAllFormClusterings(inputs: Seq[DocumentGraph], weights: Array[Double]) = {
//    val allPredBackptrs = inputs.toArray.map(predict(_, weights));
//    val allPredClusteringsSeq = (0 until inputs.size).map(i => OrderedClustering.createFromBackpointers(allPredBackptrs(i)));
//    (allPredBackptrs, allPredClusteringsSeq.toArray)
//  }
//  
//  def computeObjective(ex: DocumentGraph, weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
//}
//
