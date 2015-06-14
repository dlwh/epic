package epic.corefdense

import edu.berkeley.nlp.entity.coref.DocumentGraph
import breeze.linalg.DenseVector
import breeze.linalg.SparseVector
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
import epic.dense.AffineOutputTransform
import edu.berkeley.nlp.futile.fig.basic.Indexer
import scala.collection.mutable.ArrayBuffer

class CorefNeuralModel3(val sparseFeaturizer: PairwiseIndexingFeaturizer,
                        val transform: AffineOutputTransform[Array[Int]],
                        val maybeDiscourseNewTransform: Option[AffineOutputTransform[Array[Int]]],
                        val word2vec: Word2VecIndexed[String],
                        val lossFcn: (CorefDoc, Int, Int) => Float,
                        val surfaceFeats: String = "",
                        val maybeOutputFeaturizer: Option[OutputFeaturizer] = None,
                        val maybeDiscourseNewOutputFeaturizer: Option[OutputFeaturizer] = None,
                        val maybeSparseNetFeaturizer: Option[SimplePairwiseIndexingFeaturizerJoint] = None) extends CorefPredictor {
  
  val sparseIndexer = sparseFeaturizer.getIndexer()
  
  val rng = new Random(0)
  
  val MaxSize = 2000
  // TODO: Pick MaxSize to be low enough
  val cachedWordVectors = new Array[Array[Int]](MaxSize)
//  val cachedVectors = new Array[DenseVector[Double]](MaxSize)
//  val cachedActivations = new Array[DenseVector[Double]](MaxSize)
  
  def getInitialWeights(initialWeightsScale: Double) = {
    transform.initialWeightVector(initialWeightsScale, rng, true, "").data ++
    (if (maybeDiscourseNewTransform.isDefined) maybeDiscourseNewTransform.get.initialWeightVector(initialWeightsScale, rng, true, "").data else Array[Double]()) ++
    Array.tabulate(sparseIndexer.size)(i => 0.0)
  }
  
  private def getWordIndicators(ment: Mention) = {
    CorefNeuralModel.extractRelevantMentionWords(ment, surfaceFeats).map(str => word2vec.indexWord(str))
  }
  
  private def formVector(docGraph: DocumentGraph, mentIdx: Int, antIdx: Int) = {
    if (maybeSparseNetFeaturizer.isDefined) {
      cachedWordVectors(mentIdx) ++ cachedWordVectors(antIdx) ++ maybeSparseNetFeaturizer.get.featurizePredefined(docGraph, mentIdx, antIdx)
    } else {
      cachedWordVectors(mentIdx) ++ cachedWordVectors(antIdx)
    }
  }
  
  private def formDiscourseNewVector(docGraph: DocumentGraph, mentIdx: Int) = {
    cachedWordVectors(mentIdx)
  }
  
//  private def cacheActivations(ex: DocumentGraph, layer: AffineTransform[DenseVector[Double],DenseVector[Double]]#Layer) {
//    for (i <- 0 until ex.getMentions().size) {
//      cachedVectors(i) = DenseVector(formVector(ex.getMention(i)))
////      Logger.logss(cachedVectors(i).size)
//      cachedActivations(i) = layer.activations(cachedVectors(i))
//    }
//  }
  
  private def sparseFeatsOffset = transform.index.size + (if (maybeDiscourseNewTransform.isDefined) maybeDiscourseNewTransform.get.index.size else 0)
  
  private def extractLayers(weights: Array[Double]) = {
    val layer = transform.extractLayer(DenseVector(weights)(0 until transform.index.size), true)
    val maybeDiscourseNewLayer = maybeDiscourseNewTransform.map(_.extractLayer(DenseVector(weights)(transform.index.size until transform.index.size + maybeDiscourseNewTransform.get.index.size), true))
    (layer, maybeDiscourseNewLayer)
  }
  
  def computeCacheLogProbs(input: DocumentGraph,
                           weights: Array[Double],
                           layer: AffineOutputTransform[Array[Int]]#OutputLayer,
                           maybeDiscourseNewLayer: Option[AffineOutputTransform[Array[Int]]#OutputLayer],
                           lossAugment: Boolean) {
    val featsChart = input.featurizeIndexNonPrunedUseCache(sparseFeaturizer)
    for (mentIdx <- 0 until input.size) {
      cachedWordVectors(mentIdx) = getWordIndicators(input.getMention(mentIdx))
      val scores = input.cachedScoreMatrix(mentIdx)
      for (antIdx <- 0 to mentIdx) {
        if (!input.isPruned(mentIdx, antIdx)) {
          val sparseScore = DeepUtils.dotProductOffset(featsChart(mentIdx)(antIdx), weights, sparseFeatsOffset)
          val denseScore = if (antIdx == mentIdx) {
            if (maybeDiscourseNewLayer.isDefined) {
              val dnVec = formDiscourseNewVector(input, mentIdx)
              if (maybeDiscourseNewOutputFeaturizer.isDefined) {
                maybeDiscourseNewLayer.get.activationsDot(dnVec, maybeDiscourseNewOutputFeaturizer.get.extractDiscourseNewFeats(input, mentIdx, false))
              } else {
                maybeDiscourseNewLayer.get.activations(dnVec)(0)
              }
            } else {
              0.0F
            }
          } else {
            if (maybeOutputFeaturizer.isDefined) {
              layer.activationsDot(formVector(input, mentIdx, antIdx), maybeOutputFeaturizer.get.extractFeats(input, mentIdx, antIdx, false))
            } else {
              layer.activations(formVector(input, mentIdx, antIdx))(0)
            }
          }
          scores(antIdx) = (denseScore + sparseScore + (if (lossAugment) lossFcn(input.corefDoc, mentIdx, antIdx) else 0.0)).toFloat
        } else {
          scores(antIdx) = Float.NegativeInfinity
        }
      }
    }
  }
  
  def accumulateGradientAndComputeObjective(input: DocumentGraph, weights: Array[Double], gradient: Array[Double]): Double = {
    require(input.size < MaxSize)
    val (layer, maybeDiscourseNewLayer) = extractLayers(weights)
//    val layer = transform.extractLayer(DenseVector(weights)(0 until transform.index.size), true)
//    val maybeDiscourseNewLayer = maybeDiscourseNewTransform.map(_.extractLayer(DenseVector(weights)(transform.index.size until transform.index.size + maybeDiscourseNewTransform.get.index.size), true))
    computeCacheLogProbs(input, weights, layer, maybeDiscourseNewLayer, true)
    val featsChart = input.featurizeIndexNonPrunedUseCache(sparseFeaturizer)
    var ll = 0.0
    for (mentIdx <- 0 until input.size) {
      val scores = input.cachedScoreMatrix(mentIdx)
      val goldAnts = input.getGoldAntecedentsUnderCurrentPruning(mentIdx)
      var allNormalizer = Double.NegativeInfinity
      var goldNormalizer = Double.NegativeInfinity
      var antIdx = 0
      while (antIdx <= mentIdx) {
        if (!input.isPruned(mentIdx, antIdx)) {
          if (goldAnts.contains(antIdx)) {
            goldNormalizer = SloppyMath.logAdd(goldNormalizer, scores(antIdx))
          }
          allNormalizer = SloppyMath.logAdd(allNormalizer, scores(antIdx))
        }
        antIdx += 1
      }
      ll += goldNormalizer - allNormalizer;
      antIdx =  0
      while (antIdx <= mentIdx) {
        if (!input.isPruned(mentIdx, antIdx)) {
          // Update sparse features
          val isGold = goldAnts.contains(antIdx)
          val delta = (if (isGold) Math.exp(scores(antIdx) - goldNormalizer) else 0) - Math.exp(scores(antIdx) - allNormalizer) 
          DeepUtils.addToGradient(featsChart(mentIdx)(antIdx), delta, gradient, sparseFeatsOffset);
          // Update the partials vector for dense features
          if (antIdx != mentIdx) {
            if (maybeOutputFeaturizer.isDefined) {
              val of = maybeOutputFeaturizer.get.extractFeats(input, mentIdx, antIdx, false)
              val deltas = new SparseVector(of.sorted, Array.tabulate(of.size)(i => delta), maybeOutputFeaturizer.get.featIdx.size)
              layer.tallyDerivative(DenseVector(gradient)(0 until transform.index.size), deltas, formVector(input, mentIdx, antIdx))
//              val deltas = Array.tabulate(maybeOutputFeaturizer.get.featIdx.size)(i => if (of.contains(i)) delta else 0.0)
//              Logger.logss(of.toSeq)
//              layer.tallyDerivative(DenseVector(gradient)(0 until transform.index.size), DenseVector(deltas), formVector(input, mentIdx, antIdx))
            } else {
              layer.tallyDerivative(DenseVector(gradient)(0 until transform.index.size), DenseVector(Array(delta)), formVector(input, mentIdx, antIdx))
            }
          } else {
            if (maybeDiscourseNewLayer.isDefined) {
              val dnVec = formDiscourseNewVector(input, mentIdx)
              if (maybeDiscourseNewOutputFeaturizer.isDefined) {
                val of = maybeDiscourseNewOutputFeaturizer.get.extractDiscourseNewFeats(input, mentIdx, false)
                val deltas = new SparseVector(of.sorted, Array.tabulate(of.size)(i => delta), maybeDiscourseNewOutputFeaturizer.get.featIdx.size)
                maybeDiscourseNewLayer.get.tallyDerivative(DenseVector(gradient)(transform.index.size until transform.index.size + maybeDiscourseNewTransform.get.index.size), deltas, dnVec)
              } else {
                maybeDiscourseNewLayer.get.tallyDerivative(DenseVector(gradient)(transform.index.size until transform.index.size + maybeDiscourseNewTransform.get.index.size), DenseVector(Array(delta)), dnVec)
              }
            }
          }
        }
        antIdx += 1
      }
    }
    ll
  }
  
  def predict(input: DocumentGraph, weights: Array[Double]) = {
    require(input.size < MaxSize)
//    val layer = transform.extractLayer(DenseVector(weights), false)
//    val maybeDiscourseNewLayer = maybeDiscourseNewTransform.map(_.extractLayer(DenseVector(weights), true))
    val (layer, maybeDiscourseNewLayer) = extractLayers(weights)
    computeCacheLogProbs(input, weights, layer, maybeDiscourseNewLayer, false)
    (0 until input.size).toArray.map(mentIdx => {
      val scores = input.cachedScoreMatrix(mentIdx)
      CorefNNEpic.argMaxIdxFloat(scores)
    })
  }
  
  def predictAllFormClusterings(inputs: Seq[DocumentGraph], weights: Array[Double]) = {
    val allPredBackptrs = inputs.toArray.map(predict(_, weights));
    val allPredClusteringsSeq = (0 until inputs.size).map(i => OrderedClustering.createFromBackpointers(allPredBackptrs(i)));
    (allPredBackptrs, allPredClusteringsSeq.toArray)
  }
  
  def computeObjective(ex: DocumentGraph, weights: Array[Double]): Double = accumulateGradientAndComputeObjective(ex, weights, Array.tabulate(weights.size)(i => 0.0))
}

case class OutputFeaturizer(val featIdx: Indexer[String],
                            val featSpecStr: String) {
  val featSpec = featSpecStr.split(",").toSet
  
  private def maybeAddFeat(indexedFeats: ArrayBuffer[Int], feat: String, addToIndexer: Boolean) {
    if (addToIndexer) {
      indexedFeats += featIdx.getIndex(feat)
    } else {
      val idx = featIdx.indexOf(feat)
      if (idx != -1) indexedFeats += idx;
    }
  }
  
  def extractFeats(docGraph: DocumentGraph, mentIdx: Int, antIdx: Int, addToIndexer: Boolean): Array[Int] = {
    val feats = new ArrayBuffer[Int]
    val curr = docGraph.getMention(mentIdx)
    val ant = docGraph.getMention(antIdx)
    val currType = curr.mentionType
    val antType = ant.mentionType
    val headMatch = curr.headStringLc == ant.headStringLc
    maybeAddFeat(feats, "Bias", addToIndexer)
    if (featSpec.contains("more")) {
      maybeAddFeat(feats, currType + "-" + antType, addToIndexer)
      maybeAddFeat(feats, "HM=" + headMatch, addToIndexer)
    }
    if (featSpec.contains("most")) {
      maybeAddFeat(feats, currType + "-" + antType + "-HM=" + headMatch, addToIndexer)
    }
    if (featSpec.contains("prons")) {
      val currTypePron = if (currType.isClosedClass) curr.headStringLc else currType.toString
      val antTypePron = if (antType.isClosedClass) ant.headStringLc else antType.toString
      maybeAddFeat(feats, currTypePron + "-" + antTypePron + "-HM=" + headMatch, addToIndexer)
    }
    feats.toArray
  }
  
  /**
   * Extract feats from a single mention for use in the discourse new features
   */
  def extractDiscourseNewFeats(docGraph: DocumentGraph, mentIdx: Int, addToIndexer: Boolean): Array[Int] = {
    val feats = new ArrayBuffer[Int]
    val curr = docGraph.getMention(mentIdx)
    val currType = curr.mentionType
    maybeAddFeat(feats, "Bias", addToIndexer)
    if (featSpec.contains("more") || featSpec.contains("most")) {
      maybeAddFeat(feats, currType.toString(), addToIndexer)
    }
    if (featSpec.contains("prons")) {
      val currTypePron = if (currType.isClosedClass) curr.headStringLc else currType.toString
      maybeAddFeat(feats, currTypePron, addToIndexer)
    }
    feats.toArray
  }
}

object OutputFeaturizer {
  
  def featurizeAll(outputFeaturizer: OutputFeaturizer, docGraphs: Seq[DocumentGraph]) {
    for (docGraph <- docGraphs) {
      for (i <- 0 until docGraph.size) {
        for (j <- 0 until i) {
          outputFeaturizer.extractFeats(docGraph, i, j, true)
        }
      }
    }
  }
  
  def featurizeAllDiscourseNew(outputFeaturizer: OutputFeaturizer, docGraphs: Seq[DocumentGraph]) {
    for (docGraph <- docGraphs) {
      for (i <- 0 until docGraph.size) {
        outputFeaturizer.extractDiscourseNewFeats(docGraph, i, true)
      }
    }
  }
}
