package epic.parser
package models

import scala.collection.mutable.HashMap
import scala.util.Random
import scala.collection.GenTraversable
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.IdentityTransform
import epic.dense.OutputTransform
import epic.dense.Transform
import epic.dense.Word2VecDepFeaturizerIndexed
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.features._
import epic.framework.Feature
import epic.framework.StandardExpectedCounts
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import scala.collection.mutable.ArrayBuffer

/**
 * Main neural CRF parser class.
 *
 * @author gdurrett
 **/
@SerialVersionUID(1L)
class PositionalNeuralModel[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                      val constrainer: ChartConstraints.Factory[L, W],
                                      val topology: RuleTopology[L],
                                      val lexicon: Lexicon[L, W],
                                      refinedTopology: RuleTopology[L2],
                                      refinements: GrammarRefinements[L, L2],
                                      labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                      surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[W],
                                      depFeaturizer: Word2VecDepFeaturizerIndexed[W],
                                      val transforms: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]],
                                      val maybeSparseSurfaceFeaturizer: Option[IndexedSpanFeaturizer[L, L2, W]],
                                      val depTransforms: Seq[OutputTransform[Array[Int],DenseVector[Double]]],
                                      val decoupledTransforms: Seq[OutputTransform[Array[Int],DenseVector[Double]]]) extends ParserModel[L, W] with Serializable {
  
  def mergeWeightsForEnsembling(x1: DenseVector[Double], x2: DenseVector[Double]) = {
    require(decoupledTransforms.isEmpty)
    require(x1.size == x2.size)
    // Stack up the dense parts, average the sparse parts
    if (maybeSparseSurfaceFeaturizer.isDefined) {
      val sparseFeatsStart = index.componentOffset(index.indices.size - 1)
      val summedSparseFeatures = x1(sparseFeatsStart to -1) + x2(sparseFeatsStart to -1)
      DenseVector.vertcat(x1(0 until sparseFeatsStart), x2(0 until sparseFeatsStart), summedSparseFeatures)
    } else {
      DenseVector.vertcat(x1, x2)
    }
  }
  
  def cloneModelForEnsembling = {
    require(decoupledTransforms.isEmpty)
    // Note that duping the transforms is okay because they still produce distinct
    // layers, so caching behavior is unaffected
    val newTransforms = transforms ++ transforms
    val newDepTransforms = depTransforms ++ depTransforms
    new PositionalNeuralModel(annotator, constrainer, topology, lexicon, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, depFeaturizer,
                                 newTransforms, maybeSparseSurfaceFeaturizer, newDepTransforms, decoupledTransforms)
  }
  
  override type Inference = PositionalNeuralModel.Inference[L, L2, W]

  override def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    // println("Extracting ecounts")
    inf.grammar.extractEcounts(m, accum.counts, scale)
    
    if (maybeSparseSurfaceFeaturizer.isDefined) {
      val f = maybeSparseSurfaceFeaturizer.get
      val innerAccum = StandardExpectedCounts.zero(f.index)
      m.expectedCounts(maybeSparseSurfaceFeaturizer.get, innerAccum, scale)
      //      val totalTransformSize = transform.index.size
      val totalTransformSize = transforms.map(_.index.size).sum + depTransforms.map(_.index.size).sum  + decoupledTransforms.map(_.index.size).sum
      accum.counts += DenseVector.vertcat(DenseVector.zeros[Double](totalTransformSize), innerAccum.counts)
    }
    // println("Ecounts extracted")
    accum.loss += scale * m.logPartition
  }

  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  val index = if (maybeSparseSurfaceFeaturizer.isDefined) {
    SegmentedIndex(transforms.map(_.index) ++ depTransforms.map(_.index) ++ decoupledTransforms.map(_.index) ++ IndexedSeq(maybeSparseSurfaceFeaturizer.get.index):_*)
  } else {
    SegmentedIndex(transforms.map(_.index) ++ depTransforms.map(_.index) ++ decoupledTransforms.map(_.index):_*)
  }
  
  def initialWeightVector(initWeightsScale: Double, initializerSpec: String, trulyRandom: Boolean = false): DenseVector[Double] = {
    val rng = if (trulyRandom) new Random() else new Random(0)
    val initTransformWeights = DenseVector.vertcat(transforms.map(_.initialWeightVector(initWeightsScale, rng, true, initializerSpec)):_*)
    val initDepWeights = DenseVector.vertcat(depTransforms.map(_.initialWeightVector(initWeightsScale, rng, true, initializerSpec)):_*)
    val initDecoupledWeights = DenseVector.vertcat(decoupledTransforms.map(_.initialWeightVector(initWeightsScale, rng, true, initializerSpec)):_*)
    val newInitVector: DenseVector[Double] = if (maybeSparseSurfaceFeaturizer.isDefined) {
      DenseVector.vertcat(initTransformWeights, initDepWeights, initDecoupledWeights, DenseVector.zeros(maybeSparseSurfaceFeaturizer.get.index.size))
    } else {
      DenseVector.vertcat(initTransformWeights, initDepWeights, initDecoupledWeights)
    }
    require(newInitVector.size == index.size, newInitVector.size + " " + index.size)
    newInitVector
  }
  
  override def featureIndex: Index[Feature] = index

  override def inferenceFromWeights(weights: DenseVector[Double]): Inference = inferenceFromWeights(weights, true)
  
  def inferenceFromWeights(weights: DenseVector[Double], forTrain: Boolean): Inference = {
    val layersAndInnerLayers = transforms.indices.map { i =>
      transforms(i).extractLayerAndPenultimateLayer(weights(index.componentOffset(i) until index.componentOffset(i) + index.indices(i).size), forTrain)
    }
    val layers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer] = layersAndInnerLayers.map(_._1)
    val innerLayers: IndexedSeq[epic.dense.Transform.Layer[Array[Int],DenseVector[Double]]] = layersAndInnerLayers.map(_._2)
    val depLayers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer] = depTransforms.indices.map { i =>
      val idxIdx = transforms.size + i
      depTransforms(i).extractLayer(weights(index.componentOffset(idxIdx) until index.componentOffset(idxIdx) + index.indices(idxIdx).size), forTrain)
    }
    val decoupledLayersAndInner = decoupledTransforms.indices.map { i =>
      val idxIdx = transforms.size + depTransforms.size + i
      decoupledTransforms(i).extractLayerAndPenultimateLayer(weights(index.componentOffset(idxIdx) until index.componentOffset(idxIdx) + index.indices(idxIdx).size), forTrain)
    }
    val decoupledLayers = decoupledLayersAndInner.map(_._1)
    val decoupledInnerLayers = decoupledLayersAndInner.map(_._2)
    val grammar = new PositionalNeuralModel.PositionalNeuralGrammar[L, L2, W](topology, lexicon, refinedTopology, refinements, labelFeaturizer,
                                                                                   surfaceFeaturizer, depFeaturizer, layers, innerLayers, depLayers, maybeSparseSurfaceFeaturizer, decoupledLayers, decoupledInnerLayers, weights, this)
    new Inference(annotator, constrainer, grammar, refinements)
  }
  
  /**
   * When doing batch normalization, we need to normalize the test network
   */
  def extractParser(weights: DenseVector[Double], trainExs: Seq[TreeInstance[L,W]])(implicit deb: Debinarizer[L]) = {
    val inf = inferenceFromWeights(weights).forTesting
    inf.relativizeToData(trainExs.slice(0, Math.min(trainExs.size, 200)).asInstanceOf[Seq[TreeInstance[AnnotatedLabel,String]]])
    Parser(constrainer, inf.grammar, ChartDecoder[L, W]())
  }

  override def initialValueForFeature(f: Feature): Double = 0.0
}

object PositionalNeuralModel {

  case class Inference[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                 constrainer: ChartConstraints.Factory[L, W],
                                 grammar: PositionalNeuralGrammar[L, L2, W],
                                 refinements: GrammarRefinements[L, L2]) extends ParserInference[L, W]  {
    override def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {

      import ti._

      val annotated = annotator(tree, words).map(_.map(refinements.labels.localize))

      val product = grammar.anchor(words, constrainer.constraints(ti.words))
      LatentTreeMarginal(product, annotated)
    }
    
    // This needs to be different for dropout, so that we can get the right layers
    override def forTesting = grammar.origPTModel.inferenceFromWeights(grammar.weights, false)
    
    def relativizeToData(data: GenTraversable[TreeInstance[AnnotatedLabel,String]]) {
    }
  }

  @SerialVersionUID(4749637878577393596L)
  class PositionalNeuralGrammar[L, L2, W](val topology: RuleTopology[L],
                                             val lexicon: Lexicon[L, W],
                                             val refinedTopology: RuleTopology[L2],
                                             val refinements: GrammarRefinements[L, L2],
                                             labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                             val surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[W],
                                             depFeaturizer: Word2VecDepFeaturizerIndexed[W],
                                             val layers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer],
                                             penultimateLayers: IndexedSeq[epic.dense.Transform.Layer[Array[Int],DenseVector[Double]]],
                                             depLayers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer],
                                             val maybeSparseSurfaceFeaturizer: Option[IndexedSpanFeaturizer[L, L2, W]],
                                             decoupledLayers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer],
                                             penultimateDecoupledLayers: IndexedSeq[epic.dense.Transform.Layer[Array[Int],DenseVector[Double]]],
                                             val weights: DenseVector[Double],
                                             val origPTModel: PositionalNeuralModel[L,L2,W]) extends Grammar[L, W] with Serializable {

    val SpanLayerIdx = 0
    val UnaryLayerIdx = 1
    val BinaryLayerIdx = 2
    val dcSpanFeatOffset = layers.map(_.index.size).sum + depLayers.map(_.index.size).sum
    val dcUnaryFeatOffset = dcSpanFeatOffset + (if (decoupledLayers.nonEmpty) decoupledLayers(0).index.size else 0)
    val dcBinaryFeatOffset = dcUnaryFeatOffset + (if (decoupledLayers.nonEmpty) decoupledLayers(1).index.size else 0)
    
    override def withPermissiveLexicon: Grammar[L, W] = {
      new PositionalNeuralGrammar(topology, lexicon.morePermissive, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer,
                                     depFeaturizer, layers, penultimateLayers, depLayers, maybeSparseSurfaceFeaturizer, decoupledLayers, penultimateDecoupledLayers, weights, origPTModel)
    }

    /**
     * N.B. does not extracted expected counts for sparse features; this is done outside this loop
     */
    def extractEcounts(m: ParseMarginal[L, W], deriv: DenseVector[Double], scale: Double): Unit = {
      val w = m.words
      val length = w.length
      val sspec = surfaceFeaturizer.anchor(w)
      val depSpec = depFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)
      
      // val maxTetraLen = ((w.size + 2) * (w.size + 3) * (w.size + 4))/6 + ((w.size + 1) * (w.size + 2))/2 + w.size + 2
      
      def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }
      
      // This representation appears to make things a bit faster?
      val ruleCountsPerState = new HashMap[Int,SparseVector[Double]]
      val unaryRuleCountsPerState = new HashMap[Int,SparseVector[Double]]
      val binaryRuleCountsPerState = new HashMap[Int,SparseVector[Double]]
      val spanCountsPerState = new HashMap[Int,SparseVector[Double]]
      // val ruleCountsPerState = Array.fill(maxTetraLen)(SparseVector.zeros[Double](labelFeaturizer.index.size))
      // val countsPerHeadDepPair = Array.tabulate(w.size, w.size)((i, j) => 0.0)
      // val statesUsed = Array.fill(maxTetraLen)(false)
      // val untetra = Array.fill(maxTetraLen)((-1, -1, -1))
      val untetra = new HashMap[Int,(Int,Int,Int)]
      
      m visit new AnchoredVisitor[L] {
        
        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 1)
          untetra(tetraIdx) = (begin, end, length + 1)
          val fv = new FeatureVector(lspec.featuresForUnaryRule(begin, end, rule, ref))
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          axpy(score, fv, ruleCountsPerState(tetraIdx))
          if (decoupledLayers.nonEmpty) {
            if (!unaryRuleCountsPerState.contains(tetraIdx)) unaryRuleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
            axpy(score, fv, unaryRuleCountsPerState(tetraIdx))
          }
        }

        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 2)
          untetra(tetraIdx) = (begin, end, length + 2)
          val fv = new FeatureVector(lspec.featuresForSpan(begin, end, tag, ref))
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          axpy(score, fv, ruleCountsPerState(tetraIdx))
          if (decoupledLayers.nonEmpty) {
            if (!spanCountsPerState.contains(tetraIdx)) spanCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
            axpy(score, fv, spanCountsPerState(tetraIdx))
          }
        }

        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, split, end)
          untetra(tetraIdx) = (begin, split, end)
          val fv = new FeatureVector(lspec.featuresForBinaryRule(begin, split, end, rule, ref))
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          axpy(score, fv, ruleCountsPerState(tetraIdx))
          if (decoupledLayers.nonEmpty) {
            if (!binaryRuleCountsPerState.contains(tetraIdx)) binaryRuleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
            axpy(score, fv, binaryRuleCountsPerState(tetraIdx))
          }
        }
      }

      for (key <- ruleCountsPerState.keySet) {
        val (begin, split, end) = untetra(key)
        val ffeats = if (end > length) sspec.featuresForSpan(begin, split) else sspec.featuresForSplit(begin, split, end)
        var layerSizeTally = 0
        layers.indices.foreach { j =>
          layers(j).tallyDerivative(deriv(layerSizeTally until layerSizeTally + layers(j).index.size), { ruleCountsPerState(key) * scale }, ffeats)
          layerSizeTally += layers(j).index.size
        }
      }
      if (decoupledLayers.nonEmpty) {
        for (key <- spanCountsPerState.keySet) {
          val (begin, end, _) = untetra(key)
          val ffeats = sspec.reducedFeaturesForSpan(begin, end)
          decoupledLayers(SpanLayerIdx).tallyDerivative(deriv(dcSpanFeatOffset until dcSpanFeatOffset + decoupledLayers(SpanLayerIdx).index.size), { spanCountsPerState(key) * scale }, ffeats)
        }
        for (key <- unaryRuleCountsPerState.keySet) {
          val (begin, end, _) = untetra(key)
          val ffeats = sspec.reducedFeaturesForSpan(begin, end)
          decoupledLayers(UnaryLayerIdx).tallyDerivative(deriv(dcUnaryFeatOffset until dcUnaryFeatOffset + decoupledLayers(UnaryLayerIdx).index.size), { unaryRuleCountsPerState(key) * scale }, ffeats)
        }
        for (key <- binaryRuleCountsPerState.keySet) {
          val (begin, split, end) = untetra(key)
          val ffeats = sspec.featuresForSplit(begin, split, end)
          decoupledLayers(BinaryLayerIdx).tallyDerivative(deriv(dcBinaryFeatOffset until dcBinaryFeatOffset + decoupledLayers(BinaryLayerIdx).index.size), { binaryRuleCountsPerState(key) * scale }, ffeats)
        }
      }
    }

    def anchor(w: IndexedSeq[W], cons: ChartConstraints[L]):GrammarAnchoring[L, W] = new ProjectionsGrammarAnchoring[L, L2, W] {
      
      override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        anchor(w, cons & constraints)
      }

      override def sparsityPattern: ChartConstraints[L] = cons

      def refinements = PositionalNeuralGrammar.this.refinements
      def refinedTopology: RuleTopology[L2] = PositionalNeuralGrammar.this.refinedTopology

      val topology = PositionalNeuralGrammar.this.topology
      val lexicon = PositionalNeuralGrammar.this.lexicon

      def words = w

      val l = w.size
      val maxTetraLen = ((l + 2) * (l + 3) * (l + 4))/6 + ((l + 1) * (l + 2))/2 + l + 2
      
      // Doesn't make things faster to use HashMaps here
      val cache = Array.tabulate(layers.size + decoupledLayers.size)(i => new Array[DenseVector[Double]](maxTetraLen))
      val finalCache = Array.tabulate(layers.size + decoupledLayers.size)(i => new Array[SparseVector[Double]](maxTetraLen))
      
      def getOrElseUpdate(layerIdx: Int, tetraIdx: Int, fun: => DenseVector[Double]) = {
        if (cache(layerIdx)(tetraIdx) == null) cache(layerIdx)(tetraIdx) = fun
        cache(layerIdx)(tetraIdx)
      }

      def getOrElseUpdateFinal(layerIdx: Int, tetraIdx: Int, rfeatIdx: Int, maxVectSize: Int, fun: => Double) = {
        if (finalCache(layerIdx)(tetraIdx) == null) finalCache(layerIdx)(tetraIdx) = SparseVector.zeros(maxVectSize)
        if (!finalCache(layerIdx)(tetraIdx).contains(rfeatIdx)) finalCache(layerIdx)(tetraIdx)(rfeatIdx) = fun
        finalCache(layerIdx)(tetraIdx)(rfeatIdx)
      }
      
      val sspec = surfaceFeaturizer.anchor(w)
      val depSpec = depFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)
      val fspec = if (maybeSparseSurfaceFeaturizer.isDefined) maybeSparseSurfaceFeaturizer.get.anchor(w) else null
      val sparseFeatsStart = if (maybeSparseSurfaceFeaturizer.isDefined) layers.map(_.index.size).sum + depLayers.map(_.index.size).sum + decoupledLayers.map(_.index.size).sum else -1

      private def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }
      
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        var total = 0.0
        val tetraIdx = tetra(begin, split, end)
        val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
        layers.indices.foreach { layerIdx =>
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSplit(begin, split, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (decoupledLayers.nonEmpty) {
          val layerIdx = layers.size + BinaryLayerIdx
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateDecoupledLayers(BinaryLayerIdx).activations(sspec.featuresForSplit(begin, split, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { decoupledLayers(BinaryLayerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForBinaryRule(begin, split, end, rule, ref), sparseFeatsStart)
        }
        total
      }
      
      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        var total = 0.0
        val tetraIdx = tetra(begin, end, length + 1)
        val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
        layers.indices.foreach { layerIdx =>
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (decoupledLayers.nonEmpty) {
          val layerIdx = layers.size + UnaryLayerIdx
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateDecoupledLayers(UnaryLayerIdx).activations(sspec.reducedFeaturesForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { decoupledLayers(UnaryLayerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForUnaryRule(begin, end, rule, ref), sparseFeatsStart)
        }
        total
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
        var total = 0.0
        val tetraIdx = tetra(begin, end, length + 2)
        val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
        layers.indices.foreach { layerIdx =>
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (decoupledLayers.nonEmpty) {
          val layerIdx = layers.size + SpanLayerIdx
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateDecoupledLayers(SpanLayerIdx).activations(sspec.reducedFeaturesForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { decoupledLayers(SpanLayerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForSpan(begin, end, tag, ref), sparseFeatsStart)
        }
        total
      }

      private def dot(features: Array[Int], sparseFeaturesOffset: Int) = {
        var i = 0
        var score = 0.0
        val wdata = weights.data
        while (i < features.length) {
          score += wdata(features(i) + sparseFeaturesOffset)
          i += 1
        }
        score
      }
    }
  }
}
