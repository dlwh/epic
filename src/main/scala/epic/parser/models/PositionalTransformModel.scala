package epic.parser
package models

import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.IdentityTransform
import epic.dense.Transform
import epic.dense.OutputTransform
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.dense.Word2VecDepFeaturizerIndexed
import epic.features._
import epic.framework.Feature
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import spire.math.fpf.MaybeDouble
import epic.framework.StandardExpectedCounts
import scala.util.Random
import scala.collection.mutable.HashMap

/**
 * TODO
 *
 * @author dlwh
 **/
class PositionalTransformModel[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
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
                               val depTransforms: Seq[OutputTransform[Array[Int],DenseVector[Double]]]) extends ParserModel[L, W] {
  
  def cloneModelForEnsembling = {
    val newTransforms = transforms ++ transforms;
    // Can't have anything else in there for the parameter vector ramming to work
    require(depTransforms.isEmpty && !maybeSparseSurfaceFeaturizer.isDefined)
    new PositionalTransformModel(annotator, constrainer, topology, lexicon, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, depFeaturizer,
                                 newTransforms, maybeSparseSurfaceFeaturizer, depTransforms)
  }
  
  override type Inference = PositionalTransformModel.Inference[L, L2, W]

//  override def accumulateCounts(inf: Inference, d: TreeInstance[L, W], accum: ExpectedCounts, scale: Double):Unit = {
//    val s = inf.scorer(d)
//    val m = inf.marginal(s, d)
//    val gm = inf.goldMarginal(s, d)
//    accumulateCounts(inf, s, d, m, accum, scale)
//    accumulateCounts(inf, s, d, gm, accum, -scale)
//  }

  override def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
//    println("Extracting ecounts")
    inf.grammar.extractEcounts(m, accum.counts, scale)
    
    if (maybeSparseSurfaceFeaturizer.isDefined) {
      val f = maybeSparseSurfaceFeaturizer.get
      val innerAccum = StandardExpectedCounts.zero(f.index)
      m.expectedCounts(maybeSparseSurfaceFeaturizer.get, innerAccum, scale)
      //      val totalTransformSize = transform.index.size
      val totalTransformSize = transforms.map(_.index.size).reduce(_ + _)
      accum.counts += DenseVector.vertcat(DenseVector.zeros[Double](totalTransformSize), innerAccum.counts)
    }
//    println("Ecounts extracted")
    accum.loss += scale * m.logPartition
  }

  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  
//  val index = if (maybeSparseSurfaceFeaturizer.isDefined) {
//    SegmentedIndex(transform.index, maybeSparseSurfaceFeaturizer.get.index)
//  } else {
//    transform.index
//  }
  val index = if (maybeSparseSurfaceFeaturizer.isDefined) {
    SegmentedIndex((transforms.map(_.index) ++ depTransforms.map(_.index) ++ IndexedSeq(maybeSparseSurfaceFeaturizer.get.index)):_*)
  } else {
    SegmentedIndex((transforms.map(_.index) ++ depTransforms.map(_.index)):_*)
  }
  
  def initialWeightVector(randomize: Boolean, initWeightsScale: Double, initializerSpec: String, trulyRandom: Boolean = false): DenseVector[Double] = {
    val rng = if (trulyRandom) new Random() else new Random(0)
//    val initTransformWeights = transform.initialWeightVector(initWeightsScale, rng, true);
    val initTransformWeights = DenseVector.vertcat(transforms.map(_.initialWeightVector(initWeightsScale, rng, true, initializerSpec)):_*);
    val initDepWeights = DenseVector.vertcat(depTransforms.map(_.initialWeightVector(initWeightsScale, rng, true, initializerSpec)):_*);
    val newInitVector: DenseVector[Double] = if (maybeSparseSurfaceFeaturizer.isDefined) {
      DenseVector.vertcat(initTransformWeights, initDepWeights, DenseVector.zeros(maybeSparseSurfaceFeaturizer.get.index.size))
    } else {
      DenseVector.vertcat(initTransformWeights, initDepWeights)
    }
    newInitVector
  }
  
//  override def featureIndex: Index[Feature] = transform.index
  override def featureIndex: Index[Feature] = index

  override def inferenceFromWeights(weights: DenseVector[Double]): Inference = inferenceFromWeights(weights, true)
  
  def inferenceFromWeights(weights: DenseVector[Double], forTrain: Boolean): Inference = {
    val layersAndInnerLayers = for (i <- 0 until transforms.size) yield {
      transforms(i).extractLayerAndPenultimateLayer(weights(index.componentOffset(i) until index.componentOffset(i) + index.indices(i).size), forTrain)
    }
    val layers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer] = layersAndInnerLayers.map(_._1)
    val innerLayers: IndexedSeq[epic.dense.Transform.Layer[Array[Int],DenseVector[Double]]] = layersAndInnerLayers.map(_._2)
    val depLayers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer] = for (i <- 0 until depTransforms.size) yield {
      val idxIdx = transforms.size + i
//      println("dep " + i + ": " + index.componentOffset(idxIdx))
      depTransforms(i).extractLayer(weights(index.componentOffset(idxIdx) until index.componentOffset(idxIdx) + index.indices(idxIdx).size), forTrain)
    }
    val grammar = new PositionalTransformModel.PositionalTransformGrammar[L, L2, W](topology, lexicon, refinedTopology, refinements, labelFeaturizer,
                                                                                   surfaceFeaturizer, depFeaturizer, layers, innerLayers, depLayers, maybeSparseSurfaceFeaturizer, weights, this)
    new Inference(annotator, constrainer, grammar, refinements)
  }

  override def initialValueForFeature(f: Feature): Double = 0.0
}

object PositionalTransformModel {

  case class Inference[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                 constrainer: ChartConstraints.Factory[L, W],
                                 grammar: PositionalTransformGrammar[L, L2, W],
                                 refinements: GrammarRefinements[L, L2]) extends ParserInference[L, W]  {
    override def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {

      import ti._

      val annotated = annotator(tree, words).map(_.map(refinements.labels.localize))

      val product = grammar.anchor(words, constrainer.constraints(ti.words))
      LatentTreeMarginal(product, annotated)
    }
    
    // This needs to be different for dropout, so that we can get the right layers
//    override def forTesting = this
    override def forTesting = grammar.origPTModel.inferenceFromWeights(grammar.weights, false)
  }

  @SerialVersionUID(4749637878577393596L)
  class PositionalTransformGrammar[L, L2, W](val topology: RuleTopology[L],
                                             val lexicon: Lexicon[L, W],
                                             val refinedTopology: RuleTopology[L2],
                                             val refinements: GrammarRefinements[L, L2],
                                             labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                             surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[W],
                                             depFeaturizer: Word2VecDepFeaturizerIndexed[W],
                                             layers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer],
                                             penultimateLayers: IndexedSeq[epic.dense.Transform.Layer[Array[Int],DenseVector[Double]]],
                                             depLayers: IndexedSeq[OutputTransform[Array[Int],DenseVector[Double]]#OutputLayer],
                                             val maybeSparseSurfaceFeaturizer: Option[IndexedSpanFeaturizer[L, L2, W]],
                                             val weights: DenseVector[Double],
                                             val origPTModel: PositionalTransformModel[L,L2,W]) extends Grammar[L, W] with Serializable {

    override def withPermissiveLexicon: Grammar[L, W] = {
      new PositionalTransformGrammar(topology, lexicon.morePermissive, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, depFeaturizer, layers, penultimateLayers, depLayers, maybeSparseSurfaceFeaturizer, weights, origPTModel)
    }


    def extractEcounts(m: ParseMarginal[L, W], deriv: DenseVector[Double], scale: Double): Unit = {
      val w = m.words
      val length = w.length
      val sspec = surfaceFeaturizer.anchor(w)
      val depSpec = depFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)
      
//      val maxTetraLen = ((w.size + 2) * (w.size + 3) * (w.size + 4))/6 + ((w.size + 1) * (w.size + 2))/2 + w.size + 2
      
      def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }
      
      // This representation appears to make things a bit faster?
      val ruleCountsPerState = new HashMap[Int,SparseVector[Double]]
//      val ruleCountsPerState = Array.fill(maxTetraLen)(SparseVector.zeros[Double](labelFeaturizer.index.size))
//      val countsPerHeadDepPair = Array.tabulate(w.size, w.size)((i, j) => 0.0)
//      val statesUsed = Array.fill(maxTetraLen)(false)
//      val untetra = Array.fill(maxTetraLen)((-1, -1, -1))
      val untetra = new HashMap[Int,(Int,Int,Int)]
      
      m visit new AnchoredVisitor[L] {
        
        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 1)
//          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, end, length + 1)
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          
          axpy(score, new FeatureVector(lspec.featuresForUnaryRule(begin, end, rule, ref)), ruleCountsPerState(tetraIdx))
        }

        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 2)
//          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, end, length + 2)
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          
          axpy(score, new FeatureVector(lspec.featuresForSpan(begin, end, tag, ref)), ruleCountsPerState(tetraIdx))
          
        }

        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, split, end)
//          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, split, end)
          if (!ruleCountsPerState.contains(tetraIdx)) ruleCountsPerState.put(tetraIdx, SparseVector.zeros[Double](labelFeaturizer.index.size))
          
          axpy(score, new FeatureVector(lspec.featuresForBinaryRule(begin, split, end, rule, ref)), ruleCountsPerState(tetraIdx))
          
//          val (headIdx, childIdx) = depSpec.getHeadDepPair(begin, split, end, rule)
//          countsPerHeadDepPair(headIdx)(childIdx) += score
        }
      }
      
//      println("MTL: " + maxTetraLen + ", " + w.size)
//      println(tetra(w.length - 1, w.length, w.length + 2))
//      println(statesUsed.map(a => if (a) 1 else 0).reduce(_ + _) + " used")

      
      for (key <- ruleCountsPerState.keySet) {
        val (begin, split, end) = untetra(key)
        val ffeats = if (end > length) sspec.featuresForSpan(begin, split) else sspec.featuresForSplit(begin, split, end)
        var layerSizeTally = 0
        for (j <- 0 until layers.size) {
          layers(j).tallyDerivative(deriv(layerSizeTally until layerSizeTally + layers(j).index.size), { ruleCountsPerState(key) * scale }, ffeats)
          layerSizeTally += layers(j).index.size;
        }
      }
      
//      for (i <- 0 until ruleCountsPerState.length) {
//        if (statesUsed(i)) {
//          val (begin, split, end) = untetra(i)
//          val ffeats = if (end > length) sspec.featuresForSpan(begin, split) else sspec.featuresForSplit(begin, split, end)
//          var layerSizeTally = 0
//          for (j <- 0 until layers.size) {
//            layers(j).tallyDerivative(deriv(layerSizeTally until layerSizeTally + layers(j).index.size), { ruleCountsPerState(i) * scale }, ffeats)
//            layerSizeTally += layers(j).index.size;
//          }
//        }
//      }
//      for (headIdx <- 0 until w.size) {
//        for (childIdx <- 0 until w.size) {
//          var layerSizeTally = layers.map(_.index.size).foldLeft(0)(_ + _)
//          for (j <- 0 until depLayers.size) {
//            val score = countsPerHeadDepPair(headIdx)(childIdx)
//            if (score != 0 && Math.abs(score) > 1e-8) {
//              depLayers(j).tallyDerivative(deriv(layerSizeTally until layerSizeTally + depLayers(j).index.size), DenseVector(Array(score * scale)), depSpec.featuresForHeadPair(headIdx, childIdx))
//            }
//            layerSizeTally += depLayers(j).index.size
//          }
//        }
//      }
    }

    def anchor(w: IndexedSeq[W], cons: ChartConstraints[L]):GrammarAnchoring[L, W] = new ProjectionsGrammarAnchoring[L, L2, W] {

//      var numScored = 0
      
      override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        anchor(w, cons & constraints)
      }

      override def sparsityPattern: ChartConstraints[L] = cons

      def refinements = PositionalTransformGrammar.this.refinements
      def refinedTopology: RuleTopology[L2] = PositionalTransformGrammar.this.refinedTopology

      val topology = PositionalTransformGrammar.this.topology
      val lexicon = PositionalTransformGrammar.this.lexicon

      def words = w

      val l = w.size
      val maxTetraLen = ((l + 2) * (l + 3) * (l + 4))/6 + ((l + 1) * (l + 2))/2 + l + 2
      
      // Doesn't make things faster to use HashMaps here
      val cache = Array.tabulate(layers.size)(i => new Array[DenseVector[Double]](maxTetraLen))
      val finalCache = Array.tabulate(layers.size)(i => new Array[SparseVector[Double]](maxTetraLen))
      
//      val headDepCache = Array.tabulate(w.size, w.size)((i, j) => 0.0)
//      val headDepStale = Array.tabulate(w.size, w.size)((i, j) => true)

      def getOrElseUpdate(layerIdx: Int, tetraIdx: Int, fun: => DenseVector[Double]) = {
        if (cache(layerIdx)(tetraIdx) == null) cache(layerIdx)(tetraIdx) = fun
        cache(layerIdx)(tetraIdx)
      }

      def getOrElseUpdateFinal(layerIdx: Int, tetraIdx: Int, rfeatIdx: Int, maxVectSize: Int, fun: => Double) = {
        if (finalCache(layerIdx)(tetraIdx) == null) finalCache(layerIdx)(tetraIdx) = SparseVector.zeros(maxVectSize)
        if (!finalCache(layerIdx)(tetraIdx).contains(rfeatIdx)) finalCache(layerIdx)(tetraIdx)(rfeatIdx) = fun
        finalCache(layerIdx)(tetraIdx)(rfeatIdx)
      }
      
//      def getOrElseUpdateHeadDep(headIdx: Int, depIdx: Int, fun: => Double) = {
//        if (headDepStale(headIdx)(depIdx)) {
//          headDepCache(headIdx)(depIdx) = fun
//          headDepStale(headIdx)(depIdx) = false
//        }
//        headDepCache(headIdx)(depIdx)
//      }
      
      val sspec = surfaceFeaturizer.anchor(w)
      val depSpec = depFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)
      val fspec = if (maybeSparseSurfaceFeaturizer.isDefined) maybeSparseSurfaceFeaturizer.get.anchor(w) else null
      val sparseFeatsStart = if (maybeSparseSurfaceFeaturizer.isDefined) (layers.map(_.index.size).foldLeft(0)(_ + _) + depLayers.map(_.index.size).foldLeft(0)(_ + _)) else -1

      private def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }
      
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        var total = 0.0;
        val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
        for (layerIdx <- 0 until layers.size) {
          val tetraIdx = tetra(begin, split, end)
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSplit(begin, split, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
//        if (depLayers.size > 0) {
//          val (headIdx, childIdx) = depSpec.getHeadDepPair(begin, split, end, rule)
//          val pairFeats = depSpec.featuresForHeadPair(headIdx, childIdx)
//          total += getOrElseUpdateHeadDep(headIdx, childIdx, { depLayers.map(layer => layer.activations(pairFeats)).reduce(_ + _).data(0) })
//        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForBinaryRule(begin, split, end, rule, ref), sparseFeatsStart)
        }
        total
      }
      
      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        var total = 0.0;
        val tetraIdx = tetra(begin, end, length + 1)
        val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
        for (layerIdx <- 0 until layers.size) {
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
          }
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForUnaryRule(begin, end, rule, ref), sparseFeatsStart)
        }
        total
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
        var total = 0.0;
        val tetraIdx = tetra(begin, end, length + 2)
        val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
        for (layerIdx <- 0 until layers.size) {
          val fs = getOrElseUpdate(layerIdx, tetraIdx, { penultimateLayers(layerIdx).activations(sspec.featuresForSpan(begin, end)) })
          for (rfeat <- rfeats) {
            total += getOrElseUpdateFinal(layerIdx, tetraIdx, rfeat, labelFeaturizer.index.size, { layers(layerIdx).activationsFromPenultimateDot(fs, rfeat) })
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
        while(i < features.length) {
          score += wdata(features(i) + sparseFeaturesOffset)
          i += 1
        }
        score
      }
    }
  }
}
