package epic.parser
package models

import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.AffineTransformDense
import epic.dense.IdentityTransform
import epic.dense.Transform
import epic.dense.Transform
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import epic.features._
import epic.framework.Feature
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import spire.math.fpf.MaybeDouble
import epic.framework.StandardExpectedCounts
import scala.util.Random

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
                               val transform: AffineTransformDense[Array[Int]],
                               val maybeSparseSurfaceFeaturizer: Option[IndexedSpanFeaturizer[L, L2, W]]) extends ParserModel[L, W] {
  override type Inference = PositionalTransformModel.Inference[L, L2, W]


  override def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
//    println("Extracting ecounts")
    inf.grammar.extractEcounts(m, accum.counts, scale)
    
    if (maybeSparseSurfaceFeaturizer.isDefined) {
      val f = maybeSparseSurfaceFeaturizer.get
      val innerAccum = StandardExpectedCounts.zero(f.index)
      m.expectedCounts(maybeSparseSurfaceFeaturizer.get, innerAccum, scale)
      accum.counts += DenseVector.vertcat(DenseVector.zeros[Double](transform.index.size), innerAccum.counts)
    }
//    println("Ecounts extracted")
    accum.loss += scale * m.logPartition
  }

  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  
  val index = if (maybeSparseSurfaceFeaturizer.isDefined) {
    SegmentedIndex(transform.index, maybeSparseSurfaceFeaturizer.get.index)
  } else {
    transform.index
  }
  
  def initialWeightVector(randomize: Boolean, initWeightsScale: Double): DenseVector[Double] = {
//    val transformIndex = if (maybeSparseSurfaceFeaturizer.isDefined) {
//      index.asInstanceOf[SegmentedIndex[Feature,Index[Feature]]].indices(0)
//    } else {
//      index
//    }
//    val subIndices = transformIndex.asInstanceOf[SegmentedIndex[Feature,Index[Feature]]].indices
//    val startOfInnerLayers = subIndices(0).size;
//    val endOfInnerLayers = startOfInnerLayers + subIndices(1).size;
//    println("Setting higher initial values for weights from " + startOfInnerLayers + " to " + endOfInnerLayers)
//    val rng = new Random(0)
//    val oldInitVector = new DenseVector[Double](Array.tabulate(index.size)(i => {
//      if (i >= startOfInnerLayers && i < endOfInnerLayers) {
////        initWeightsScale * (if (randomizeGaussian) rng.nextGaussian else rng.nextDouble)
//        initWeightsScale * rng.nextGaussian
//      } else {
//        0.0
//      }
//    }));
    val initTransformWeights = transform.initialWeightVector(initWeightsScale, new Random(0));
    val newInitVector: DenseVector[Double] = if (maybeSparseSurfaceFeaturizer.isDefined) {
      DenseVector.vertcat(initTransformWeights, DenseVector.zeros(maybeSparseSurfaceFeaturizer.get.index.size))
    } else {
      initTransformWeights
    }
    newInitVector
  }
  
//  override def featureIndex: Index[Feature] = transform.index
  override def featureIndex: Index[Feature] = index

  override def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val (layer, innerLayer) = transform.extractLayerAndPenultimateLayer(weights)
    val grammar = new PositionalTransformModel.PositionalTransformGrammar[L, L2, W](topology, lexicon, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, layer, innerLayer, maybeSparseSurfaceFeaturizer, weights)
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
  }

  @SerialVersionUID(4749637878577393596L)
  class PositionalTransformGrammar[L, L2, W](val topology: RuleTopology[L],
                                   val lexicon: Lexicon[L, W],
                                   val refinedTopology: RuleTopology[L2],
                                   val refinements: GrammarRefinements[L, L2],
                                   labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                   surfaceFeaturizer: Word2VecSurfaceFeaturizerIndexed[W],
                                   layer: AffineTransformDense[Array[Int]]#Layer,
                                   penultimateLayer: epic.dense.Transform.Layer[Array[Int],DenseVector[Double]],
                                   val maybeSparseSurfaceFeaturizer: Option[IndexedSpanFeaturizer[L, L2, W]],
                                   weights: DenseVector[Double]) extends Grammar[L, W] with Serializable {

    override def withPermissiveLexicon: Grammar[L, W] = {
      new PositionalTransformGrammar(topology, lexicon.morePermissive, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, layer, penultimateLayer, maybeSparseSurfaceFeaturizer, weights)
    }


    def extractEcounts(m: ParseMarginal[L, W], deriv: DenseVector[Double], scale: Double): Unit = {
      val w = m.words
      val length = w.length
      val sspec = surfaceFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)

      // For each split point, remember the (begin, end) pair that that split point was observed with. There'll
      // only be one in the gold, but more in the prediction. Accumulate rule counts (output layer) until
      // we need this split point for a different set of indices or we come to the end. Then, backpropagate
      // the rule marginals through the network to get the derivative.
//      val UNUSED = (-1, -1)
//      val states = Array.fill(w.length + 2)(UNUSED) // 1 for each split,  length for unaries, length +1 for spans
//      val ruleCountsPerState = Array.fill(w.length + 2)(SparseVector.zeros[Double](labelFeaturizer.index.size))
//
//      def checkFlush(begin: Int, split: Int, end: Int) {
//        val state: (Int, Int) = (begin, end)
//        val oldState: (Int, Int) = states(split)
//        if(oldState != state) {
//          if(oldState != UNUSED) {
//            val ffeats = if(split >= length) sspec.featuresForSpan(oldState._1, oldState._2) else sspec.featuresForSplit(oldState._1, split, oldState._2)
//            layer.tallyDerivative(deriv, ruleCountsPerState(split) *= scale, ffeats)
//            ruleCountsPerState(split) := 0.0
//          }
//          states(split) = state
//        }
//      }
//      
//      m visit new AnchoredVisitor[L] {
//        
//        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
//          checkFlush(begin, length, end)
//          axpy(score, new FeatureVector(lspec.featuresForUnaryRule(begin, end, rule, ref)), ruleCountsPerState(length))
//        }
//
//        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
//          checkFlush(begin, length + 1, end)
//          axpy(score, new FeatureVector(lspec.featuresForSpan(begin, end, tag, ref)), ruleCountsPerState(length + 1))
//
//        }
//
//        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
//          checkFlush(begin, split, end)
//          axpy(score, new FeatureVector(lspec.featuresForBinaryRule(begin, split, end, rule, ref)), ruleCountsPerState(split))
//        }
//      }
//
//      for(i <- 0 until states.length) {
//        checkFlush(-1, i, -1) // force a flush
//      }
      
      
      val maxTetraLen = ((w.size + 2) * (w.size + 3) * (w.size + 4))/6 + ((w.size + 1) * (w.size + 2))/2 + w.size + 2
      
      def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }
      
      val ruleCountsPerState = Array.fill(maxTetraLen)(SparseVector.zeros[Double](labelFeaturizer.index.size))
      val statesUsed = Array.fill(maxTetraLen)(false)
      val untetra = Array.fill(maxTetraLen)((-1, -1, -1))
      
      m visit new AnchoredVisitor[L] {
        
        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 1)
          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, end, length + 1)
          axpy(score, new FeatureVector(lspec.featuresForUnaryRule(begin, end, rule, ref)), ruleCountsPerState(tetraIdx))
        }

        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, end, length + 2)
          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, end, length + 2)
          axpy(score, new FeatureVector(lspec.featuresForSpan(begin, end, tag, ref)), ruleCountsPerState(tetraIdx))
          
        }

        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val tetraIdx = tetra(begin, split, end)
          statesUsed(tetraIdx) = true;
          untetra(tetraIdx) = (begin, split, end)
          axpy(score, new FeatureVector(lspec.featuresForBinaryRule(begin, split, end, rule, ref)), ruleCountsPerState(tetraIdx))
        }
      }

      for (i <- 0 until ruleCountsPerState.length) {
        if (statesUsed(i)) {
          val (begin, split, end) = untetra(i)
          val ffeats = if (end > length) sspec.featuresForSpan(begin, split) else sspec.featuresForSplit(begin, split, end)
          layer.tallyDerivative(deriv, ruleCountsPerState(i) *= scale, ffeats)
        }
      }
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

//      val cache = new LRUCache[Long, Vector[Double]](2 * length)
//      val sspec = surfaceFeaturizer.anchor(w)
//      val lspec = labelFeaturizer.anchor(w)
//
//      private def tetra(begin: Int, split: Int, end: Int) = {
//        (end.toLong * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
//      }
//
//      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
//        val fs = cache.getOrElseUpdate(tetra(begin, split, end), {
//          val sfeats = sspec.featuresForSplit(begin, split, end)
//          layer.activations(sfeats)
//        })
//        val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
//        new FeatureVector(rfeats) dot fs
//      }
//
//      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
//        val fs = cache.getOrElseUpdate(tetra(begin, end, length + 1), {
//          val sfeats = sspec.featuresForSpan(begin, end)
//          layer.activations(sfeats)
//        })
//        val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
//        new FeatureVector(rfeats) dot fs
//      }
//
//      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
//        val fs = cache.getOrElseUpdate(tetra(begin, end, length + 1), {
//        val sfeats = sspec.featuresForSpan(begin, end)
//          layer.activations(sfeats)
//        })
//        val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
//        new FeatureVector(rfeats) dot fs
//      }
      
//      val cache = new LRUCache[Long, DenseVector[Double]](2 * length)
      val maxTetraLen = ((w.size + 2) * (w.size + 3) * (w.size + 4))/6 + ((w.size + 1) * (w.size + 2))/2 + w.size + 2
      val cache = new Array[DenseVector[Double]](maxTetraLen)
      val finalCache = new Array[SparseVector[Double]](maxTetraLen)
      
      def getOrElseUpdate(tetraIdx: Int, fun: => DenseVector[Double]) = {
        if (cache(tetraIdx) == null) cache(tetraIdx) = fun
        cache(tetraIdx)
      }
      
      def getOrElseUpdateFinal(tetraIdx: Int, rfeatIdx: Int, maxVectSize: Int, fun: => Double) = {
        if (finalCache(tetraIdx) == null) finalCache(tetraIdx) = SparseVector.zeros(maxVectSize)
        if (!finalCache(tetraIdx).contains(rfeatIdx)) finalCache(tetraIdx)(rfeatIdx) = fun
        finalCache(tetraIdx)(rfeatIdx)
      }
      
      val sspec = surfaceFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)
      val fspec = if (maybeSparseSurfaceFeaturizer.isDefined) maybeSparseSurfaceFeaturizer.get.anchor(w) else null

      private def tetra(begin: Int, split: Int, end: Int) = {
        (end * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        val tetraIdx = tetra(begin, split, end)
        val fs = getOrElseUpdate(tetraIdx, {
          val sfeats = sspec.featuresForSplit(begin, split, end)
          penultimateLayer.activations(sfeats)
        })
        val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
//        layer.activationsFromPenultimateDot(fs, rfeats)
        var total = 0.0;
        for (rfeat <- rfeats) {
          total += getOrElseUpdateFinal(tetraIdx, rfeat, labelFeaturizer.index.size, {
            layer.activationsFromPenultimateDot(fs, Array(rfeat))
          })
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForBinaryRule(begin, split, end, rule, ref), layer.index.size)
        }
        total
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        val tetraIdx = tetra(begin, end, length + 1)
        val fs = getOrElseUpdate(tetraIdx, {
          val sfeats = sspec.featuresForSpan(begin, end)
          penultimateLayer.activations(sfeats)
        })
        val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
//        layer.activationsFromPenultimateDot(fs, rfeats)
        var total = 0.0;
        for (rfeat <- rfeats) {
          total += getOrElseUpdateFinal(tetraIdx, rfeat, labelFeaturizer.index.size, {
            layer.activationsFromPenultimateDot(fs, Array(rfeat))
          })
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForUnaryRule(begin, end, rule, ref), layer.index.size)
        }
        total
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
        val tetraIdx = tetra(begin, end, length + 2)
        val fs = getOrElseUpdate(tetraIdx, {
        val sfeats = sspec.featuresForSpan(begin, end)
          penultimateLayer.activations(sfeats)
        })
        val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
//        layer.activationsFromPenultimateDot(fs, rfeats)
        var total = 0.0;
        for (rfeat <- rfeats) {
          total += getOrElseUpdateFinal(tetraIdx, rfeat, labelFeaturizer.index.size, {
            layer.activationsFromPenultimateDot(fs, Array(rfeat))
          })
        }
        if (maybeSparseSurfaceFeaturizer.isDefined) {
          total += dot(fspec.featuresForSpan(begin, end, tag, ref), layer.index.size)
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


//case class PositionalTransformModelFactory(@Help(text=
//                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
//You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
//                              """)
//                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
//                            @Help(text="Old weights to initialize with. Optional")
//                            oldWeights: File = null,
//                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
//                            dummyFeats: Double = 0.5,
//                            commonWordThreshold: Int = 100,
//                            ngramCountThreshold: Int = 5,
//                            useGrammar: Boolean = true,
//                            numHidden: Int = 100,
//                            posFeaturizer: Optional[WordFeaturizer[String]] = NotProvided,
//                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = NotProvided,
//                            word2vecPath: String = "../cnnkim/data/GoogleNews-vectors-negative300.bin") extends ParserModelFactory[AnnotatedLabel, String] {
//
//  type MyModel = PositionalTransformModel[AnnotatedLabel, AnnotatedLabel, String]
//
//
//
//  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
//                    topology: RuleTopology[AnnotatedLabel],
//                    lexicon: Lexicon[AnnotatedLabel, String],
//                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
//    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
//    println("Here's what the annotation looks like on the first few trees")
//    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))
//
//    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
//    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)
//
//    val xbarGrammar = topology
//    val xbarLexicon = lexicon
//
//    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)
//
//    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)
////    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))
////    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))
////    span += new SingleWordSpanFeaturizer[String](wf)
////    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)
//
//    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
////    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if(r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
//
//    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
//      lGen=labelFeaturizer,
//      rGen=ruleFeaturizer)
//      
//    val word2vec = Word2Vec.loadVectorsForVocabulary(word2vecPath, summedWordCounts.keySet.toSet, true)
//    // Convert Array[Float] values to DenseVector[Double] values
//    val word2vecDenseVect = word2vec.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
////    val word2vecDenseVect = word2vec.map(keyValue => (keyValue._1 -> new DenseVector[Double](keyValue._2.map(_.toDouble))))
//    val surfaceFeaturizer = new PositionalTransformModel.Word2VecSurfaceFeaturizer(word2vecDenseVect)
//    
//
////    val baselineAffineTransform = new AffineTransform(featurizer.index.size, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]())
////    val lowRankAffineTransform = new AffineTransform(featurizer.index.size, rank, new AffineTransform(rank, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]()))
//    
//    // Affine transform of word embeddings, tanh, affine transform to output layer
//    val neuralAffineTransform = new AffineTransform(featurizer.index.size, numHidden, new TanhTransform(new AffineTransform(numHidden, surfaceFeaturizer.vectorSize, new IdentityTransform[DenseVector[Double]]())))
//    
//    new PositionalTransformModel(annotator.latent,
//      constrainer,
//      topology, lexicon,
//      refGrammar,
//      indexedRefinements,
//      featurizer,
//      surfaceFeaturizer,
////      baselineAffineTransform
////      lowRankAffineTransform
//      neuralAffineTransform
//      )
//  }
//
//
//
//}