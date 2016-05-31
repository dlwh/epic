package epic.parser
package models

import java.io.File

import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.dense.{IdentityTransform, AffineTransform, Transform}
import epic.features.SurfaceFeaturizer.SingleWordSpanFeaturizer
import epic.features._
import epic.framework.Feature
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import epic.util.{LRUCache, Optional}

/**
 * TODO
 *
 * @author dlwh
 **/
class TransformModel[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                               val constrainer: ChartConstraints.Factory[L, W],
                               val topology: RuleTopology[L],
                               val lexicon: Lexicon[L, W],
                               refinedTopology: RuleTopology[L2],
                               refinements: GrammarRefinements[L, L2],
                               labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                               surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                               val transform: Transform[FeatureVector, Vector[Double]]) extends ParserModel[L, W] {
  override type Inference = TransformModel.Inference[L, L2, W, transform.type]

  override def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    inf.grammar.extractEcounts(m, accum.counts, scale)
    accum.loss += scale * m.logPartition
  }

  /**
   * Models have features, and this defines the mapping from indices in the weight vector to features.
   * @return
   */
  override def featureIndex: Index[Feature] = transform.index

  override def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val layer = transform.extractLayer(weights, true)

    val grammar = new TransformModel.TransformGrammar[L, L2, W, transform.type](topology, lexicon, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, layer)
    new Inference(annotator, constrainer, grammar, refinements)
  }

  override def initialValueForFeature(f: Feature): Double = 0.0
}

object TransformModel {

  case class Inference[L, L2, W, T <: Transform[FeatureVector, Vector[Double]]](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                                                                constrainer: ChartConstraints.Factory[L, W],
                                                                                grammar: TransformGrammar[L, L2, W, T],
                                                                                refinements: GrammarRefinements[L, L2]) extends ParserInference[L, W]  {
    override def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {

      import ti._

      val annotated = annotator(tree, words).map(_.map(refinements.labels.localize))

      val product = grammar.anchor(words, constrainer.constraints(ti.words))
      LatentTreeMarginal(product, annotated)
    }
  }

  @SerialVersionUID(4749637878577393596L)
  class TransformGrammar[L, L2, W, T <: Transform[FeatureVector, Vector[Double]]](val topology: RuleTopology[L],
                                   val lexicon: Lexicon[L, W],
                                   val refinedTopology: RuleTopology[L2],
                                   val refinements: GrammarRefinements[L, L2],
                                   labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                   surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                   layer: T#Layer) extends Grammar[L, W] with Serializable {


    override def withPermissiveLexicon: Grammar[L, W] = {
      new TransformGrammar(topology, lexicon.morePermissive, refinedTopology, refinements, labelFeaturizer, surfaceFeaturizer, layer)
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
      val UNUSED = (-1, -1)
      val states = Array.fill(w.length + 2)(UNUSED) // 1 for each split,  length for unaries, length +1 for spans
      val ruleCountsPerState = Array.fill(w.length + 2)(SparseVector.zeros[Double](labelFeaturizer.index.size))

      def checkFlush(begin: Int, split: Int, end: Int) {
        val state: (Int, Int) = (begin, end)
        val oldState: (Int, Int) = states(split)
        if (oldState != state) {
          if (oldState != UNUSED) {
            val ffeats = if (split >= length) sspec.featuresForSpan(oldState._1, oldState._2) else sspec.featuresForSplit(oldState._1, split, oldState._2)
            layer.tallyDerivative(deriv, ruleCountsPerState(split) *= scale, new FeatureVector(ffeats))
            ruleCountsPerState(split) := 0.0
          }
          states(split) = state
        }
      }

      m visit new AnchoredVisitor[L] {

        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
                    checkFlush(begin, length, end)
                    axpy(score, new FeatureVector(lspec.featuresForUnaryRule(begin, end, rule, ref)), ruleCountsPerState(length))
        // val ffeats = sspec.featuresForSpan(begin, end)
        // layer.tallyDerivative(deriv, SparseVector(labelFeaturizer.index.size)(lspec.featuresForUnaryRule(begin, end, rule, ref).map(_ -> (scale * score)):_*), new FeatureVector(ffeats))
        }

        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
          checkFlush(begin, length + 1, end)
          axpy(score, new FeatureVector(lspec.featuresForSpan(begin, end, tag, ref)), ruleCountsPerState(length + 1))
          // val ffeats = sspec.featuresForSpan(begin, end)
          // layer.tallyDerivative(deriv, SparseVector(labelFeaturizer.index.size)(lspec.featuresForSpan(begin, end, tag, ref).map(_ -> (scale * score)):_*), new FeatureVector(ffeats))
        }

        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          // val ffeats = sspec.featuresForSplit(begin, split, end)
          // layer.tallyDerivative(deriv, SparseVector(labelFeaturizer.index.size)(lspec.featuresForBinaryRule(begin, split, end, rule, ref).map(_ -> (scale * score)):_*), new FeatureVector(ffeats))
          checkFlush(begin, split, end)
          axpy(score, new FeatureVector(lspec.featuresForBinaryRule(begin, split, end, rule, ref)), ruleCountsPerState(split))
        }
      }

      states.indices.foreach { i =>
        checkFlush(-1, i, -1) // force a flush
      }
    }

    def anchor(w: IndexedSeq[W], cons: ChartConstraints[L]):GrammarAnchoring[L, W] = new ProjectionsGrammarAnchoring[L, L2, W] {

      override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        anchor(w, cons & constraints)
      }

      override def sparsityPattern: ChartConstraints[L] = cons

      def refinements = TransformGrammar.this.refinements
      def refinedTopology: RuleTopology[L2] = TransformGrammar.this.refinedTopology

      val topology = TransformGrammar.this.topology
      val lexicon = TransformGrammar.this.lexicon

      def words = w

      val cache = new LRUCache[Long, Vector[Double]](2 * length)
      val sspec = surfaceFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)

      private def tetra(begin: Int, split: Int, end: Int) = {
        (end.toLong * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
        // (begin, split, end)
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        val fs = cache.getOrElseUpdate(tetra(begin, split, end), {
        val sfeats = sspec.featuresForSplit(begin, split, end)
          layer.activations(new FeatureVector(sfeats))
        })
        // if (fs !=  layer.activations(new FeatureVector( sspec.featuresForSplit(begin, split, end)))) {
        //  println("!!!!")
        // }
        val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
        new FeatureVector(rfeats) dot fs
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        val fs = cache.getOrElseUpdate(tetra(begin, end, length + 1), {
        val sfeats = sspec.featuresForSpan(begin, end)
          layer.activations(new FeatureVector(sfeats))
        })
        val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
        new FeatureVector(rfeats) dot fs
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
        val fs = cache.getOrElseUpdate(tetra(begin, end, length + 1), {
        val sfeats = sspec.featuresForSpan(begin, end)
          layer.activations(new FeatureVector(sfeats))
        })
        val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
        new FeatureVector(rfeats) dot fs
      }

    }
  }

}

case class TransformModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
                            dummyFeats: Double = 0.5,
                            commonWordThreshold: Int = 100,
                            ngramCountThreshold: Int = 5,
                            useNGrams:Boolean = false,
                            maxNGramOrder:Int = 2,
                            useGrammar: Boolean = true,
                            rank: Int = 1,
                            posFeaturizer: Optional[WordFeaturizer[String]] = None,
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = None,
                            extraParams: ExtraParams = ExtraParams()) extends ParserModelFactory[AnnotatedLabel, String] {

  type MyModel = TransformModel[AnnotatedLabel, AnnotatedLabel, String]

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: ChartConstraints.Factory[AnnotatedLabel, String]): MyModel = {
    import extraParams._
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    println("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => println(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = topology
    val xbarLexicon = lexicon

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    lazy val mf: MorphFeaturizer =  MorphFeaturizer(pathsToMorph.split(","))
    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    lazy val ngramF = new NGramSpanFeaturizer(summedWordCounts, NGramSpanFeaturizer.countBigrams(annTrees), annTrees.map(_.words), ngramCountThreshold, maxNGramOrder, useNot = false)
    lazy val tagSpanShape = new TagSpanShapeFeaturizer(TagSpanShapeGenerator.makeBaseLexicon(trainTrees))
    // lazy val fullShape = new FullWordSpanShapeFeaturizer(summedWordCounts.iterator.filter(_._2 > commonWordThreshold * 10).map(_._1).toSet, numSpanContextWords, useRichSpanContext)

    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))

    if (useMorph)
      wf += mf

    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = false))

    if (useNGrams)
      span += ngramF

    span += new SingleWordSpanFeaturizer[String](wf)

    val indexedSurface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if (r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    new TransformModel(
      annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar, indexedRefinements,
      featurizer, indexedSurface,
      new AffineTransform(featurizer.index.size, rank, new AffineTransform(rank, indexedSurface.featureIndex.size, new IdentityTransform[FeatureVector]()))
    )
  }

}