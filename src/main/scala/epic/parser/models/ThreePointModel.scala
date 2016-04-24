package epic.parser.models

import java.io.File

import breeze.collection.mutable.TriangularArray
import breeze.config.Help
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util.Index
import epic.constraints.ChartConstraints
import epic.constraints.ChartConstraints.Factory
import epic.dense.{IdentityTransform, AffineTransform, Transform}
import epic.features.SurfaceFeaturizer.SingleWordSpanFeaturizer
import epic.features._
import epic.framework.{Inference, Feature}
import epic.lexicon.Lexicon
import epic.parser._
import epic.parser.projections.GrammarRefinements
import epic.preprocess.MLSentenceSegmenter.BiasFeature
import epic.trees.annotations.TreeAnnotator
import epic.trees._
import epic.util.{Optional, LRUCache}

/**
 * TODO
 *
 * @author dlwh
 **/
class ThreePointModel[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                 val constrainer: ChartConstraints.Factory[L, W],
                                 val topology: RuleTopology[L],
                                 val lexicon: Lexicon[L, W],
                                 refinedTopology: RuleTopology[L2],
                                 refinements: GrammarRefinements[L, L2],
                                 labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 wordFeaturizer: IndexedWordFeaturizer[W],
                                 rank: Int) extends ParserModel[L, W] {

  override type Inference = ThreePointModel.ThreePointInference[L, L2, W]

  override def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    inf.grammar.extractEcounts(m, accum.counts, scale)
    accum.loss += scale * m.logPartition
  }

  override val featureIndex = new SegmentedIndex(new AffineTransform.Index(rank, labelFeaturizer.index.size, false) +: IndexedSeq.fill(3)(new AffineTransform.Index(rank, wordFeaturizer.featureIndex.size, false)))

  override def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    val grammar = new ThreePointModel.Grammar[L, L2, W](topology, lexicon, refinedTopology, refinements, labelFeaturizer,
      wordFeaturizer,
      featureIndex,
    weights
    )
    new Inference(annotator, constrainer,
      grammar, refinements)
  }

  override def initialValueForFeature(f: Feature): Double = f.hashCode().toDouble / 1000 % 2

}

object ThreePointModel {

  object Point extends Enumeration {
    val First, Split, Last = Value // TODO: probably add Lex
  }

  case class ThreePointInference[L, L2, W](annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                                         constrainer: ChartConstraints.Factory[L, W],
                                         grammar: Grammar[L, L2, W],
                                         refinements: GrammarRefinements[L, L2]) extends ParserInference[L, W] {

    override def goldMarginal(scorer: Scorer, ti: TreeInstance[L, W], aug: UnrefinedGrammarAnchoring[L, W]): Marginal = {
      import ti._

      val annotated = annotator(tree, words).map(_.map(refinements.labels.localize))

      val product = grammar.anchor(words, constrainer.constraints(ti.words))
      LatentTreeMarginal(product, annotated)
    }

  }

  @SerialVersionUID(1L)
  case class Grammar[L, L2, W](topology: RuleTopology[L],
                          lexicon: Lexicon[L, W],
                          refinedTopology: RuleTopology[L2],
                          refinements: GrammarRefinements[L, L2],
                          labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                          wordFeaturizer: IndexedWordFeaturizer[W],
                          featureIndex: SegmentedIndex[Feature, AffineTransform.Index],
                          weights: DenseVector[Double]) extends epic.parser.Grammar[L, W] with Serializable {
    val IndexedSeq(ruleMatrix, wordMatrices@ _*) = reshapeWeightMatrices(weights)
    assert(wordMatrices.length == 3)

    private def reshapeWeightMatrices(weights: DenseVector[Double]): IndexedSeq[DenseMatrix[Double]] = {
      val segments = featureIndex.shardWeights(weights)
      (featureIndex.indices zip segments).map { case (index, segment) => index.makeMatrix(segment)}

    }

    override def withPermissiveLexicon = {
      new Grammar(topology, lexicon.morePermissive, refinedTopology, refinements, labelFeaturizer, wordFeaturizer, featureIndex, weights)
    }

    def anchor(w: IndexedSeq[W], cons: ChartConstraints[L]) = new Anchoring(w, cons)

    // TODO: cache these if necessary for speed
    class Anchoring(w: IndexedSeq[W], cons: ChartConstraints[L]) extends ProjectionsGrammarAnchoring[L, L2, W] {

      override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
        anchor(w, cons & constraints)
      }

      override def sparsityPattern: ChartConstraints[L] = cons

      def refinements = Grammar.this.refinements
      def refinedTopology: RuleTopology[L2] = Grammar.this.refinedTopology

      val topology = Grammar.this.topology
      val lexicon = Grammar.this.lexicon

      def words = w

      val sspec = wordFeaturizer.anchor(w)
      val lspec = labelFeaturizer.anchor(w)

      val wordActivations: Array[Array[DenseVector[Double]]] = Array.tabulate(words.length, wordMatrices.length) { (i, c) =>
        wordMatrices(c) * new FeatureVector(sspec.featuresForWord(i))
      }

      def actForPos(w: Int, c: Point.Value) = wordActivations(w)(c.id)

      // doesn't include split point, which we'll do online
      val precachedSpanActivations = TriangularArray.tabulate(words.length + 1) { (i, j) =>
        if (sparsityPattern.isAllowedSpan(i, j) && i != j) {
          val result = DenseVector.ones[Double](wordActivations.head.head.size)

          result :*= actForPos(i, Point.First)
          result :*= actForPos(j - 1, Point.Last)
          // println(result)

          result
        } else {
          null
        }
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
        val surfaceAct = precachedSpanActivations(begin, end)
        if (surfaceAct == null) {
          Double.NegativeInfinity
        } else {
          val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
          (ruleMatrix * new FeatureVector(rfeats)) dot (surfaceAct :* actForPos(split, Point.Split))
        }
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
        val surfaceAct = precachedSpanActivations(begin, end)
        if (surfaceAct == null) {
          Double.NegativeInfinity
        } else {
          val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
          (ruleMatrix * new FeatureVector(rfeats)) dot surfaceAct
        }
      }

      def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
        val surfaceAct = precachedSpanActivations(begin, end)
        if (surfaceAct == null) {
          Double.NegativeInfinity
        } else {
          val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
          (ruleMatrix * new FeatureVector(rfeats)) dot surfaceAct
        }
      }

    }

    def extractEcounts(m: ParseMarginal[L, W], deriv: DenseVector[Double], scale: Double): Unit = {
      val anchoring = anchor(m.words, m.anchoring.sparsityPattern)

      val IndexedSeq(dRuleAct, dWeights @ _*) = reshapeWeightMatrices(deriv)

      val w = m.words
      val length = w.length

      /*
      // cache: we remember the (begin/end) pair we saw with each
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
      */

      m visit new AnchoredVisitor[L] {
        import anchoring._

        override def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val rfeats = lspec.featuresForUnaryRule(begin, end, rule, ref)
          val surfaceAct = precachedSpanActivations(begin, end)

          val ruleAct = ruleMatrix * new FeatureVector(rfeats)
          for(f <- rfeats) {
            axpy(score * scale, surfaceAct, dRuleAct(::, f))
          }

          val beginAct = ruleAct :* actForPos(end - 1, Point.Last)
          for(f <- sspec.featuresForWord(begin)) {
            axpy(score * scale, beginAct, dWeights(Point.First.id)(::, f))
          }

          val endAct = ruleAct :* actForPos(begin, Point.First)
          for(f <- sspec.featuresForWord(end - 1)) {
            axpy(score * scale, endAct, dWeights(Point.Last.id)(::, f))
          }

        }

        override def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double): Unit = {
          val rfeats = lspec.featuresForSpan(begin, end, tag, ref)
          val surfaceAct = precachedSpanActivations(begin, end)

          val ruleAct = ruleMatrix * new FeatureVector(rfeats)
          for(f <- rfeats) {
            axpy(score * scale, surfaceAct, dRuleAct(::, f))
          }

          val actWithoutBegin = ruleAct :* actForPos(end - 1, Point.Last)
          for(f <- sspec.featuresForWord(begin)) {
            axpy(score * scale, actWithoutBegin, dWeights(Point.First.id)(::, f))
          }

          val actWithoutEnd = ruleAct :* actForPos(begin, Point.First)
          for(f <- sspec.featuresForWord(end - 1)) {
            axpy(score * scale, actWithoutEnd, dWeights(Point.Last.id)(::, f))
          }
        }

        override def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double): Unit = {
          val rfeats = lspec.featuresForBinaryRule(begin, split, end, rule, ref)
          val surfaceAct = precachedSpanActivations(begin, end)

          val ruleAct = ruleMatrix * new FeatureVector(rfeats)
          for(f <- rfeats) {
            axpy(score * scale, surfaceAct, dRuleAct(::, f))
          }

          val beginAct = ruleAct :* actForPos(end - 1, Point.Last) :* actForPos(split, Point.Split)
          for(f <- sspec.featuresForWord(begin)) {
            axpy(score * scale, beginAct, dWeights(Point.First.id)(::, f))
          }

          val endAct = ruleAct :* actForPos(begin, Point.First) :* actForPos(split, Point.Split)
          for(f <- sspec.featuresForWord(end - 1)) {
            axpy(score * scale, endAct, dWeights(Point.Last.id)(::, f))
          }

          val splitAct = ruleAct :* surfaceAct
          for(f <- sspec.featuresForWord(split)) {
            axpy(score * scale, splitAct, dWeights(Point.Split.id)(::, f))
          }
        }
      }

    }
  }

}

case class ThreePointModelFactory(@Help(text=
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

  type MyModel = ThreePointModel[AnnotatedLabel, AnnotatedLabel, String]

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

    var wf: WordFeaturizer[String] = {
      val dsl = new WordFeaturizer.DSL(annWords)
      import dsl._
      unigrams(lfsuf, 1) + word + new WordFeaturizer[String] {
        override def anchor(w: IndexedSeq[String]): WordFeatureAnchoring[String] = new WordFeatureAnchoring[String] {
          override def featuresForWord(pos: Int): Array[Feature] = Array(BiasFeature)

          override def words: IndexedSeq[String] =  w
        }
      }
    }

    if (useMorph)
      wf += MorphFeaturizer(pathsToMorph.split(","))

    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words})

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if (r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    new ThreePointModel(annotator.latent,
      constrainer,
      topology, lexicon,
      refGrammar, indexedRefinements,
      featurizer, indexedWord,
      rank)

  }

}
