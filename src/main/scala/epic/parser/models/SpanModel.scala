package epic.parser
package models

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import java.io.File

import breeze.collection.mutable.{OpenAddressHashArray, TriangularArray}
import breeze.config.Help
import breeze.linalg._
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.util._
import breeze.util.SerializableLogging
import epic.constraints.ChartConstraints.Factory
import epic.constraints.{CachedChartConstraintsFactory, ChartConstraints}
import epic.features._
import epic.framework.{Feature, ModelObjective}
import epic.lexicon.Lexicon
import epic.parser.ParserParams.XbarGrammar
import epic.parser.projections.{GrammarRefinements, ParserChartConstraintsFactory}
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import epic.util._

import scala.io.Source

/**
 * A rather more sophisticated discriminative parser. Uses features on
 * the underlying span.
 * @author dlwh
 */
@SerialVersionUID(1L)
class SpanModel[L, L2, W](val featurizer: RefinedFeaturizer[L, W, Feature],
                          val featureIndex: Index[Feature],
                          val annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                          val constrainer: ChartConstraints.Factory[L, W],
                          val topology: RuleTopology[L],
                          val lexicon: Lexicon[L, W],
                          val refinedGrammar: RuleTopology[L2],
                          val refinements: GrammarRefinements[L, L2],
                          initialFeatureVal: (Feature => Option[Double]) = { _ => None }) extends ParserModel[L, W] with Serializable {
  type Inference = LatentParserInference[L, L2, W]

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val dpGrammar = new DotProductGrammar(topology, lexicon, refinedGrammar, refinements, weights, featurizer)
    new LatentParserInference(featurizer, annotator, dpGrammar, constrainer, refinements)
  }

  def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    m.expectedCounts(featurizer, accum, scale)
  }
}

@SerialVersionUID(4749637878577393596L)
class DotProductGrammar[L, L2, W, Feature](val topology: RuleTopology[L],
                                           val lexicon: Lexicon[L, W],
                                           val refinedTopology: RuleTopology[L2],
                                           val refinements: GrammarRefinements[L, L2],
                                           val weights: DenseVector[Double],
                                           val featurizer: RefinedFeaturizer[L, W, Feature]) extends Grammar[L, W] {

  override def withPermissiveLexicon: Grammar[L, W] = {
    new DotProductGrammar(topology, lexicon.morePermissive, refinedTopology, refinements, weights, featurizer)
  }

  def anchor(w: IndexedSeq[W], cons: ChartConstraints[L]):GrammarAnchoring[L, W] = new ProjectionsGrammarAnchoring[L, L2, W] {

    override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
      anchor(w, cons & constraints)
    }

    override def sparsityPattern: ChartConstraints[L] = cons

    def refinements = DotProductGrammar.this.refinements
    def refinedTopology: RuleTopology[L2] = DotProductGrammar.this.refinedTopology

    val topology = DotProductGrammar.this.topology
    val lexicon = DotProductGrammar.this.lexicon

    def words = w

    val fspec = featurizer.anchor(w)
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      dot(fspec.featuresForBinaryRule(begin, split, end, rule, ref))
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      dot(fspec.featuresForUnaryRule(begin, end, rule, ref))
    }

    def scoreSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      dot(fspec.featuresForSpan(begin, end, tag, ref))
    }

    private def dot(features: Array[Int]) = {
      var i = 0
      var score = 0.0
      val wdata = weights.data
      while (i < features.length) {
        score += wdata(features(i))
        i += 1
      }
      score
    }

  }
}

@SerialVersionUID(1L)
case class IndexedSpanFeaturizer[L, L2, W](wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                      spanFeatureIndex: CrossProductIndex[Feature, Feature],
                                      ruleAndSpansFeatureIndex: Index[Feature],
                                      labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                      wordFeaturizer: IndexedWordFeaturizer[W],
                                      surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                      ruleAndSpansFeaturizer: RuleAndSpansFeaturizer[W],
                                      refinements: GrammarRefinements[L, L2],
                                      grammar: RuleTopology[L]) extends RefinedFeaturizer[L, W, Feature] with Serializable {

  def lock = copy(wordFeatureIndex.lock, spanFeatureIndex.lock)

  val index = SegmentedIndex(wordFeatureIndex, spanFeatureIndex, ruleAndSpansFeatureIndex)
  println("Total index size: " + index.size + ", " + wordFeatureIndex.size + " word feats, " + spanFeatureIndex.size +
          " span feats, " + ruleAndSpansFeatureIndex.size + " rule+span feats (all including hash features)")
  private val wordOffset = index.componentOffset(0)
  private val spanOffset = index.componentOffset(1)
  private val ruleAndSpansOffset = index.componentOffset(2)

  def anchor(words: IndexedSeq[W]):Anchoring = new Spec(words)

  case class Spec(words: IndexedSeq[W]) extends super.Anchoring {
    def length = words.length
    private val fspec = labelFeaturizer.anchor(words)
    private val sspec = surfaceFeaturizer.anchor(words)
    private val wspec = wordFeaturizer.anchor(words)
    private val rspec = ruleAndSpansFeaturizer.anchor(words)

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int): Array[Int] = {
      val globalized = refinements.labels.globalize(tag, ref)

      val ind = TriangularArray.index(begin, end)
      var rcache = spanCache(ind)
      if (rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size)
        spanCache(ind) = rcache
      }
      var cache = rcache(globalized)
      if (cache == null)  {
        val spanFeats: Array[Int] = fspec.featuresForSpan(begin, end, tag, ref)
        cache = if (begin + 1 == end) {
          wordFeatureIndex.crossProduct(spanFeats, wspec.featuresForWord(begin), wordOffset)
        } else {
          require(rspec.featuresForSpan(begin, end, tag, ref).isEmpty, "Span features on the extraProductionFeaturizer currently unsupported")
          spanFeatureIndex.crossProduct(spanFeats, getSpanFeatures(begin, end), spanOffset, true)
        }
        rcache(globalized) = cache
      }
      cache
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val globalized = refinements.rules.globalize(rule, ref)
      val ind = TriangularArray.index(begin, end)
      var rcache = unaryCache(ind)
      if (rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        unaryCache(ind) = rcache
      }
      var cache = rcache(globalized)
      if (cache == null)  {
        require(rspec.featuresForUnaryRule(begin, end, rule, ref).isEmpty, "Span features on the extraProductionFeaturizer currently unsupported")
        cache = spanFeatureIndex.crossProduct(fspec.featuresForUnaryRule(begin, end, rule, ref),
          getSpanFeatures(begin, end), spanOffset, true)
        rcache(globalized) = cache
      }
      cache
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Array[Int] = {
      val globalized = refinements.rules.globalize(rule, ref)
      val ind = TriangularArray.index(begin, end)
      var rcache = binaryCache(ind)
      if (rcache eq null) {
        rcache = new Array[OpenAddressHashArray[Array[Int]]](end - begin)
        binaryCache(ind) = rcache
      }
      var scache = rcache(split - begin)
      if (scache eq null) {
        scache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        rcache(split - begin) = scache
      }
      var cache = scache(globalized)
      if (cache == null)  {
        val spanFeatures = getSpanFeatures(begin, end)
        cache = spanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ref),spanFeatures, spanOffset, true)
        // val forSplit = spanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ref), sspec.featuresForSplit(begin, split, end), spanOffset, false)
        val ruleAndSpansFeatures = RuleAndSpansFeaturizer.indexAndOffset(ruleAndSpansFeatureIndex, rspec.featuresForBinaryRule(begin, split, end, rule, ref), ruleAndSpansOffset)
        val forSplit = Arrays.concatenate(spanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ref), sspec.featuresForSplit(begin, split, end), spanOffset, false),
                                          ruleAndSpansFeatures)
        if (forSplit.length > 0)
          cache = Arrays.concatenate(cache, forSplit)
        scache(globalized) = cache
      }

      cache
    }

    private def getSpanFeatures(begin: Int, end: Int):Array[Int] = {
      val ind = TriangularArray.index(begin, end)
      var cache = rawSpanCache(ind)
      if (cache eq null) {
        cache = sspec.featuresForSpan(begin, end)
        rawSpanCache(ind) = cache
      }
      cache
    }

    // caches:
    // (begin,end) -> label ->  Array[Int]
    val spanCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (begin,end) ->  Array[Int]
    val rawSpanCache = TriangularArray.raw[Array[Int]](length + 1, null)
    // (begin,end) -> rule -> Array[Int]
    val unaryCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (begin, end) -> (split - begin) -> Array[Int]
    val binaryCache = TriangularArray.raw[Array[OpenAddressHashArray[Array[Int]]]](length + 1, null)
  }

}

object IndexedSpanFeaturizer {
  def extract[L, L2, W](wordFeaturizer: IndexedWordFeaturizer[W],
                        surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                        featurizer: RefinedFeaturizer[L,W, Feature] ,
                        ruleAndSpansFeaturizer: RuleAndSpansFeaturizer[W],
                        ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[IndexedSeq[L2]],
                        refinements: GrammarRefinements[L, L2],
                        grammar: RuleTopology[L],
                        dummyFeatScale: HashFeature.Scale,
                        filterUnseenFeatures: Boolean,
                        minFeatCount: Int,
                        trees: Traversable[TreeInstance[L, W]]): IndexedSpanFeaturizer[L, L2, W] = {

    def seenSet =  if (filterUnseenFeatures) new ThreadLocalBloomFilter[Long](8 * 1024 * 1024 * 50, 3) else AlwaysSeenSet

    val spanBuilder = new CrossProductIndex.Builder(featurizer.index, surfaceFeaturizer.featureIndex, dummyFeatScale, seenSet = seenSet, minCount = minFeatCount)
    val wordBuilder = new CrossProductIndex.Builder(featurizer.index, wordFeaturizer.featureIndex, dummyFeatScale, seenSet = seenSet, includeLabelOnlyFeatures = false)
    val ruleAndSpansIndex = Index[Feature]

    for(ti <- trees) {
      val spec = featurizer.anchor(ti.words)
      val wspec = wordFeaturizer.anchor(ti.words)
      val sspec = surfaceFeaturizer.anchor(ti.words)
      val rspec = ruleAndSpansFeaturizer.anchor(ti.words)
      ann(ti.tree, ti.words).allChildren.foreach {
        case NullaryTree(as, span) =>
          for(a <- as) {
            val (ai, aref) = refinements.labels.indexAndLocalize(a)
            wordBuilder.add(spec.featuresForSpan(span.begin, span.end, ai, aref), wspec.featuresForWord(span.begin))
            RuleAndSpansFeaturizer.addToIndex(ruleAndSpansIndex, rspec.featuresForSpan(span.begin, span.end, ai, aref))
          }
        case UnaryTree(as, bs, chain, span) =>
          for(a <- as; b <- bs.label) {
            val r = UnaryRule(a, b, chain)
            val (ri, rref) = refinements.rules.indexAndLocalize(r)
            if (rref != -1) {
              spanBuilder.add(spec.featuresForUnaryRule(span.begin, span.end, ri, rref), sspec.featuresForSpan(span.begin, span.end))
              RuleAndSpansFeaturizer.addToIndex(ruleAndSpansIndex, rspec.featuresForUnaryRule(span.begin, span.end, ri, rref))
            }
          }
        case t@BinaryTree(as, bs, cs, span) =>
          for(a <- as; b <- bs.label; c <- cs.label) {
            val (ai, aref) = refinements.labels.indexAndLocalize(a)
            val r = BinaryRule(a, b, c)
            val (ri, rref) = refinements.rules.indexAndLocalize(r)
            if (rref != -1) {
              spanBuilder.add(spec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
                sspec.featuresForSpan(span.begin, span.end))
              spanBuilder.add(spec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
                sspec.featuresForSplit(span.begin, t.splitPoint, span.end))
              spanBuilder.add(spec.featuresForSpan(span.begin, span.end, ai, aref),
                sspec.featuresForSpan(span.begin, span.end))
              RuleAndSpansFeaturizer.addToIndex(ruleAndSpansIndex, rspec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref))
            }
          }
      }

    }
    val ruleAndSpansIndexExtended = new HashExtendingIndex(ruleAndSpansIndex, HashFeature(_), dummyFeatScale, seenSet)
    new IndexedSpanFeaturizer(wordBuilder.result(), spanBuilder.result(), ruleAndSpansIndex, featurizer, wordFeaturizer, surfaceFeaturizer, ruleAndSpansFeaturizer, refinements, grammar)
  }
}

@SerialVersionUID(-155022487059445275L)
case class ExtraParams(useHackyLexicalFeatures:Boolean = false,
                       hackyLexicalFeatureDesc:String = "",
                       useMorph:Boolean = false,
                       useTagSpanShape:Boolean = false,
                       pathsToMorph:String = "")

@SerialVersionUID(5415542513198973305L)
case class SpanModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing. If negative, use absolute value as number of hash features.")
                            dummyFeats: Double = 0.5,
                            minFeatCount: Int = 1,
                            pruneRedundantFeatures: Boolean = false,
                            commonWordThreshold: Int = 100,
                            ngramCountThreshold: Int = 5,
                            useShape: Boolean = true,
                            useRichSpanContext:Boolean = false,
                            numSpanContextWords:Int = 1,
                            useNGrams:Boolean = false,
                            maxNGramOrder:Int = 2,
                            useGrammar: Boolean = true,
                            useChildFeats: Boolean = false,
                            useFullShape: Boolean = false,
                            useSplitShape: Boolean = false,
                            posFeaturizer: Optional[WordFeaturizer[String]] = None,
                            spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = None,
                            extraParams: ExtraParams = ExtraParams()) extends ParserModelFactory[AnnotatedLabel, String] with SerializableLogging {
  
  type MyModel = SpanModel[AnnotatedLabel, AnnotatedLabel, String]

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    constrainer: Factory[AnnotatedLabel, String]): MyModel = {
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
    lazy val spanShapeBetter = new SpanShapeFeaturizerBetter(numSpanContextWords, useRichSpanContext)
    lazy val tagSpanShape = new TagSpanShapeFeaturizer(TagSpanShapeGenerator.makeBaseLexicon(trainTrees))
    lazy val fullShape = new FullWordSpanShapeFeaturizer(summedWordCounts.iterator.filter(_._2 > commonWordThreshold * 10).map(_._1).toSet, numSpanContextWords, useRichSpanContext)

    var wf = posFeaturizer.getOrElse( SpanModelFactory.defaultPOSFeaturizer(annWords))

    if (useMorph)
      wf += mf

    var span: SplitSpanFeaturizer[String] = spanFeaturizer.getOrElse(SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold, useShape = useShape))

    if (useRichSpanContext)
      span += spanShapeBetter

    if (useNGrams)
      span += ngramF

    if (useTagSpanShape)
      span += tagSpanShape

    if (useFullShape)
      span += fullShape


    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words}, deduplicateFeatures = pruneRedundantFeatures)
    val surface = IndexedSplitSpanFeaturizer.fromData(span, annTrees, bloomFilter = false, deduplicateFeatures = pruneRedundantFeatures)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    
//    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (useGrammar) Set(r, r.map(_.baseAnnotatedLabel)).toSeq else if (r.isInstanceOf[UnaryRule[AnnotatedLabel]]) Set(r.parent, r.parent.baseAnnotatedLabel).toSeq else Seq.empty
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = if (useGrammar) {
      if (useChildFeats && r.isInstanceOf[BinaryRule[AnnotatedLabel]]) {
        Set(r,
            r.map(_.baseAnnotatedLabel),
            new LeftChildFeature(r.asInstanceOf[BinaryRule[AnnotatedLabel]].left),
            new RightChildFeature(r.asInstanceOf[BinaryRule[AnnotatedLabel]].right)).toSeq
      } else {
        Set(r, r.map(_.baseAnnotatedLabel)).toSeq
      }
    } else if (r.isInstanceOf[UnaryRule[AnnotatedLabel]]) {
      Set(r.parent, r.parent.baseAnnotatedLabel).toSeq
    } else {
      Seq.empty
    }

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer, filterRedundantFeatures = pruneRedundantFeatures)
    
    // This is a catch-all for other features that must be instantiated over the entire rule
    // and which are not synthesized on-the-fly from cross-products.
    val ruleAndSpansFeaturizer: RuleAndSpansFeaturizer[String] = if (useHackyLexicalFeatures) {
      new HackyLexicalProductionFeaturizer(TagSpanShapeGenerator.makeStandardLexicon(annTrees), xbarGrammar, hackyLexicalFeatureDesc)
    } else {
      new ZeroRuleAndSpansFeaturizer()
    }

    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
      featurizer,
      ruleAndSpansFeaturizer,
      annotator.latent,
      indexedRefinements,
      xbarGrammar,
      if (dummyFeats < 0) HashFeature.Absolute(-dummyFeats.toInt) else HashFeature.Relative(dummyFeats),
      filterUnseenFeatures = false,
      minFeatCount,
      trainTrees)

    println("LAST FEAT:::::" + indexed.spanFeatureIndex.last)

    logger.info(s"Num features: Indexed Features: ${indexed.index.size}")

    val featureCounter = readWeights(oldWeights)

    new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator.latent, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get(_))
  }

}

case class LatentSpanModelFactory(inner: SpanModelFactory,
                                  @Help(text="Path to substates to use for each symbol. Uses numStates for missing states.")
                                  substates: File = null,
                                  @Help(text="Split states that the Berkeley Parser doesn't want to split.")
                                  splitUselessStates: Boolean = false,
                                  @Help(text="Number of states to use. Overridden by substates file")
                                  numStates: Int = 2) extends ParserModelFactory[AnnotatedLabel, String] with SerializableLogging {

  type MyModel = SpanModel[AnnotatedLabel, (AnnotatedLabel, Int), String]

  override def make(train: IndexedSeq[TreeInstance[AnnotatedLabel, String]], topology: RuleTopology[AnnotatedLabel], lexicon: Lexicon[AnnotatedLabel, String], constrainer: Factory[AnnotatedLabel, String]): MyModel = {
    import inner.{logger => _, _}
    import extraParams._
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = train.map(annotator(_))
    logger.info("Here's what the annotation looks like on the first few trees")
    annTrees.slice(0, Math.min(3, annTrees.size)).foreach(tree => logger.info(tree.render(false)))

    val (annWords, annBinaries, annUnaries) = GenerativeParser.extractCounts(annTrees)

    val xbarLexicon = lexicon

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (topology.root -> 1)
    } else if (splitUselessStates) {
      Map(topology.root -> 1)
    } else {
      LatentModelFactory.statesToNotSplit.iterator.map(s => AnnotatedLabel(s) -> 1).toMap  + (topology.root -> 1)
    }

    def splitLabel(x: AnnotatedLabel): Seq[(AnnotatedLabel, Int)] = {
      for (i <- 0 until substateMap.getOrElse(x, numStates)) yield (x, i)
    }

    val splitLabels = topology.labelIndex.map(l => l -> splitLabel(l)).toMap

    def unsplit(x: (AnnotatedLabel, Int)): AnnotatedLabel = x._1

    def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
      // don't allow non-identity rule refinements for identity rewrites
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

    val annTopology: RuleTopology[AnnotatedLabel] = RuleTopology(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(topology, annTopology, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annTopology, splitLabel _, {splitRule(_ :Rule[AnnotatedLabel], splitLabels)}, unsplit _)
    val finalRefinements = firstLevelRefinements compose secondLevel
    logger.info("Label refinements:" + finalRefinements.labels)

    val mf: MorphFeaturizer = if (useMorph) {
      MorphFeaturizer(pathsToMorph.split(","))
    } else {
      null
    }
    val summedWordCounts: Counter[String, Double] = sum(annWords, Axis._0)
    lazy val ngramF = new NGramSpanFeaturizer(summedWordCounts, NGramSpanFeaturizer.countBigrams(annTrees), annTrees.map(_.words), ngramCountThreshold, maxNGramOrder, useNot = false)
    lazy val spanShapeBetter = new SpanShapeFeaturizerBetter(numSpanContextWords, useRichSpanContext)
    lazy val tagSpanShape = new TagSpanShapeFeaturizer(TagSpanShapeGenerator.makeBaseLexicon(train))
    lazy val fullShape = new FullWordSpanShapeFeaturizer(summedWordCounts.iterator.filter(_._2 > commonWordThreshold * 10).map(_._1).toSet, numSpanContextWords, useRichSpanContext)

    val wf = {//WordFeaturizer.goodPOSTagFeaturizer(annWords)
    val dsl = new WordFeaturizer.DSL(annWords)
      import dsl._
      if (useMorph) {
        unigrams(word, 1) + suffixes() + prefixes() + mf
      } else {
        unigrams(word, 1) + suffixes() + prefixes()
      }
    }

    var span: SplitSpanFeaturizer[String] = SpanModelFactory.goodFeaturizer(annWords, commonWordThreshold)

    if (useRichSpanContext)
      span += spanShapeBetter

    if (useNGrams)
      span += ngramF

    if (useFullShape)
      span += fullShape

    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words})
    val surface = IndexedSplitSpanFeaturizer.fromData(span, annTrees)

    def labelFeaturizer(l: (AnnotatedLabel, Int)) = Set[Feature](IndicatorFeature(l), l._1, l._1.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[(AnnotatedLabel, Int)]) = if (useGrammar) Set(r, r.map(_._1)).toSeq else if (r.isInstanceOf[UnaryRule[(AnnotatedLabel, Int)]]) labelFeaturizer(r.parent) else Seq.empty

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, (AnnotatedLabel, Int), String](topology, finalRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    // This is a catch-all for other features that must be instantiated over the entire rule
    // and which are not synthesized on-the-fly from cross-products.
    val ruleAndSpansFeaturizer: RuleAndSpansFeaturizer[String] = if (useHackyLexicalFeatures) {
      new HackyLexicalProductionFeaturizer(TagSpanShapeGenerator.makeStandardLexicon(annTrees), topology, hackyLexicalFeatureDesc)
    } else {
      new ZeroRuleAndSpansFeaturizer()
    }

    def latentAnnotator(t: BinarizedTree[AnnotatedLabel], w: IndexedSeq[String]): BinarizedTree[IndexedSeq[(AnnotatedLabel, Int)]] = {
      annotator(t, w).map(finalRefinements.labels.refinementsOf)
    }

    val indexed = IndexedSpanFeaturizer.extract[AnnotatedLabel, (AnnotatedLabel, Int), String](
      indexedWord,
      surface,
      featurizer,
      ruleAndSpansFeaturizer,
      latentAnnotator,
      finalRefinements,
      topology,
      if (dummyFeats < 0) HashFeature.Absolute(-dummyFeats.toInt) else HashFeature.Relative(dummyFeats),
      // filterUnseenFeatures = true,
      filterUnseenFeatures = false,
      1,
      train
    )

    val featureCounter = this.readWeights(oldWeights)

    val refGrammar = RuleTopology(finalRefinements.labels.refinementsOf(topology.root)(0),
      finalRefinements.labels.fineIndex,
      finalRefinements.rules.fineIndex)

    new SpanModel[AnnotatedLabel, (AnnotatedLabel, Int), String](indexed, indexed.index, latentAnnotator,
      constrainer, topology, xbarLexicon, refGrammar, finalRefinements, featureCounter.get(_))
  }

}

object SpanModelFactory {
  def goodFeaturizer[L](wordCounts: Counter2[AnnotatedLabel, String, Double],
                        commonWordThreshold: Int = 100,
                        useShape: Boolean = true,
                        useLfsuf: Boolean = true,
                        useBrown: Boolean = false,
                        useMostSparseIndicators: Boolean = false) = {
    val dsl = new WordFeaturizer.DSL(wordCounts, commonWordThreshold) with SurfaceFeaturizer.DSL with SplitSpanFeaturizer.DSL
    import dsl._

    // class(split + 1)
    var baseCat: WordFeaturizer[String] = new ZeroFeaturizer[String]
    if (useLfsuf) {
      baseCat += lfsuf
    }
    if (useBrown) {
      baseCat += new BrownClusterFeaturizer(Array(4, 10))
    }
    
    val leftOfSplit: SplitSpanFeaturizer[String] = baseCat(-1)apply split
    
    var featurizer: SplitSpanFeaturizer[String] = zeroSplit[String]
    featurizer += baseCat(begin)
    featurizer += baseCat(end-1)
    featurizer += baseCat(begin-1)
    featurizer += baseCat(end)
    featurizer += leftOfSplit
    featurizer += baseCat(split)
    featurizer += length
    if (useMostSparseIndicators) {
      featurizer += baseCat(begin-2)
      featurizer += baseCat(end-2)
      featurizer += baseCat(begin+1)
      featurizer += baseCat(end+1)
      featurizer += baseCat(-2)apply split
      featurizer += baseCat(1)apply split
    }

    featurizer += distance[String](begin, split)
    featurizer += distance[String](split, end)
    if (useShape)
      featurizer += spanShape
    featurizer
  }

  def defaultPOSFeaturizer(annWords: Counter2[AnnotatedLabel, String, Double], useBrown: Boolean = false): WordFeaturizer[String] = {
    {
      val dsl = new WordFeaturizer.DSL(annWords)
      import dsl._
      if (useBrown) {
        val brown = new BrownClusterFeaturizer(Array(4, 10))
        unigrams(brown, 1) + unigrams(word, 1) + suffixes() + prefixes()
      } else {
        unigrams(word, 1) + suffixes() + prefixes()
      }
    }
  }

  def buildSimple(trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                  posFeaturizer: Optional[WordFeaturizer[String]] = None,
                  spanFeaturizer: Optional[SplitSpanFeaturizer[String]] = None,
                  opt: OptParams = OptParams())(implicit cache: CacheBroker) = {
    val (topo, lexicon) = XbarGrammar().xbarGrammar(trees)
    val initialParser =  GenerativeParser.annotatedParser(topo, lexicon, annotator, trees)

    val constraints = {

      val maxMarginalized = initialParser.copy(marginalFactory=initialParser.marginalFactory match {
        case StandardChartFactory(ref, mm) => StandardChartFactory(ref, maxMarginal = true)
        case x => x
      })

      val uncached = new ParserChartConstraintsFactory[AnnotatedLabel, String](maxMarginalized, {(_:AnnotatedLabel).isIntermediate})
      new CachedChartConstraintsFactory[AnnotatedLabel, String](uncached)
    }

    val mf = new SpanModelFactory(annotator = annotator, posFeaturizer = posFeaturizer, spanFeaturizer = spanFeaturizer).make(trees, topo, lexicon, constraints)

    val mobj = new ModelObjective(mf, trees)

    val weights = breeze.optimize.minimize(mobj, mobj.initialWeightVector(false))

    mf.extractParser(weights)
  }

}

