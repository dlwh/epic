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

import breeze.collection.mutable.{TriangularArray, OpenAddressHashArray}
import breeze.linalg._
import epic.trees._
import annotations.TreeAnnotator
import collection.mutable.ArrayBuffer
import java.io.File
import breeze.util._
import epic.framework.Feature
import epic.parser.projections.GrammarRefinements
import breeze.config.Help
import epic.lexicon.Lexicon
import epic.features._
import epic.features.HashFeature
import epic.util.{Arrays, CacheBroker}
import epic.trees.annotations.FilterAnnotations

/**
 * A rather more sophisticated discriminative parser. Uses features on
 * the underlying span.
 * @author dlwh
 */
@SerialVersionUID(1L)
class SpanModel[L, L2, W](val featurizer: RefinedFeaturizer[L, W, Feature],
                          val featureIndex: Index[Feature],
                          val annotator: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                          val baseFactory: CoreGrammar[L, W],
                          val baseGrammar: BaseGrammar[L],
                          val lexicon: Lexicon[L, W],
                          val refinedGrammar: BaseGrammar[L2],
                          val refinements: GrammarRefinements[L, L2],
                          initialFeatureVal: (Feature => Option[Double]) = { _ => None }) extends ParserModel[L, W] with Serializable {
  type Inference = AnnotatedParserInference[L, W]

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val factory = new DotProductGrammar(baseGrammar, lexicon, refinedGrammar, refinements, weights, featurizer)
    def reannotate(bt: BinarizedTree[L], words: IndexedSeq[W]) = {
      val annotated = annotator(bt, words)

      val localized = annotated.map { l =>
        refinements.labels.project(l) -> refinements.labels.localize(l)
      }

      localized
    }
    new AnnotatedParserInference(featurizer, reannotate, factory, baseFactory)
  }

  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
    m.expectedCounts(featurizer, accum, scale)
  }
}


class DotProductGrammar[L, L2, W, Feature](val grammar: BaseGrammar[L],
                                           val lexicon: Lexicon[L, W],
                                           val refinedGrammar: BaseGrammar[L2],
                                           val refinements: GrammarRefinements[L, L2],
                                           val weights: DenseVector[Double],
                                           val featurizer: RefinedFeaturizer[L, W, Feature]) extends RefinedGrammar[L, W] {

  def anchor(w: IndexedSeq[W]):RefinedAnchoring[L, W] = new ProjectionsRefinedAnchoring[L, L2, W] {

    def refinements = DotProductGrammar.this.refinements
    def refinedGrammar: BaseGrammar[L2] = DotProductGrammar.this.refinedGrammar

    val grammar = DotProductGrammar.this.grammar
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
      while(i < features.length) {
        score += wdata(features(i))
        i += 1
      }
      score
    }


  }
}

class IndexedSpanFeaturizer[L, L2, W](wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                      spanFeatureIndex: CrossProductIndex[Feature, Feature],
                                      labelFeaturizer: RefinedFeaturizer[L, W, Feature],
                                      wordFeaturizer: IndexedWordFeaturizer[W],
                                      surfaceFeaturizer: IndexedSplitSpanFeaturizer[W],
                                      refinements: GrammarRefinements[L, L2],
                                      grammar: BaseGrammar[L]) extends RefinedFeaturizer[L, W, Feature] with Serializable {


  val index = SegmentedIndex(wordFeatureIndex, spanFeatureIndex)
  private val wordOffset = index.componentOffset(0)
  private val spanOffset = index.componentOffset(1)

  def anchor(words: IndexedSeq[W]):Anchoring = new Spec(words)

  case class Spec(words: IndexedSeq[W]) extends super.Anchoring {
    def length = words.length
    private val fspec = labelFeaturizer.anchor(words)
    private val sspec = surfaceFeaturizer.anchor(words)
    private val wspec = wordFeaturizer.anchor(words)

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int): Array[Int] = {
      val globalized = refinements.labels.globalize(tag, ref)

      val ind = TriangularArray.index(begin, end)
      var rcache = spanCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size)
        spanCache(ind) = rcache
      }
      var cache = rcache(globalized)
      if(cache == null)  {
        val spanFeats: Array[Int] = fspec.featuresForSpan(begin, end, tag, ref)
        cache = if(begin + 1 == end) {
          wordFeatureIndex.crossProduct(spanFeats, wspec.featuresForWord(begin), wordOffset)
        } else {
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
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        unaryCache(ind) = rcache
      }
      var cache = rcache(globalized)
      if(cache == null)  {
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
      if(rcache eq null) {
        rcache = new Array[OpenAddressHashArray[Array[Int]]](end - begin)
        binaryCache(ind) = rcache
      }
      var scache = rcache(split - begin)
      if(scache eq null) {
        scache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        rcache(split - begin) = scache
      }
      var cache = scache(globalized)
      if(cache == null)  {
        val spanFeatures = getSpanFeatures(begin, end)
        cache = spanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ref),spanFeatures, spanOffset, true)
        val forSplit = spanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ref), sspec.featuresForSplit(begin, split, end), spanOffset, false)
        if(forSplit.length > 0)
          cache = Arrays.concatenate(cache, forSplit)
        scache(globalized) = cache
      }

      cache
    }

    private def getSpanFeatures(begin: Int, end: Int):Array[Int] = {
      val ind = TriangularArray.index(begin, end)
      var cache = rawSpanCache(ind)
      if(cache eq null) {
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
                        ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                        refinements: GrammarRefinements[L, L2],
                        grammar: BaseGrammar[L],
                        dummyFeatScale: HashFeature.Scale,
                        trees: Traversable[TreeInstance[L, W]]): IndexedSpanFeaturizer[L, L2, W] = {

    val spanBuilder = new CrossProductIndex.Builder(featurizer.index, surfaceFeaturizer.featureIndex, dummyFeatScale)
    val wordBuilder = new CrossProductIndex.Builder(featurizer.index, wordFeaturizer.featureIndex, dummyFeatScale, includeLabelOnlyFeatures = false)

    for(ti <- trees) {
      val spec = featurizer.anchor(ti.words)
      val wspec = wordFeaturizer.anchor(ti.words)
      val sspec = surfaceFeaturizer.anchor(ti.words)
      ann(ti.tree, ti.words).allChildren.foreach {
        case NullaryTree(a, span) =>
          val (ai, aref) = refinements.labels.indexAndLocalize(a)
          wordBuilder.add(spec.featuresForSpan(span.begin, span.end, ai, aref), wspec.featuresForWord(span.begin))
        case UnaryTree(a, b, chain, span) =>
          val r = UnaryRule(a, b.label, chain)
          val (ri, rref) = refinements.rules.indexAndLocalize(r)
          spanBuilder.add(spec.featuresForUnaryRule(span.begin, span.end, ri, rref), sspec.featuresForSpan(span.begin, span.end))
        case t@BinaryTree(a, b, c, span) =>
          val (ai, aref) = refinements.labels.indexAndLocalize(a)
          val r = BinaryRule(a, b.label, c.label)
          val (ri, rref) = refinements.rules.indexAndLocalize(r)
          spanBuilder.add(spec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
            sspec.featuresForSpan(span.begin, span.end))
          spanBuilder.add(spec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
            sspec.featuresForSplit(span.begin, t.splitPoint, span.end))
          spanBuilder.add(spec.featuresForSpan(span.begin, span.end, ai, aref),
            sspec.featuresForSpan(span.begin, span.end))
      }

    }

    new IndexedSpanFeaturizer(wordBuilder.result, spanBuilder.result(), featurizer, wordFeaturizer, surfaceFeaturizer, refinements, grammar)
  }
}

case class SpanModelFactory(@Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses just parent annotation.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = GenerativeParser.defaultAnnotator(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                            dummyFeats: Double = 0.5,
                            commonWordThreshold: Int = 100,
                            useShape: Boolean = true,
                            useSpanLength: Boolean = true,
                            useBinaryLengths: Boolean = true,
                            useFirstLast: Boolean = true,
                            useSplits: Boolean = true,
                            useBeforeAfter:Boolean = true) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = SpanModel[AnnotatedLabel, AnnotatedLabel, String]



  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String]) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val xbarGrammar = constrainer.grammar
    val xbarLexicon = constrainer.lexicon
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val wf = {//WordFeaturizer.goodPOSTagFeaturizer(annWords)
    val dsl = new WordFeaturizer.DSL(annWords)
      import dsl._

      (
        unigrams(word, 1)
          + suffixes()
          + prefixes()
        )
    }
    val span:SplitSpanFeaturizer[String] = {
      val dsl = new WordFeaturizer.DSL(annWords, commonWordThreshold) with SurfaceFeaturizer.DSL with SplitSpanFeaturizer.DSL
      import dsl._

      // class(split + 1)
      val baseCat = (lfsuf)

      val leftOfSplit =  ((baseCat)(-1)apply (split))

      var featurizer: SplitSpanFeaturizer[String] = zeroSplit[String]
      if ( useFirstLast) {
        featurizer += baseCat(begin)
        featurizer += baseCat(end-1)
      }
      if (useBeforeAfter) {
        featurizer += baseCat(begin-1)
        featurizer += baseCat(end)
      }

      if(useSplits) {
        featurizer += leftOfSplit
        featurizer += baseCat(split)
      }

      if(useSpanLength) {
        featurizer += length
      }

      if(useShape) {
        featurizer += spanShape
      }

      if(useBinaryLengths) {
        featurizer += distance[String](begin, split)
        featurizer += distance[String](split, end)
      }

      featurizer
    }
    val indexedWord = IndexedWordFeaturizer.fromData(wf, annTrees.map{_.words})
    val surface = IndexedSplitSpanFeaturizer.fromData(span, annTrees)

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = Set(r, r.map(_.baseAnnotatedLabel)).toSeq

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](indexedWord,
      surface,
      featurizer,
      annotator,
      indexedRefinements,
      xbarGrammar,
      HashFeature.Relative(dummyFeats),
      trainTrees)

    val featureCounter = readWeights(oldWeights)

    new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get(_))
  }

}

