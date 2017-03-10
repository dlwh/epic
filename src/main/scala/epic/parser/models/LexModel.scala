package epic
package parser
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
import breeze.util._
import breeze.util.SerializableLogging
import epic.constraints.ChartConstraints
import epic.constraints.ChartConstraints.Factory
import epic.features.SplitSpanFeaturizer.ZeroSplitSpanFeaturizer
import epic.features._
import epic.framework._
import epic.lexicon.Lexicon
import epic.parser.projections.GrammarRefinements
import epic.trees._
import epic.trees.annotations.{TreeAnnotator, Xbarize}
import epic.util._

import scala.collection.mutable.ArrayBuffer

class LexModel[L, L2, W](bundle: LexGrammarBundle[L, L2, W],
                         reannotate: (BinarizedTree[L], IndexedSeq[W])=>BinarizedTree[L2],
                         indexed: IndexedLexFeaturizer[L, L2, W],
                         val constrainer: ChartConstraints.Factory[L, W],
                         initFeatureValue: Feature=>Option[Double]) extends ParserModel[L, W] with Serializable with ParserExtractable[L, W] {


  def accumulateCounts(inf: Inference, s: Scorer, d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    m.expectedCounts(indexed, accum, scale)
  }

  def topology: RuleTopology[L] = bundle.topology
  def lexicon = bundle.baseLexicon

  val featureIndex = indexed.index

  def initialValueForFeature(f: Feature) = initFeatureValue(f).getOrElse(0)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram = bundle.makeGrammar(indexed, weights)
    def ann(tree: BinarizedTree[L], words: IndexedSeq[W]):BinarizedTree[(L, Int)] = {
      val reannotated = reannotate(tree, words)
      val headed = bundle.headFinder.projected(indexed.refinements.labels.project(_:L2)).annotateHeadIndices(reannotated)
      headed.map { case (l2, head) =>
        indexed.refinements.labels.project(l2) -> indexed.joinTagRef(head, indexed.refinements.labels.localize(l2)._2, words.length)
      }

    }
    new AnnotatedParserInference(indexed, ann, gram, constrainer)
  }

  type Inference = AnnotatedParserInference[L, W]
}




/**
 * Indexes and caches features for more efficient access.
 * Two kinds of features: features we observed in a gold tree (true Features), and features
 * we haven't. The latter are binned by hashcode into the higher-end of the feature index
 * @param ruleFeaturizer base featurizer
 * @tparam L Label
 * @tparam W Word
 */
class IndexedLexFeaturizer[L, L2, W](grammar: RuleTopology[L],
                                 rawLabelFeatures: Array[Array[Int]],
                                 rawDirFeatures: Array[Array[Int]],
                                 ruleFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 wordFeaturizer: IndexedWordFeaturizer[W],
                                 unaryFeaturizer: IndexedWordFeaturizer[W],
                                 bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                                 splitSpanFeaturizer: Option[IndexedSplitSpanFeaturizer[W]],
                                 val refinements: GrammarRefinements[L, L2],
                                 val useBilexRuleFeatures: Boolean,
                                 wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                 unaryFeatureIndex: CrossProductIndex[Feature, Feature],
                                 bilexFeatureIndex: CrossProductIndex[Feature, Feature],
                                 splitSpanFeatureIndex: CrossProductIndex[Feature, Feature]) extends RefinedFeaturizer[L, W, Feature] with Serializable {

  val index = SegmentedIndex(wordFeatureIndex, bilexFeatureIndex, unaryFeatureIndex, splitSpanFeatureIndex)
  private val wordOffset = index.componentOffset(0)
  private val bilexOffset = index.componentOffset(1)
  private val unaryOffset = index.componentOffset(2)
  private val splitOffset = index.componentOffset(3)

  override def lock = this

  def joinTagRef(head: Int, ref: Int, length: Int) : Int = {
    head + ref * length
  }

  def anchor(datum: IndexedSeq[W]):Spec = new Spec(datum)

  class Spec(val words: IndexedSeq[W]) extends Anchoring {
    private val fspec = ruleFeaturizer.anchor(words)
    private val bilexSpec = bilexFeaturizer.anchor(words)
    private val wordSpec = wordFeaturizer.anchor(words)
    private val unarySpec = unaryFeaturizer.anchor(words)
    val splitSpanSpec = splitSpanFeaturizer.map(_.anchor(words))

    def length = words.length

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val head = unaryHeadIndex(ref)
      if (head < begin || head >= end) throw new RuntimeException(s"Head $head not in bounds for rule $rule in span [$begin, $end)}")
      val ruleRef = unaryRuleRefinement(ref)
      val globalizedRule = refinements.rules.globalize(rule, ruleRef)
      var rcache = headCache(head)
      if (rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        headCache(head) = rcache
      }

      var headCached = rcache(globalizedRule)
      if (headCached == null)  {
        val surfFeatures = unarySpec.featuresForWord(head)
        val rFeatures = fspec.featuresForUnaryRule(begin, end, rule, ruleRef)
        headCached = unaryFeatureIndex.crossProduct(rFeatures, surfFeatures, unaryOffset)
        rcache(globalizedRule) = headCached
      }

      if (splitSpanSpec.isEmpty) {
        headCached
      } else {

        var ucache = unarySpanCache(begin, end)
        if (ucache eq null) {
          ucache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
          unarySpanCache(begin, end) = ucache
        }
        var surfCached = ucache(globalizedRule)
        if (surfCached == null)  {
          surfCached = splitSpanFeatureIndex.crossProduct(fspec.featuresForUnaryRule(begin, end, rule, ruleRef),
            getSpanFeatures(begin, end), splitOffset, true)
          ucache(globalizedRule) = surfCached
        }
        Arrays.concatenate(surfCached, headCached)
      }

    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      val localTagRef = tagRefinement(ref)
      val refinedTag = refinements.labels.globalize(tag, localTagRef)
      val head = headTagIndex(ref)
      if (head < begin || head >= end) throw new RuntimeException(s"Head $head not in bounds for tag $tag in span [$begin, $end)}")
      var rcache = wordCache(head)
      if (rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size, null:Array[Int], 2)
        wordCache(head) = rcache
      }
      var cache = rcache(refinedTag)
      if (cache == null) {
        cache = wordFeatureIndex.crossProduct(fspec.featuresForSpan(begin, end, tag, localTagRef),
          wordSpec.featuresForWord(head), offset = wordOffset, usePlainLabelFeatures = false)
        rcache(refinedTag) = cache
      }

      if (splitSpanSpec.nonEmpty && begin < end - 1) {
        var labelCache = spanCache(begin, end)
        if (labelCache eq null) {
          labelCache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size)
          spanCache(begin, end) = labelCache
        }
        var lcached = labelCache(refinedTag)
        if (lcached == null)  {
          val spanFeats: Array[Int] = fspec.featuresForSpan(begin, end, tag, localTagRef)
          lcached = splitSpanFeatureIndex.crossProduct(spanFeats, getSpanFeatures(begin, end), splitOffset, true)
          labelCache(refinedTag) = lcached
        }
        cache = Arrays.concatenate(cache, lcached)
      }
      cache
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) =  {
      val head = headIndex(ref)
      val dep = depIndex(ref)
      assert(head < end && head >= begin, (head, begin, end))
      assert(dep < end && dep >= begin, (dep, begin, end))
      assert( (head < split && end >= split) || (head >= split && dep < split))
      val ruleRef = binaryRuleRefinement(ref)

      val arrays = new ArrayBuffer[Array[Int]]()

      if (useBilexRuleFeatures) {
        arrays += featuresForHeadDepRule(begin, split, end, head, dep, rule, ruleRef)
      }

      if (splitSpanSpec.nonEmpty) {
        arrays += featuresForSplitRule(begin, split, end, rule, ruleRef)
      }

      val tag = grammar.parent(rule)
      val refinedTag = refinements.labels.globalize(tag, refinements.parentRefinement(rule, ruleRef))
      arrays += featuresForAttach(head, dep, refinedTag)

      Arrays.concatenate(arrays:_*)
    }

    def featuresForAttach(head: Int, dep: Int, refinedTag: Int) = {
      var cache = attachCache(head)(dep)
      if (cache == null) {
        cache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size)
        attachCache(head)(dep) = cache
      }

      var feats = cache(refinedTag)
      if (feats == null) {
        var bilexFeatures: Array[Int] = bilexCache(head)(dep)
        if (bilexFeatures eq null) {
          bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          bilexCache(head)(dep) = bilexFeatures
        }

        val fi = Arrays.concatenate(rawLabelFeatures(refinedTag), if (head < dep) rawDirFeatures(0) else rawDirFeatures(1))
        feats = bilexFeatureIndex.crossProduct(fi, bilexFeatures, offset = bilexOffset, usePlainLabelFeatures = false)
        cache(refinedTag) = feats

      }
      feats
    }

    def featuresForHeadDepRule(begin: Int, split: Int, end: Int, head: Int, dep: Int, rule: Int, ruleRef: Int): Array[Int] = {
      var cache = ruleCache(head)(dep)
      if (cache == null) {
        cache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size, null:Array[Int], 256)
        ruleCache(head)(dep) = cache
      }
      // val x = cache.activeSize * 1.0/cache.size
      // val y = cache.activeSize * 1.0/cache.data.length
      // if (math.random < .01) println(x + " " + y + " " + cache.size)
      var feats = cache(refinements.rules.globalize(rule, ruleRef))
      if (feats == null) {
        var bilexFeatures: Array[Int] = bilexCache(head)(dep)
        if (bilexFeatures eq null) {
          bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          bilexCache(head)(dep) = bilexFeatures
        }
        val fi = fspec.featuresForBinaryRule(begin, split, end, rule, ruleRef)
        feats = bilexFeatureIndex.crossProduct(fi, bilexFeatures, offset = bilexOffset, usePlainLabelFeatures = true)
        cache(refinements.rules.globalize(rule, ruleRef)) = feats
      }
      feats
    }

    def featuresForSplitRule(begin: Int, split: Int, end: Int, rule: Int, ruleRef: Int): Array[Int] = {
      val globalizedRule = refinements.rules.globalize(rule, ruleRef)

      var ucache = binaryCache(begin, end)
      if (ucache eq null) {
        ucache = new Array[OpenAddressHashArray[Array[Int]]](end - begin)
        binaryCache(begin, end) = ucache
      }

      var scache = ucache(split - begin)
      if (scache eq null) {
        scache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        ucache(split - begin) = scache
      }

      var lcached = scache(globalizedRule)
      if (lcached == null) {
      // val spanFeatures = getSpanFeatures(begin, end)
      // lcached = splitSpanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ruleRef), spanFeatures, splitOffset, true)
        lcached = splitSpanFeatureIndex.crossProduct(fspec.featuresForBinaryRule(begin, split, end, rule, ruleRef),
          getSplitFeatures(begin, split, end), splitOffset, true)
          // if (forSplit.length > 0)
          // lcached = Arrays.concatenate(lcached, forSplit)
        scache(globalizedRule) = lcached
      }
      lcached
    }

    def headIndex(ruleRef: Int) = {
      (ruleRef / words.length) % words.length
    }

    def depIndex(ruleRef: Int) = {
      ruleRef % words.length
    }

    def unaryHeadIndex(ref: Int) = {
      ref % words.length
    }

    def headTagIndex(ref: Int) = {
      ref % words.length
    }

    def binaryRuleRefinement(ref: Int) = {
      ref /(words.length * words.length)
    }

    def unaryRuleRefinement(ref: Int) = {
      ref / words.length
    }

    def tagRefinement(ref: Int) = {
      ref / words.length
    }

    def joinTagRef(head: Int, ref: Int) : Int = IndexedLexFeaturizer.this.joinTagRef(head, ref, words.length)

    private def getSpanFeatures(begin: Int, end: Int):Array[Int] = {
      var cache = rawSpanCache(begin, end)
      if (cache eq null) {
        cache = splitSpanSpec.get.featuresForSpan(begin, end)
        rawSpanCache(begin, end) = cache
      }
      cache
    }

    private def getSplitFeatures(begin: Int, split: Int, end: Int):Array[Int] = {
      var cache = rawSplitCache(begin, end)

      if (cache eq null) {
        cache = new Array[Array[Int]](end- begin)
        rawSplitCache(begin, end) = cache
      }

      var scache = cache(split - begin)
      if (scache eq null) {
        val span = getSpanFeatures(begin, end)
        scache = Arrays.concatenate(span, splitSpanSpec.get.featuresForSplit(begin, split, end))
        cache(split - begin) = scache
      }

      scache
    }

    // caches:

    // words
    // headIndex -> depIndex -> Array[Int]
    val bilexCache = Array.ofDim[Array[Int]](words.length, words.length)
    val ruleCache = Array.ofDim[OpenAddressHashArray[Array[Int]]](words.length, words.length)
    val attachCache = Array.ofDim[OpenAddressHashArray[Array[Int]]](words.length, words.length)
    // headIndex -> (ruleIndex) -> Array[Int]
    val headCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // headIndex -> (depIndex x ruleIndex) -> Array[Int]
    val wordCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    val rawSpanCache = new TriangularArray[Array[Int]](words.length + 1)
    val rawSplitCache = new TriangularArray[Array[Array[Int]]](words.length + 1)
    val unarySpanCache = new TriangularArray[OpenAddressHashArray[Array[Int]]](words.length + 1)
    // (begin, end) -> (split - begin) -> Array[Int]
    val binaryCache = new TriangularArray[Array[OpenAddressHashArray[Array[Int]]]](words.length + 1)
    val spanCache = new TriangularArray[OpenAddressHashArray[Array[Int]]](words.length + 1)
  }

}

final class LexGrammar[L, L2, W](val topology: RuleTopology[L],
                             val lexicon: Lexicon[L, W],
                             refinedGrammar: RuleTopology[L2],
                             featurizer: IndexedLexFeaturizer[L, L2, W],
                             weights: DenseVector[Double],
                             binaries: Array[Boolean],
                             leftRules: Array[Boolean],
                             rightRules: Array[Boolean]) extends Grammar[L, W] {


  override def withPermissiveLexicon: Grammar[L, W] = {
    new LexGrammar(topology, lexicon.morePermissive, refinedGrammar, featurizer, weights, binaries, leftRules, rightRules)
  }

  def isHeadOnLeftForRule(r: Int) = leftRules(r)

  def isHeadOnRightForRule(r: Int) = rightRules(r)

  override def anchor(words: IndexedSeq[W], constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = {
    new Spec(words, constraints)
  }

  def refinements = featurizer.refinements

  // refinement scheme:
  // binaryRule is (head * words.length + dep)
  // unaryRule is (head)
  // parent/leftchild/rightchild is (head)
  final case class Spec(words: IndexedSeq[W], sparsityPattern: ChartConstraints[L]) extends GrammarAnchoring[L, W] {
    override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & constraints)

    override def annotationTag: Int = 1

    val topology = LexGrammar.this.topology
    val lexicon = LexGrammar.this.lexicon
    private val f = featurizer.anchor(words)

    private def dot(features: Array[Int]) = {
      var i = 0
      var score = 0.0
      while (i < features.length) {
        score += weights(features(i))
        i += 1
      }
      score
    }

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      dot(f.featuresForSpan(begin, end, label, ref))
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      dot(f.featuresForUnaryRule(begin, end, rule, ref))
    }

    val attachCache = Array.ofDim[OpenAddressHashArray[Double]](words.length, words.length)
    val ruleCache = new TriangularArray[Array[OpenAddressHashArray[Double]]](words.length + 1)
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      var score = 0.0
      val head = headIndex(ref)
      val dep = depIndex(ref)
      if (head >= end || head < begin) return Double.NegativeInfinity
      if (dep >= end || dep < begin) return Double.NegativeInfinity

      var cache = attachCache(head)(dep)
      if (cache == null) {
        cache = new OpenAddressHashArray[Double](refinements.labels.fineIndex.size, Double.NaN)
        attachCache(head)(dep) = cache
      }

      val tag = topology.parent(rule)
      val ruleRef = this.binaryRuleRef(ref)
      val refinedTag = refinements.labels.globalize(tag, refinements.parentRefinement(rule, ruleRef))
      var attachScore = cache(refinedTag)
      if (java.lang.Double.isNaN(attachScore)) {
        attachScore = dot(f.featuresForAttach(head, dep, refinedTag))
        cache(refinedTag) = attachScore
      }
      score += attachScore

      if (f.splitSpanSpec.nonEmpty) {
        var ucache = ruleCache(begin, end)
        if (ucache eq null) {
          ucache = new Array[OpenAddressHashArray[Double]](end - begin)
          ruleCache(begin, end) = ucache
        }

        var scache = ucache(split - begin)
        if (scache eq null) {
          scache = new OpenAddressHashArray[Double](refinements.rules.fineIndex.size, Double.NaN)
          ucache(split - begin) = scache
        }

        val globalizedRule = refinements.rules.globalize(rule, ruleRef)

        var lcached = scache(globalizedRule)
        if (java.lang.Double.isNaN(lcached)) {
          lcached = dot(f.featuresForSplitRule(begin, split, end, rule, ruleRef))
          scache(globalizedRule) = lcached
        }

        score += lcached
      }

      if (featurizer.useBilexRuleFeatures) {
        score += dot(f.featuresForHeadDepRule(begin, split, end, head, dep, rule, ruleRef))
      }

      score
    }

    def headIndex(ruleRef: Int) = f.headIndex(ruleRef)
    def depIndex(ruleRef: Int) = f.depIndex(ruleRef)
    def unaryHeadIndex(ref: Int) = f.unaryHeadIndex(ref)
    def binaryRuleRef(ref: Int) = f.binaryRuleRefinement(ref)
    def unaryRuleRef(ref: Int) = f.unaryRuleRefinement(ref)
    def tagRef(ref: Int) = f.tagRefinement(ref)
    def joinTagRef(head: Int, ref: Int) = f.joinTagRef(head, ref)


    def joinBinaryRuleRef(head: Int, ref: Int) : Int = {
      head + ref * words.length * words.length
    }

    def joinUnaryRuleRef(head: Int, ref: Int) : Int = {
      head + ref * words.length
    }

    def joinUnaryRuleRefs(lexRefs: Array[Int], ruleRefs: Array[Int]) = {
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs, words.length)
    }

    def joinBinaryRuleRefs(lexRefs: Array[Int], ruleRefs: Array[Int]) = {
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs, words.length * words.length)
    }

    def joinTagRefs(lexRefs: Array[Int], ruleRefs: Array[Int]) = {
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs, words.length)
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = joinTagRefs(Array.range(begin,end), refinements.labels.localRefinements(label))

    def numValidRefinements(label: Int) = joinTagRef(words.length, refinements.labels.numRefinements(label))

    def numValidRuleRefinements(rule: Int): Int = {
      if (binaries(rule)) {
        joinBinaryRuleRef(words.length * words.length, refinements.rules.numRefinements(rule))
      } else {
        joinUnaryRuleRef(words.length, refinements.rules.numRefinements(rule))
      }
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      if (!binaries(rule)) {
        val lexicalizedRefinements = Array(unaryHeadIndex(parentRef))
        val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
        joinUnaryRuleRefs(lexicalizedRefinements, ruleRefs)
      } else {
       val lexicalizedRefinements = if (isHeadOnLeftForRule(rule)) {
         val head = unaryHeadIndex(parentRef)
          //        val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => leftChildRefinement(rule,x) == parentRef && rightChildRefinement(rule, x) > parentRef && rightChildRefinement(rule, x) < end)
          val result = new Array[Int](end - (head+1))
          var ref = head * words.length + head + 1
          var i = 0
          while(i < result.length) {
            result(i) = ref
            ref += 1
            i += 1
          }
          result
        } else {
         val head = unaryHeadIndex(parentRef)
          //        val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => rightChildRefinement(rule,x) == parentRef && leftChildRefinement(rule, x) < parentRef && leftChildRefinement(rule, x) >= begin)
          val result = new Array[Int](head - begin)
          var ref = head * words.length + begin
          var i = 0
          while(i < result.length) {
            result(i) = ref
            i += 1
            ref += 1
          }
          //        assert(x.toSet == result.toSet)
          result
        }
         val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))

        joinBinaryRuleRefs(lexicalizedRefinements, ruleRefs)
      }

    }


    override def validRuleRefinementsGivenParent(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
      if (!binaries(rule)) {
        val lexicalizedRefinements = Array(parentRef:Int)
        val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
        joinUnaryRuleRefs(lexicalizedRefinements, ruleRefs)
      } else {
        val headIndex = unaryHeadIndex(parentRef)
        val lexicalizedRefinements = if (isHeadOnLeftForRule(rule)) {
          // if the head is on the left, then the dependent
          // can be in Span(math.max(splitBegin, ref1+1), end).
          // Further, if the ref1 is <= splitEnd, then
          // we can't even build this rule with this parent.
          // [begin....splitBegin....splitEnd...end)
          //  ^------ref1------^
          // max:      ^------^----dep---------^
          //
          if (splitEnd <= headIndex) return Array.empty
          val firstPossibleStart = math.max(headIndex +1, splitBegin)
          val result = new Array[Int](end - firstPossibleStart)
          var ref = headIndex * words.length + firstPossibleStart
          var i = 0
          while(i < result.length) {
            result(i) = ref
            ref += 1
            i += 1
          }
          result
        } else {
          // if the head is on the right, then the dependent
          // can be in (begin until math.min(splitEnd,ref1))
          // Further, if the ref1 is <= splitBegin, then
          // we can't even build this rule with this parent.
          // [begin....splitBegin....splitEnd...end)
          //           ^--------ref1------^
          //  ^-----------dep---^-----^ : min
          //
          if (splitBegin >= headIndex) return Array.empty
          val lastPossibleEnd = math.min(headIndex, splitEnd)
          val result = new Array[Int](lastPossibleEnd - begin)
          var ref = headIndex * words.length + begin
          var i = 0
          while(i < result.length) {
            result(i) = ref
            i += 1
            ref += 1
          }
          result
        }

        if (lexicalizedRefinements.isEmpty) {
          lexicalizedRefinements
        } else {
          val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
          joinBinaryRuleRefs(lexicalizedRefinements, ruleRefs)
        }
      }

    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin:Int, completionEnd: Int, rule: Int, lcRef: Int) = {
      val lexicalizedRefinements = if (isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](completionEnd - split)
        val lc = unaryHeadIndex(lcRef)
        var ref = lc * words.length + split
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
        val lc = unaryHeadIndex(lcRef)
        val result = new Array[Int](completionEnd - split)
        var ref = split * words.length + lc
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
        result
      }
      val ruleRefs = refinements.ruleRefinementsCompatibleWithLeftRef(rule, tagRef(lcRef))
      joinBinaryRuleRefs(lexicalizedRefinements, ruleRefs)
    }

    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, rcRef: Int): Array[Int] = {
      val rc = unaryHeadIndex(rcRef)
      val lexicalizedRefinements = if (!isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](split - completionBegin)
        var ref = rc * words.length + completionBegin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
        val result = new Array[Int](split - completionBegin)
        var ref = completionBegin * words.length + rc
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
        result
      }
      val ruleRefs = refinements.ruleRefinementsCompatibleWithRightRef(rule, tagRef(rcRef))
      joinBinaryRuleRefs(lexicalizedRefinements, ruleRefs)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      val lexicalizedRefinements = Array(unaryHeadIndex(childRef))
      val ruleRefs = refinements.ruleRefinementsCompatibleWithChildRef(rule, tagRef(childRef))
      joinUnaryRuleRefs(lexicalizedRefinements, ruleRefs)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      val word = if (isHeadOnLeftForRule(rule)) {
        headIndex(ruleRef)
      }  else {
        depIndex(ruleRef)
      }
      val refinedRuleId = refinements.rules.globalize(rule, binaryRuleRef(ruleRef))
      val tagref = refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
      joinTagRef(word, tagref)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val word = if (isHeadOnRightForRule(rule)) {
        headIndex(ruleRef)
      } else {
        depIndex(ruleRef)
      }
      val refinedRuleId = refinements.rules.globalize(rule, binaryRuleRef(ruleRef))
      val tagref = refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
      joinTagRef(word, tagref)
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val word = if (binaries(rule)) {
        headIndex(ruleRef)
      } else {
        unaryHeadIndex(ruleRef)
      }
      val rr = if (binaries(rule)) {
        binaryRuleRef(ruleRef)
      } else {
        unaryRuleRef(ruleRef)
      }
      val refinedRuleId = refinements.rules.globalize(rule, rr)
      val tagref = refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
      joinTagRef(word, tagref)
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      val word = unaryHeadIndex(ruleRef)
      val refinedRuleId = refinements.rules.globalize(rule, unaryRuleRef(ruleRef))
      val tagref = refinements.labels.localize(refinedGrammar.child(refinedRuleId))
      joinTagRef(word, tagref)
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      val labelA = tagRef(refA)
      val labelB = tagRef(refB)
      val hA = unaryHeadIndex(refA)
      val hB = unaryHeadIndex(refB)
      require(hA == hB, s"Parent head for rule ${topology.index.get(r)} was '${words(hA)}' and child head was '${words(hB)}', but should be the same!" + words)
      val a = topology.parent(r)
      val b = topology.child(r)
      val a2 = refinements.labels.globalize(a, labelA)
      val b2 = refinements.labels.globalize(b, labelB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), topology.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      val refR = if (refinedRuleIndex < 0) {
        -1
      } else {
        refinements.rules.localize(refinedRuleIndex)
      }
      joinUnaryRuleRef(hA, refR)
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val hA = unaryHeadIndex(refA)
      val hB = unaryHeadIndex(refB)
      val hC = unaryHeadIndex(refC)
      val lexRef = if (isHeadOnLeftForRule(r)) {
        require(hA == hB)
        hA * words.length + hC
      } else {
        require(hA == hC)
        hA * words.length + hB
      }
      val labelA = tagRef(refA)
      val labelB = tagRef(refB)
      val labelC = tagRef(refC)
      val a = topology.parent(r)
      val b = topology.leftChild(r)
      val c = topology.rightChild(r)
      val a2 = refinements.labels.globalize(a, labelA)
      val b2 = refinements.labels.globalize(b, labelB)
      val c2 = refinements.labels.globalize(c, labelC)
      val refR = refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      )))
      assert(headIndex(lexRef) == hA)
      joinBinaryRuleRef(lexRef, refR)
    }

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = {
      refinements.coarseRulesGivenParentRef(a, tagRef(refA))
    }

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      val lexRefs = {
        if (isHeadOnLeftForRule(rule)) Array.range(begin, splitEnd)
        else Array.range(splitBegin, end)
      }
      joinTagRefs(lexRefs, refinements.parentRefinementsCompatibleWithRule(rule))
    }

    def validLeftChildRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      val lexRefs = Array.range(begin, splitEnd)
      joinTagRefs(lexRefs, refinements.leftChildRefinementsCompatibleWithRule(rule))
    }

    def validRightChildRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      val lexRefs = Array.range(splitBegin, end)
      joinTagRefs(lexRefs, refinements.rightChildRefinementsCompatibleWithRule(rule))
    }
  }

}

case class LexGrammarBundle[L, L2, W](topology: RuleTopology[L],
                                  baseLexicon: Lexicon[L, W],
                                  refinedGrammar: RuleTopology[L2],
                                  headFinder: HeadFinder[L]) { bundle =>
  val bg = topology
  val leftRules = new Array[Boolean](bg.index.size)
  val rightRules = new Array[Boolean](bg.index.size)
  val binaries = new Array[Boolean](bg.index.size)

  for( (rule@BinaryRule(a, b,c), r) <- bg.index.iterator.zipWithIndex) {
    binaries(r) = true
    val headChild = headFinder.findHeadChild(rule)
    if (headChild == 0) {
      leftRules(r) = true
    } else {
      rightRules(r) = true
    }
  }

  def makeGrammar(fi: IndexedLexFeaturizer[L, L2, W], weights: DenseVector[Double]): LexGrammar[L, L2, W] = {
    new LexGrammar(topology, baseLexicon, refinedGrammar, fi, weights, binaries, leftRules, rightRules)
  }
}

object IndexedLexFeaturizer extends SerializableLogging {
  def extract[L, L2, Datum, W](ruleFeaturizer: ProductionFeaturizer[L, L2, W],
                           bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                           splitSpanFeaturizer: Option[IndexedSplitSpanFeaturizer[W]],
                           unaryFeaturizer: IndexedWordFeaturizer[W],
                           wordFeaturizer: IndexedWordFeaturizer[W],
                           headFinder: HeadFinder[L2],
                           ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                           refinements: GrammarRefinements[L, L2],
                           useBilexRuleFeatures: Boolean,
                           dummyFeatScale: HashFeature.Scale = HashFeature.Absolute(0),
                           trees: Traversable[Datum])(implicit hasWords: Has2[Datum, IndexedSeq[W]], hasTree: Has2[Datum, BinarizedTree[L]]) : IndexedLexFeaturizer[L, L2, W] = {

    val bilexFeatureIndex = bilexFeaturizer.featureIndex
    val wordFeatureIndex = wordFeaturizer.featureIndex
    val unaryFeatureIndex = unaryFeaturizer.featureIndex
    val splitSpanFeatureIndex = splitSpanFeaturizer.map(_.featureIndex)

    val labelFeatures = Array.tabulate(refinements.labels.fineIndex.size)(rf => ruleFeaturizer.featuresFor(refinements.labels.fineIndex.get(rf)))
    val attachFeatures = Array(Array(ruleFeaturizer.index(AttachLeft)), Array(ruleFeaturizer.index(AttachRight)))

    val progress = new ProgressLog(logger, trees.size, name="LexFeatures")

    val ruleFeatureIndex = ruleFeaturizer.index
    val bilexBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, bilexFeatureIndex, dummyFeatScale, "Bilex", includeLabelOnlyFeatures = true)
    val wordBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, wordFeatureIndex, dummyFeatScale, "Monolex", includeLabelOnlyFeatures = false)
    val unaryBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, unaryFeatureIndex, dummyFeatScale, "Unary", includeLabelOnlyFeatures = true)
    val splitBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, splitSpanFeatureIndex.getOrElse(Index[Feature]()), dummyFeatScale, "Split", includeLabelOnlyFeatures = false)

    for(ti <- trees) {
      val ruleSpec = ruleFeaturizer.anchor(hasWords.get(ti))
      val bilexSpec = bilexFeaturizer.anchor(hasWords.get(ti))
      val wordSpec = wordFeaturizer.anchor(hasWords.get(ti))
      val unarySpec = unaryFeaturizer.anchor(hasWords.get(ti))
      val splitSpanSpec = splitSpanFeaturizer.map(_.anchor(hasWords.get(ti)))
      val words = hasWords.get(ti)
      val tree = ann(hasTree.get(ti), words)
      // returns head
      def rec(t: BinarizedTree[L2]): Int= t match {
        case NullaryTree(a, span) =>
          val (ai, aref) = refinements.labels.indexAndLocalize(a)
          wordBuilder.add(ruleSpec.featuresForSpan(span.begin, span.end, ai, aref),
                          wordSpec.featuresForWord(span.begin))
          span.begin
        case UnaryTree(a, b, chain, span) =>
          val head = rec(b)
          val r = UnaryRule(a, b.label, chain)
          val (ri, rref) = refinements.rules.indexAndLocalize(r)
          unaryBuilder.add(ruleSpec.featuresForUnaryRule(span.begin, span.end, ri, rref),
                           unarySpec.featuresForWord(head))
          if (splitSpanSpec.nonEmpty)
            splitBuilder.add(ruleSpec.featuresForUnaryRule(span.begin, span.end, ri, rref),
              splitSpanSpec.get.featuresForSpan(span.begin, span.end))
          head
        case t@BinaryTree(a, b, c, span) =>
          val (leftHead,rightHead) = (rec(t.leftChild), rec(t.rightChild))
          val headIsLeft = headFinder.findHeadChild(t) == 0
          val (head, dep) = if (headIsLeft) leftHead -> rightHead else rightHead -> leftHead
          val r = BinaryRule[L2](a, b.label, c.label)
          val (ri, rref) = refinements.rules.indexAndLocalize(r)
          val bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          val split = t.splitPoint
          val (ai, aref) = refinements.labels.indexAndLocalize(a)
          wordBuilder.add(ruleSpec.featuresForSpan(span.begin, span.end, ai, aref),
            wordSpec.featuresForWord(head))
          val aglob = refinements.labels.fineIndex(a)

          if (useBilexRuleFeatures)
            bilexBuilder.add(ruleSpec.featuresForBinaryRule(span.begin, split, span.end, ri, rref), bilexFeatures)
          bilexBuilder.add(labelFeatures(aglob), bilexFeatures)
          bilexBuilder.add(attachFeatures(if (headIsLeft) 0 else 1), bilexFeatures)

          if (splitSpanFeaturizer.nonEmpty) splitBuilder.add(ruleSpec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
            splitSpanSpec.get.featuresForSpan(span.begin, span.end))

          if (splitSpanFeaturizer.nonEmpty) splitBuilder.add(ruleSpec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, ri, rref),
            splitSpanSpec.get.featuresForSplit(span.begin, t.splitPoint, span.end))

          if (splitSpanFeaturizer.nonEmpty) splitBuilder.add(ruleSpec.featuresForSpan(span.begin, span.end, ai, aref),
            splitSpanSpec.get.featuresForSpan(span.begin, span.end))
          head
      }
      rec(tree)
      progress.info(s"${bilexBuilder.size} bilex features; ${wordBuilder.size} word features; ${unaryBuilder.size} unary features")
    }

    val wfi = wordBuilder.result()
    val bfi = bilexBuilder.result()
    assert(bfi.includePlainLabelFeatures)
    val ufi = unaryBuilder.result()
    val sfi = splitBuilder.result()

    new IndexedLexFeaturizer(ruleFeaturizer.topology,
      labelFeatures,
      attachFeatures,
      ruleFeaturizer,
      wordFeaturizer,
      unaryFeaturizer,
      bilexFeaturizer,
      splitSpanFeaturizer,
      refinements,
      useBilexRuleFeatures,
      wfi, ufi, bfi, sfi)
  }
}

case class LexModelFactory(@Help(text= "The kind of annotation to do on the refined grammar. Defaults to xbar.")
                           annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = Xbarize(),
                           @Help(text="Old weights to initialize with. Optional")
                           oldWeights: File = null,
                           @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                           dummyFeats: Double = 1.0,
                           @Help(text="How common must a feature be before we remember it?")
                           minFeatCutoff: Int = 1,
                           useSpanFeatures: Boolean = false,
                           useBilexRuleFeatures: Boolean = true) extends ParserModelFactory[AnnotatedLabel, String] with SerializableLogging {
  type MyModel = LexModel[AnnotatedLabel, AnnotatedLabel, String]

  override def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                    topology: RuleTopology[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String], constrainer: Factory[AnnotatedLabel, String]): MyModel = {

    val trees = trainTrees.map(annotator)
    val (initLexicon, annBinaries, annUnaries) = GenerativeParser.extractCounts(trees)
    val refGrammar = RuleTopology(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val indexedRefinements = GrammarRefinements(topology, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val (wordFeaturizer, unaryFeaturizer, bilexFeaturizer) = {
      val dsl = new WordFeaturizer.DSL(initLexicon) with BilexicalFeaturizer.DSL with SurfaceFeaturizer.DSL
      import dsl._

      val wf =  unigrams(word, 1) + suffixes() + prefixes()
      val offsets = lfsuf(-1) + lfsuf(1)
      var bilexF:BilexicalFeaturizer[String] = (
        withDistance(bilex(lfsuf) + lfsuf(head) + lfsuf(dep))
          + adaptSpanFeaturizer(spanShape)
        + offsets(head)
        + offsets(dep)
        )

      bilexF = bilexF

      (wf, lfsuf + offsets, bilexF)
    }

    val spanFeaturizer = if (!useSpanFeatures) {
      new ZeroSplitSpanFeaturizer[String]
    } else {
      val dsl = new WordFeaturizer.DSL(initLexicon) with SurfaceFeaturizer.DSL with SplitSpanFeaturizer.DSL
      import dsl._

      // class(split + 1)
      val baseCat = lfsuf

      val leftOfSplit = baseCat(-1)apply split

      var featurizer: SplitSpanFeaturizer[String] = zeroSplit[String]
//      if (useFirstLast) {
        featurizer += baseCat(begin)
        featurizer += baseCat(end-1)
//      }
//      if (useBeforeAfter) {
        featurizer += baseCat(begin-1)
        featurizer += baseCat(end)
//      }

//      if (useSplits) {
        featurizer += leftOfSplit
        featurizer += baseCat(split)
//      }

//      if (useSpanLength) {
        featurizer += length
//      }

//      if (useShape) {
        featurizer += spanShape
//      }

//      if (useBinaryLengths) {
        featurizer += distance[String](begin, split)
        featurizer += distance[String](split, end)
//      }

      featurizer
    }

    val headFinder = HeadFinder.collins.lensed[AnnotatedLabel]

    val indexedWordFeaturizer = IndexedWordFeaturizer.fromData(wordFeaturizer, trees.map(_.words))
    val indexedUnaryFeaturizer = IndexedWordFeaturizer.fromData(unaryFeaturizer, trees.map(_.words))
    val indexedBilexicalFeaturizer = {
      val dependencyTrees = trees.map ( DependencyTree.fromTreeInstance[AnnotatedLabel, String](_, headFinder) )
      IndexedBilexicalFeaturizer.fromData(bilexFeaturizer, dependencyTrees)
    }

    val indexedSplitSpanFeaturizer = {
      if (useSpanFeatures)
        Some(IndexedSplitSpanFeaturizer.fromData(spanFeaturizer, trees))
      else
        None
    }

    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = r match {
      case r@BinaryRule(a,b,c) =>
        val headIsLeft = headFinder.findHeadChild(r) == 0
        val dir = if (headIsLeft) AttachLeft else AttachRight
        Set(r, r.map(_.baseAnnotatedLabel), dir).toSeq
      case r@UnaryRule(a,b,c) =>
        Set(r, r.map(_.baseAnnotatedLabel)).toSeq
    }

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](topology, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: IndexedSeq[String]) = annotator(tree, words)

    type W = String
    type L = AnnotatedLabel
    val indexed =  IndexedLexFeaturizer.extract(featurizer,
      indexedBilexicalFeaturizer,
      indexedSplitSpanFeaturizer,
      indexedUnaryFeaturizer,
      indexedWordFeaturizer,
      headFinder,
      annotator,
      indexedRefinements,
      useBilexRuleFeatures,
      HashFeature.Relative(dummyFeats),
      trainTrees)

     logger.info(s"Num features: Indexed Features: ${indexed.index.size}")

    val bundle = new LexGrammarBundle(topology,
      lexicon,
      refGrammar,
      headFinder)

    val featureCounter = readWeights(oldWeights)

    new LexModel(bundle, reannotate, indexed, constrainer, featureCounter.get)
  }
}
