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
import epic.framework._
import breeze.collection.mutable.OpenAddressHashArray
import breeze.linalg._
import epic.trees._
import epic.trees.annotations.{Xbarize, TreeAnnotator, StripAnnotations}
import java.io.File
import epic.parser._
import breeze.util._
import breeze.config.Help
import epic.lexicon.Lexicon
import epic.features._
import epic.parser.features._
import epic.util._
import com.typesafe.scalalogging.slf4j.Logging
import epic.parser.features._
import epic.trees._
import epic.parser.projections.GrammarRefinements

class LexModel[L, L2, W](bundle: LexGrammarBundle[L, L2, W],
                         reannotate: (BinarizedTree[L], IndexedSeq[W])=>BinarizedTree[L2],
                         indexed: IndexedLexFeaturizer[L, L2, W],
                         baseFactory: CoreGrammar[L, W],
                         initFeatureValue: Feature=>Option[Double]) extends ParserModel[L, W] with Serializable with ParserExtractable[L, W] {


  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
    m.expectedCounts(indexed, accum, scale)
  }

  def baseGrammar: BaseGrammar[L] = bundle.baseGrammar
  def lexicon = bundle.baseLexicon

  val featureIndex = indexed.index

  def initialValueForFeature(f: Feature) = initFeatureValue(f).getOrElse(0)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram = bundle.makeGrammar(indexed, weights)
    def ann(tree: BinarizedTree[L], words: IndexedSeq[W]):BinarizedTree[(L, Int)] = {
      val reannotated = reannotate(tree, words)
      val headed = bundle.headFinder.projected(indexed.refinements.labels.project(_:L2)).annotateHeadIndices(reannotated)
      headed.map { case (l2, head) =>
        indexed.refinements.labels.project(l2) -> indexed.joinTagRef(head, indexed.refinements.labels.localize(l2), words.length)
      }

    }
    new AnnotatedParserInference(indexed, ann _, gram, baseFactory)
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
class IndexedLexFeaturizer[L, L2, W](ruleFeaturizer: RefinedFeaturizer[L, W, Feature],
                                 wordFeaturizer: IndexedWordFeaturizer[W],
                                 unaryFeaturizer: IndexedWordFeaturizer[W],
                                 bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                                 val refinements: GrammarRefinements[L, L2],
                                 wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                 unaryFeatureIndex: CrossProductIndex[Feature, Feature],
                                 bilexFeatureIndex: CrossProductIndex[Feature, Feature]) extends RefinedFeaturizer[L, W, Feature] with Serializable {

  val index = SegmentedIndex(wordFeatureIndex, bilexFeatureIndex, unaryFeatureIndex)
  private val wordOffset = index.componentOffset(0)
  private val bilexOffset = index.componentOffset(1)
  private val unaryOffset = index.componentOffset(2)


  def joinTagRef(head: Int, ref: Int, length: Int) : Int = {
    head + ref * length
  }


  def anchor(datum: IndexedSeq[W]):Spec = new Spec(datum)

  class Spec(val words: IndexedSeq[W]) extends Anchoring {
    private val fspec = ruleFeaturizer.anchor(words)
    private val bilexSpec = bilexFeaturizer.anchor(words)
    private val wordSpec = wordFeaturizer.anchor(words)
    private val unarySpec = unaryFeaturizer.anchor(words)

    def length = words.length

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val head = unaryHeadIndex(ref)
      if(head < begin || head >= end) throw new RuntimeException(s"Head $head not in bounds for rule $rule in span [$begin, $end)}")
      val ruleRef = unaryRuleRefinement(ref)
      val globalizedRule = refinements.rules.globalize(rule, ruleRef)
      var rcache = headCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        headCache(head) = rcache
      }
      var cache = rcache(globalizedRule)
      if(cache == null)  {
        val surfFeatures = unarySpec.featuresForWord(head)
        val rFeatures = fspec.featuresForUnaryRule(begin, end, rule, ruleRef)
        cache = unaryFeatureIndex.crossProduct(rFeatures, surfFeatures, unaryOffset)
        rcache(globalizedRule) = cache
      }
      cache
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      val localTagRef = tagRefinement(ref)
      val refinedTag = refinements.labels.globalize(tag, localTagRef)
      val head = headTagIndex(ref)
      if(head < begin || head >= end) throw new RuntimeException(s"Head $head not in bounds for tag $tag in span [$begin, $end)}")
      var rcache = wordCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size, null:Array[Int], 2)
        wordCache(head) = rcache
      }
      var cache = rcache(refinedTag)
      if(cache == null) {
        cache = wordFeatureIndex.crossProduct(fspec.featuresForSpan(begin, end, tag, localTagRef),
          wordSpec.featuresForWord(head), offset = wordOffset, usePlainLabelFeatures = false)
        rcache(refinedTag) = cache
      }
      cache
    }


    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) =  {
      val head = headIndex(ref)
      val dep = depIndex(ref)
      assert(head < end && head >= begin, (head, begin, end))
      assert(dep < end && dep >= begin, (dep, begin, end))
      assert( (head < split && end >= split) || (head >= split && dep < split))
      val r = binaryRuleRefinement(ref)

      var cache = ruleCache(head)(dep)
      if (cache == null) {
        cache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        ruleCache(head)(dep) = cache
      }

      var feats = cache(refinements.rules.globalize(rule, r))
      if(feats == null) {
        var bilexFeatures: Array[Int] = bilexCache(head)(dep)
        if(bilexFeatures eq null) {
          bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          bilexCache(head)(dep) = bilexFeatures
        }

        val fi = fspec.featuresForBinaryRule(begin, split, end, rule, r)
        feats = bilexFeatureIndex.crossProduct(fi, bilexFeatures, offset = bilexOffset, usePlainLabelFeatures = true)
        cache(refinements.rules.globalize(rule, r)) = feats

      }

      feats
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

    // caches:

    // words
    // headIndex -> depIndex -> Array[Int]
    val bilexCache = Array.ofDim[Array[Int]](words.length, words.length)
    val ruleCache = Array.ofDim[OpenAddressHashArray[Array[Int]]](words.length, words.length)
    // headIndex -> (ruleIndex) -> Array[Int]
    val headCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // headIndex -> (depIndex x ruleIndex) -> Array[Int]
    val wordCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
  }

}

final class LexGrammar[L, L2, W](val grammar: BaseGrammar[L],
                             val lexicon: Lexicon[L, W],
                             refinedGrammar: BaseGrammar[L2],
                             featurizer: IndexedLexFeaturizer[L, L2, W],
                             weights: DenseVector[Double],
                             binaries: Array[Boolean],
                             leftRules: Array[Boolean],
                             rightRules: Array[Boolean]) extends RefinedGrammar[L, W] {
  def isHeadOnLeftForRule(r: Int) = leftRules(r)

  def isHeadOnRightForRule(r: Int) = rightRules(r)

  def anchor(sent: IndexedSeq[W]) = new Spec(sent)

  def refinements = featurizer.refinements

  // refinement scheme:
  // binaryRule is (head * words.length + dep)
  // unaryRule is (head)
  // parent/leftchild/rightchild is (head)
  final class Spec(val words: IndexedSeq[W]) extends RefinedAnchoring[L, W] {
    override def annotationTag: Int = 1

    val grammar = LexGrammar.this.grammar
    val lexicon = LexGrammar.this.lexicon
    private val f = featurizer.anchor(words)

    private def dot(features: Array[Int]) = {
      var i = 0
      var score = 0.0
      while(i < features.length) {
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


    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      dot(f.featuresForBinaryRule(begin, split, end, rule, ref))
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
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs)(_ + _ * words.length)
    }

    def joinBinaryRuleRefs(lexRefs: Array[Int], ruleRefs: Array[Int]) = {
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs)(_ + _ * words.length * words.length)
    }

    def joinTagRefs(lexRefs: Array[Int], ruleRefs: Array[Int]) = {
      epic.util.Arrays.crossProduct(lexRefs, ruleRefs)(_ + _ * words.length)
    }


    def validLabelRefinements(begin: Int, end: Int, label: Int) = joinTagRefs(Array.range(begin,end), refinements.labels.localRefinements(label))

    def numValidRefinements(label: Int) = joinTagRef(words.length, refinements.labels.numRefinements(label))

    def numValidRuleRefinements(rule: Int): Int = {
      if(binaries(rule)) {
        joinBinaryRuleRef(words.length * words.length, refinements.rules.numRefinements(rule))
      } else {
        joinUnaryRuleRef(words.length, refinements.rules.numRefinements(rule))
      }
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      if(!binaries(rule)) {
        val lexicalizedRefinements = Array(unaryHeadIndex(parentRef))
        val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
        joinUnaryRuleRefs(lexicalizedRefinements, ruleRefs)
      } else {
       val lexicalizedRefinements = if(isHeadOnLeftForRule(rule)) {
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
      if(!binaries(rule)) {
        val lexicalizedRefinements = Array(parentRef:Int)
        val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
        joinUnaryRuleRefs(lexicalizedRefinements, ruleRefs)
      } else {
        val headIndex = unaryHeadIndex(parentRef)
        val lexicalizedRefinements = if(isHeadOnLeftForRule(rule)) {
          // if the head is on the left, then the dependent
          // can be in Span(math.max(splitBegin, ref1+1), end).
          // Further, if the ref1 is <= splitEnd, then
          // we can't even build this rule with this parent.
          // [begin....splitBegin....splitEnd...end)
          //  ^------ref1------^
          // max:      ^------^----dep---------^
          //
          if(splitEnd <= headIndex) return Array.empty
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
          if(splitBegin >= headIndex) return Array.empty
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

        if(lexicalizedRefinements.isEmpty) {
          lexicalizedRefinements
        } else {
          val ruleRefs = refinements.ruleRefinementsCompatibleWithParentRef(rule, tagRef(parentRef))
          joinBinaryRuleRefs(lexicalizedRefinements, ruleRefs)
        }
      }

    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin:Int, completionEnd: Int, rule: Int, lcRef: Int) = {
      val lexicalizedRefinements = if(isHeadOnLeftForRule(rule)) {
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
      val lexicalizedRefinements = if(!isHeadOnLeftForRule(rule)) {
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
      val word = if(isHeadOnLeftForRule(rule)) {
        headIndex(ruleRef)
      }  else {
        depIndex(ruleRef)
      }

      val refinedRuleId = refinements.rules.globalize(rule, binaryRuleRef(ruleRef))
      val tagref = refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))

      joinTagRef(word, tagref)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val word = if(isHeadOnRightForRule(rule)) {
        headIndex(ruleRef)
      } else {
        depIndex(ruleRef)
      }

      val refinedRuleId = refinements.rules.globalize(rule, binaryRuleRef(ruleRef))
      val tagref = refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
      joinTagRef(word, tagref)
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val word = if(binaries(rule)) {
        headIndex(ruleRef)
      } else {
        unaryHeadIndex(ruleRef)
      }

      val rr = if(binaries(rule)) {
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
      require(hA == hB, s"Parent head for rule ${grammar.index.get(r)} was '${words(hA)}' and child head was '${words(hB)}', but should be the same!" + words)
      val a = grammar.parent(r)
      val b = grammar.child(r)
      val a2 = refinements.labels.globalize(a, labelA)
      val b2 = refinements.labels.globalize(b, labelB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), grammar.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      val refR = if(refinedRuleIndex < 0) {
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

      val lexRef = if(isHeadOnLeftForRule(r)) {
        require(hA == hB)
        hA * words.length + hC
      } else {
        require(hA == hC)
        hA * words.length + hB
      }

      val labelA = tagRef(refA)
      val labelB = tagRef(refB)
      val labelC = tagRef(refC)

      val a = grammar.parent(r)
      val b = grammar.leftChild(r)
      val c = grammar.rightChild(r)
      val a2 = refinements.labels.globalize(a, labelA)
      val b2 = refinements.labels.globalize(b, labelB)
      val c2 = refinements.labels.globalize(c, labelC)
      val refR = refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      ))  )

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

case class LexGrammarBundle[L, L2, W](baseGrammar: BaseGrammar[L],
                                  baseLexicon: Lexicon[L, W],
                                  refinedGrammar: BaseGrammar[L2],
                                  headFinder: HeadFinder[L]) { bundle =>
  val bg = baseGrammar
  val leftRules = new Array[Boolean](bg.index.size)
  val rightRules = new Array[Boolean](bg.index.size)
  val binaries = new Array[Boolean](bg.index.size)

  for( (rule@BinaryRule(a, b,c), r) <- bg.index.iterator.zipWithIndex) {
    binaries(r) = true
    val headChild = headFinder.findHeadChild(rule)
    if(headChild == 0) {
      leftRules(r) = true
    } else {
      rightRules(r) = true
    }
  }

  def makeGrammar(fi: IndexedLexFeaturizer[L, L2, W], weights: DenseVector[Double]): LexGrammar[L, L2, W] = {
    new LexGrammar(baseGrammar, baseLexicon, refinedGrammar, fi, weights, binaries, leftRules, rightRules)
  }
}

object IndexedLexFeaturizer extends Logging {
  def extract[L, L2, Datum, W](ruleFeaturizer: RefinedFeaturizer[L, W, Feature],
                           bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                           unaryFeaturizer: IndexedWordFeaturizer[W],
                           wordFeaturizer: IndexedWordFeaturizer[W],
                           headFinder: HeadFinder[L2],
                           ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                           refinements: GrammarRefinements[L, L2],
                           dummyFeatScale: HashFeature.Scale = HashFeature.Absolute(0),
                           trees: Traversable[Datum])(implicit hasWords: Has2[Datum, IndexedSeq[W]], hasTree: Has2[Datum, BinarizedTree[L]]) : IndexedLexFeaturizer[L, L2, W] = {

    val bilexFeatureIndex = bilexFeaturizer.featureIndex
    val wordFeatureIndex = wordFeaturizer.featureIndex
    val unaryFeatureIndex = unaryFeaturizer.featureIndex

    val progress = new ProgressLog(logger, trees.size, name="LexFeatures")

    val ruleFeatureIndex = ruleFeaturizer.index
    val bilexBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, bilexFeatureIndex, dummyFeatScale, "Bilex", includeLabelOnlyFeatures = true)
    val wordBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, wordFeatureIndex, dummyFeatScale, "Monolex", includeLabelOnlyFeatures = false)
    val unaryBuilder = new CrossProductIndex.Builder(ruleFeatureIndex, unaryFeatureIndex, dummyFeatScale, "Unary", includeLabelOnlyFeatures = true)
    for(ti <- trees) {
      val ruleSpec = ruleFeaturizer.anchor(hasWords.get(ti))
      val bilexSpec = bilexFeaturizer.anchor(hasWords.get(ti))
      val wordSpec = wordFeaturizer.anchor(hasWords.get(ti))
      val unarySpec = unaryFeaturizer.anchor(hasWords.get(ti))
      val words = hasWords.get(ti)
      val tree = ann(hasTree.get(ti), words)
      // returns head
      def rec(t: BinarizedTree[L2]):Int= t match {
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
          head
        case t@BinaryTree(a, b, c, span) =>
          val (leftHead,rightHead) = (rec(t.leftChild), rec(t.rightChild))
          val headIsLeft = headFinder.findHeadChild(t) == 0
          val (head, dep) = if(headIsLeft) leftHead -> rightHead else rightHead -> leftHead
          val r = BinaryRule[L2](a, b.label, c.label)
          val (ri, rref) = refinements.rules.indexAndLocalize(r)
          val bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          val split = t.splitPoint
          bilexBuilder.add(ruleSpec.featuresForBinaryRule(span.begin, split, span.end, ri, rref), bilexFeatures)
          head
      }
      rec(tree)
      progress.info(s"${bilexBuilder.size} bilex features; ${wordBuilder.size} word features; ${unaryBuilder.size} unary features")
    }

    val wfi = wordBuilder.result()
    val bfi = bilexBuilder.result()
    assert(bfi.includePlainLabelFeatures)
    val ufi = unaryBuilder.result()

    new IndexedLexFeaturizer(ruleFeaturizer,
      wordFeaturizer,
      unaryFeaturizer,
      bilexFeaturizer,
      refinements,
      wfi, ufi, bfi)
  }
}

case class LexModelFactory(@Help(text= "The kind of annotation to do on the refined grammar. Defaults to xbar.")
                           annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = Xbarize(),
                           @Help(text="Old weights to initialize with. Optional")
                           oldWeights: File = null,
                           @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                           dummyFeats: Double = 1.0,
                           @Help(text="How common must a feature be before we remember it?")
                           minFeatCutoff: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] with SafeLogging {
  type MyModel = LexModel[AnnotatedLabel, AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String])(implicit broker: CacheBroker) ={
    val trees = trainTrees.map(annotator)
    val (initLexicon, annBinaries, annUnaries) = GenerativeParser.extractCounts(trees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val wordIndex = Index(trainTrees.iterator.flatMap(_.words))

    val cFactory = constrainer

    val (xbarGrammar, xbarLexicon) = constrainer.grammar -> constrainer.lexicon
    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    val (wordFeaturizer, unaryFeaturizer, bilexFeaturizer) = {
      val dsl = new WordFeaturizer.DSL(initLexicon) with BilexicalFeaturizer.DSL with SurfaceFeaturizer.DSL
      import dsl._

      val wf = word + clss + shape + bigrams(word, 1) + bigrams(clss, 1)
      val offsets = clss(-1) + clss(1)
      var bilexF:BilexicalFeaturizer[String] = (
        withDistance(bilex(clss))
          + adaptSpanFeaturizer(spanShape)
        //+ bilex(word)
          + bilex(tagDict)
        + offsets(head)
        + offsets(dep)
        )

      val monolex = IndexedSeq(word, clss)

      bilexF = bilexF + monolex.map(_(head)).reduceLeft[BilexicalFeaturizer[String]](_ + _) + monolex.map(_(dep)).reduceLeft[BilexicalFeaturizer[String]](_ + _)

      (wf, word + shape + clss, bilexF)
    }

    val headFinder = HeadFinder.collins.lensed[AnnotatedLabel]

    val indexedWordFeaturizer = IndexedWordFeaturizer.fromData(wordFeaturizer, trees.map(_.words))
    val indexedUnaryFeaturizer = IndexedWordFeaturizer.fromData(unaryFeaturizer, trees.map(_.words))
    val indexedBilexicalFeaturizer = {
      val dependencyTrees = trees.map ( DependencyTree.fromTreeInstance[AnnotatedLabel, String](_, headFinder) )
      IndexedBilexicalFeaturizer.fromData(bilexFeaturizer, dependencyTrees)
    }


    def labelFeaturizer(l: AnnotatedLabel) = Set(l, l.baseAnnotatedLabel).toSeq
    def ruleFeaturizer(r: Rule[AnnotatedLabel]) = r match {
      case r@BinaryRule(a,b,c) =>
        val headIsLeft = headFinder.findHeadChild(r) == 0
        val dir = if(headIsLeft) AttachLeft else AttachRight
        Set(r, r.map(_.baseAnnotatedLabel), dir).toSeq
      case r@UnaryRule(a,b,c) =>
        Set(r, r.map(_.baseAnnotatedLabel)).toSeq
    }

    val featurizer = new ProductionFeaturizer[AnnotatedLabel, AnnotatedLabel, String](xbarGrammar, indexedRefinements,
      lGen=labelFeaturizer,
      rGen=ruleFeaturizer)

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: IndexedSeq[String]) = annotator(tree, words)

    type W = String
    type L = AnnotatedLabel
    val indexed =  IndexedLexFeaturizer.extract(featurizer,
      indexedBilexicalFeaturizer,
      indexedUnaryFeaturizer,
      indexedWordFeaturizer,
      headFinder,
      annotator,
      indexedRefinements,
      HashFeature.Relative(dummyFeats),
      trainTrees)

     logger.info(s"Num features: Indexed Features: ${indexed.index.size}")

    val bundle = new LexGrammarBundle(xbarGrammar,
      xbarLexicon,
      refGrammar,
      headFinder)

    val featureCounter = readWeights(oldWeights)

    new LexModel(bundle, reannotate, indexed, cFactory, featureCounter.get)
  }
}
