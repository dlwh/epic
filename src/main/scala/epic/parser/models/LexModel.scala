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
import annotations.TreeAnnotator
import java.io.File
import epic.parser._
import breeze.util._
import breeze.config.Help
import epic.lexicon.Lexicon
import epic.features._
import epic.parser.features._
import epic.constraints.{ChartConstraints, SpanConstraints}
import epic.util.{SafeLogging, CacheBroker, Arrays, Has2}
import epic.trees.UnaryTree
import epic.parser.features.RuleFeature
import epic.trees.TreeInstance
import epic.trees.NullaryTree
import epic.trees.annotations.StripAnnotations
import epic.parser.features.HeadFeature
import epic.trees.BinaryTree
import epic.parser.features.DepFeature
import epic.parser.projections.ConstraintCoreGrammarAdaptor
import com.typesafe.scalalogging.log4j.Logging
import epic.constraints.ChartConstraints.Factory

class LexModel[L, W](bundle: LexGrammarBundle[L, W],
                     reannotate: (BinarizedTree[L], IndexedSeq[W])=>BinarizedTree[L],
                     indexed: IndexedLexFeaturizer[L, W],
                     baseFactory: CoreGrammar[L, W],
                     coarse: BaseGrammar[L],
                     coarseLex: Lexicon[L, W],
                     initFeatureValue: Feature=>Option[Double]) extends ParserModel[L, W] with Serializable with ParserExtractable[L, W] {


  def accumulateCounts(d: TreeInstance[L, W], m: Marginal, accum: ExpectedCounts, scale: Double) {
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
      val headed = bundle.headFinder.annotateHeadIndices(reannotated)
      headed

    }
    new AnnotatedParserInference(indexed, ann _, gram, baseFactory)
  }

  type Inference = AnnotatedParserInference[L, W]

}

trait LexFeaturizer[L] extends Serializable {

  def featureIndex: Index[Feature]

  // Features for this rule with this head (and corresponding head child, as appropriate). For unaries.
  def featuresForHead(rule: Int): Array[Int]

  // Features for this rule with this dependent (and corresponding head child, as appropriate)
  def featuresForDep(rule: Int): Array[Int]

  // Features for the unlabeled attachment of these two words
  def featuresForBilex(head: Int, dep: Int): Array[Int]

  // all features for this attachment not captured by the above.
  def featuresForAttach(r: Int, head: Int, dep: Int): Array[Int]


  def featuresForTag(tag: Int): Array[Int]

  def featuresForRule(r: Int): Array[Int]
}



/**
 * Indexes and caches features for more efficient access.
 * Two kinds of features: features we observed in a gold tree (true Features), and features
 * we haven't. The latter are binned by hashcode into the higher-end of the feature index
 * @param f base featureizer
 * @tparam L Label
 * @tparam W Word
 */
class IndexedLexFeaturizer[L, W](f: LexFeaturizer[L],
                                 wordFeaturizer: IndexedWordFeaturizer[W],
                                 bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                                 labelIndex: Index[L],
                                 ruleIndex: Index[Rule[L]],
                                 wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                 bilexFeatureIndex: CrossProductIndex[Feature, Feature]) extends RefinedFeaturizer[L, W, Feature] with Serializable {

  val index = SegmentedIndex(wordFeatureIndex, bilexFeatureIndex)
  private val wordOffset = index.componentOffset(0)
  private val bilexOffset = index.componentOffset(1)

  def anchor(datum: IndexedSeq[W]):Spec = new Spec(datum)

  class Spec(val words: IndexedSeq[W]) extends Anchoring {


    private val fspec = f
    private val bilexSpec = bilexFeaturizer.anchor(words)
    private val wordSpec = wordFeaturizer.anchor(words)

    def length = words.length

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      indexedFeaturesForRuleHead(rule, unaryHeadIndex(ref))
    }

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      if(begin +1 == end)
        featuresForTag(tag, ref)
      else
        emptyArray
    }

    private val emptyArray = Array.empty[Int]

    def featuresForTag(tag: Int, head: Int): Array[Int] = {
      var rcache = wordCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](labelIndex.size, null:Array[Int], 2)
        wordCache(head) = rcache
      }
      var cache = rcache(tag)
      if(cache == null) {
        cache = wordFeatureIndex.crossProduct(fspec.featuresForTag(tag), wordSpec.featuresForWord(head), offset = wordOffset, usePlainLabelFeatures = false)
        rcache(tag) = cache
      }
      cache
    }


    def headIndex(ruleRef: Int) = {
      ruleRef / words.length
    }

    def depIndex(ruleRef: Int) = {
      ruleRef % words.length
    }

    def unaryHeadIndex(ref: Int) = {
      ref
    }


    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) =  {
      val head = headIndex(ref)
      val dep = depIndex(ref)

      //      var rcache = binaryCache(head)
      //      if(rcache eq null) {
      //        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size * words.size)
      //        binaryCache(head) = rcache
      //      }
      //      val i = rule * words.size + dep
      //      var cache = rcache(i)
      //      if(cache == null)  {
      val ruleHead = indexedFeaturesForRuleHead(rule, head)
      val ruleDep = indexedFeaturesForRuleDep(rule, dep)
      val spanFeatures: Array[Int] = bilexSpec.featuresForAttachment(head, dep)
      val bilex = indexedFeaturesForBilex(head, dep)
      val attach = bilexFeatureIndex.crossProduct(fspec.featuresForAttach(rule, head, dep), spanFeatures, offset = bilexOffset)

      //        cache = Arrays.concatenate(ruleHead, ruleDep, bilex, justRule)
      Arrays.concatenate(ruleHead, ruleDep, bilex, attach)
      //        rcache(i) = cache
      //      }
//      cache
    }

    // caches:
    // words
    val bilexCache = new Array[Array[Int]](words.length * words.length)
    // headIndex -> (ruleIndex) -> Array[Int]
    val headCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // depIndex -> (ruleIndex) -> Array[Int]
    val depCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // headIndex -> (depIndex x ruleIndex) -> Array[Int]
    // holds all features for attachment, uses other caches for faster computation
    val binaryCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // for tags. word -> tag -> Array[Int]
    val wordCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)

    private def indexedFeaturesForRuleHead(r: Int, w: Int) = {
      var rcache = headCache(w)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size)
        headCache(w) = rcache
      }
      var cache = rcache(r)
      if(cache == null)  {
        val surfFeatures = wordSpec.featuresForWord(w)
        cache = wordFeatureIndex.crossProduct(fspec.featuresForHead(r), surfFeatures, wordOffset)
        rcache(r) = cache
      }
      cache
    }

    private def indexedFeaturesForRuleDep(r: Int, w: Int) = {
      var rcache = depCache(w)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size)
        depCache(w) = rcache
      }
      var cache = rcache(r)
      if(cache == null)  {
        val surfFeatures = wordSpec.featuresForWord(w)
        cache = wordFeatureIndex.crossProduct(fspec.featuresForDep(r), surfFeatures, wordOffset, usePlainLabelFeatures = false)
        rcache(r) = cache
      }
      cache
    }

    private def indexWordPair(hw: Int, dw: Int) = {
      hw * words.size + dw
    }

    private def indexedFeaturesForBilex(hw: Int, dw: Int):Array[Int] = {
      val i = indexWordPair(hw, dw)
      var cache = bilexCache(i)
      if(cache == null) {
        val feats = fspec.featuresForBilex(hw, dw)
        val attFeats = bilexSpec.featuresForAttachment(hw, dw)
        cache = if (attFeats ne null) {
          bilexFeatureIndex.crossProduct(feats, attFeats, offset = bilexOffset, usePlainLabelFeatures = true)
        } else {
          Array.empty
        }
        bilexCache(i) = cache
      }
      cache
    }

  }
}

case class StandardLexFeaturizer[L](labelIndex: Index[L],
                                 ruleIndex: Index[Rule[L]],
                                 ruleFeatGen: Rule[L]=>IndexedSeq[Feature]) extends LexFeaturizer[L] {

  private val _featureIndex= Index[Feature]()
  def featureIndex : Index[Feature] = _featureIndex

  private val distanceBinner = DistanceBinner(Array(1,2,5,10),preserveDirection = false)

  private val ruleCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).map(_featureIndex.index(_)).toArray)
  private val attachCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => distanceBinner.binIds.toArray.map(df => Array(_featureIndex.index(LabelFeature(r.parent)), _featureIndex.index( DistFeature(df, r.parent)))))
  private val ruleHeadCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).toArray.map(f => _featureIndex.index(HeadFeature(f))))
  private val ruleDepCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).toArray.map(f => _featureIndex.index(DepFeature(f))))
  private val leftAttachFeatures = distanceBinner.binIds.map(new AttachLeft(_)).map(f => Array(f, AttachLeft).map(_featureIndex.index _))
  private val rightAttachFeatures = distanceBinner.binIds.map(new AttachRight(_)).map(f => Array(f, AttachRight).map(_featureIndex.index _))
  private val labelFeatures: Array[Array[Int]] = Encoder.fromIndex(labelIndex).tabulateArray(l => Array[Int](_featureIndex.index(LabelFeature(l))))

  def featuresForHead(rule: Int) = {
    ruleHeadCache(rule)
  }


  def featuresForDep(rule: Int) = {
    ruleDepCache(rule)
  }


  def featuresForBilex(head: Int, dep: Int) = {
//    rightAttachFeatures(0).take(1)
    val bin = distanceBinner.distanceBin(head, dep)
    if(head < dep) {
      rightAttachFeatures(bin)
    } else {
      leftAttachFeatures(bin)
    }
  }


    def featuresForAttach(r: Int, head: Int, dep: Int): Array[Int] = {
      attachCache(r)(distanceBinner.distanceBin(head,dep))
    }


    def featuresForTag(tag: Int) = {
      labelFeatures(tag)
    }

  def featuresForRule(r: Int): Array[Int] = ruleCache(r)
}

final class LexGrammar[L, W](val grammar: BaseGrammar[L],
                             val lexicon: Lexicon[L, W],
                             wordIndex: Index[W],
                             fi: IndexedLexFeaturizer[L, W],
                             weights: DenseVector[Double],
                             binaries: Array[Boolean],
                             leftRules: Array[Boolean],
                             rightRules: Array[Boolean]) extends RefinedGrammar[L, W] {
  def isHeadOnLeftForRule(r: Int) = leftRules(r)

  def isHeadOnRightForRule(r: Int) = rightRules(r)

  def anchor(sent: IndexedSeq[W]) = new Spec(sent)

  // refinement scheme:
  // binaryRule is (head * words.length + dep)
  // unaryRule is (head)
  // parent/leftchild/rightchild is (head)
  final class Spec(val words: IndexedSeq[W]) extends RefinedAnchoring[L, W] {
    override def annotationTag: Int = 1

    val grammar = LexGrammar.this.grammar
    val lexicon = LexGrammar.this.lexicon
    val indexed = words.map(wordIndex)
    private val f = fi.anchor(words)

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
      if(ref < begin || ref >= end) throw new Exception("Label refinement for lex parser (i.e. head word) must be in [begin,end)!")
      else dot(f.featuresForSpan(begin, end, label, ref))
    }

//    val bCache = new OpenAddressHashArray[Double](words.size * words.size * index.size, Double.NaN)
    val bCache = Array.fill(words.size)(new OpenAddressHashArray[Double](words.size * index.size, Double.NaN))
    val uCache = new OpenAddressHashArray[Double](words.size * index.size, Double.NaN)

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val cacheIndex = ref + words.size * rule
      val score = uCache(cacheIndex)

      if (!score.isNaN)
        score
      else {
        val score = dot(f.featuresForUnaryRule(begin, end, rule, ref))
        uCache(cacheIndex) = score
        score
      }
    }


    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val head = headIndex(ref)
      val dep = depIndex(ref)
      val cacheIndex = head + words.size * rule
      val score = bCache(dep)(cacheIndex)

      if (!score.isNaN)
        score
      else {
        val score = dot(f.featuresForBinaryRule(begin, split, end, rule, ref))
        bCache(dep)(cacheIndex) = score
        score
      }
    }

    def headIndex(ruleRef: Int) = f.headIndex(ruleRef)
    def depIndex(ruleRef: Int) = f.depIndex(ruleRef)

    def unaryHeadIndex(ref: Int) = {
      ref
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = Array.range(begin,end)

    def numValidRefinements(label: Int) = words.length

    def numValidRuleRefinements(rule: Int) = words.length * words.length

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      if(!binaries(rule)) {
        Array(parentRef:Int)
      } else if(isHeadOnLeftForRule(rule)) {
//        val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => leftChildRefinement(rule,x) == parentRef && rightChildRefinement(rule, x) > parentRef && rightChildRefinement(rule, x) < end)
        val result = new Array[Int](end - (parentRef+1))
        var ref = parentRef * words.length + parentRef + 1
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
//        val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => rightChildRefinement(rule,x) == parentRef && leftChildRefinement(rule, x) < parentRef && leftChildRefinement(rule, x) >= begin)
        val result = new Array[Int](parentRef - begin)
        var ref = parentRef * words.length + begin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += 1
        }
//        assert(x.toSet == result.toSet)
        result
      }
    }


    override def validRuleRefinementsGivenParent(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
      if(!binaries(rule)) {
        Array(parentRef:Int)
      } else if(isHeadOnLeftForRule(rule)) {
        // if the head is on the left, then the dependent
        // can be in Span(math.max(splitBegin, parentRef+1), end).
        // Further, if the parentRef is <= splitEnd, then
        // we can't even build this rule with this parent.
        // [begin....splitBegin....splitEnd...end)
        //  ^------parentRef------^
        // max:      ^------^----dep---------^
        //
        if(splitEnd <= parentRef) return Array.empty
        val firstPossibleStart = math.max(parentRef +1, splitBegin)
        val result = new Array[Int](end - firstPossibleStart)
        var ref = parentRef * words.length + firstPossibleStart
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
        // if the head is on the right, then the dependent
        // can be in (begin until math.min(splitEnd,parentRef))
        // Further, if the parentRef is <= splitBegin, then
        // we can't even build this rule with this parent.
        // [begin....splitBegin....splitEnd...end)
        //           ^--------parentRef------^
        //  ^-----------dep---^-----^ : min
        //
        if(splitBegin >= parentRef) return Array.empty
        val lastPossibleEnd = math.min(parentRef, splitEnd)
        val result = new Array[Int](lastPossibleEnd - begin)
        var ref = parentRef * words.length + begin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += 1
        }
        result
      }

    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin:Int, completionEnd: Int, rule: Int, lc: Int) = {
//      val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => leftChildRefinement(rule,x) == lc && rightChildRefinement(rule, x) >= split && rightChildRefinement(rule, x) < completionEnd)
      if(isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](completionEnd - split)
        var ref = lc * words.length + split
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
//        assert(result.toSet == x.toSet)
        result
      } else {
        val result = new Array[Int](completionEnd - split)
        var ref = split * words.length +lc
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
//        assert(result.toSet == x.toSet)
        result
      }
    }


    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
//      val x = Array.range(0,numValidRuleRefinements(rule)).filter(x => rightChildRefinement(rule,x) == childRef && leftChildRefinement(rule, x) >= completionBegin && leftChildRefinement(rule, x) < split)
      if(!isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](split - completionBegin)
        var ref = childRef * words.length + completionBegin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
//        assert(result.toSet == x.toSet)
        result
      } else {
        val result = new Array[Int](split - completionBegin)
        var ref = completionBegin * words.length + childRef
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
//        assert(result.toSet == x.toSet)
        result
      }
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      Array(childRef)
    }


    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      if(isHeadOnLeftForRule(rule)) headIndex(ruleRef)
      else depIndex(ruleRef)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      if(isHeadOnRightForRule(rule)) headIndex(ruleRef)
      else depIndex(ruleRef)
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      if(binaries(rule)) headIndex(ruleRef)
      else ruleRef
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      ruleRef
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      require(refA == refB, s"Parent head for rule ${grammar.index.get(r)} was '${words(refA)}' and child head was '${words(refB)}', but should be the same!" + words)
      refA
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val ret = if(isHeadOnLeftForRule(r)) {
        require(refA == refB)
        refA * words.length + refC
      } else {
        require(refA == refC)
        refA * words.length + refB
      }
      assert(headIndex(ret) == refA)
      ret
    }

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = grammar.indexedBinaryRulesWithParent(a)

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      if (isHeadOnLeftForRule(rule)) Array.range(begin, splitEnd)
      else Array.range(splitBegin, end)
    }


    def validLeftChildRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      Array.range(begin, splitEnd)
    }

    def validRightChildRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      Array.range(splitBegin, end)
    }
  }

}

case class LexGrammarBundle[L, W](baseGrammar: BaseGrammar[L],
                                  baseLexicon: Lexicon[L, W],
                                  headFinder: HeadFinder[L],
                                  wordIndex: Index[W]) { bundle =>
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

  def makeGrammar(fi: IndexedLexFeaturizer[L, W], weights: DenseVector[Double]): LexGrammar[L, W] = {
    new LexGrammar(baseGrammar, baseLexicon, wordIndex, fi, weights, binaries, leftRules, rightRules)
  }
}

object IndexedLexFeaturizer extends Logging {
  def extract[L, Datum, W](lexFeaturizer: LexFeaturizer[L],
                           bilexFeaturizer: IndexedBilexicalFeaturizer[W],
                           wordFeaturizer: IndexedWordFeaturizer[W],
                           headFinder: HeadFinder[L],
                           ruleIndex: Index[Rule[L]],
                           labelIndex: Index[L],
                           dummyFeatScale: HashFeature.Scale = HashFeature.Absolute(0),
                           trees: Traversable[Datum])(implicit hasWords: Has2[Datum, IndexedSeq[W]], hasTree: Has2[Datum, BinarizedTree[L]]) : IndexedLexFeaturizer[L, W] = {

    val bilexFeatureIndex = bilexFeaturizer.featureIndex
    val wordFeatureIndex = wordFeaturizer.featureIndex

    val lexFeatureIndex = lexFeaturizer.featureIndex
    val bilexBuilder = new CrossProductIndex.Builder(lexFeatureIndex, bilexFeatureIndex, dummyFeatScale, "Bilex", includeLabelOnlyFeatures = false)
    val wordBuilder = new CrossProductIndex.Builder(lexFeatureIndex, wordFeatureIndex, dummyFeatScale, "Monolex", includeLabelOnlyFeatures = true)
    for(ti <- trees) {
      val lexSpec = lexFeaturizer
      val bilexSpec = bilexFeaturizer.anchor(hasWords.get(ti))
      val wordSpec = wordFeaturizer.anchor(hasWords.get(ti))
      val words = hasWords.get(ti)
      val tree = hasTree.get(ti)
      // returns head
      def rec(t: BinarizedTree[L]):Int= t match {
        case NullaryTree(a, span) =>
          val aI = labelIndex(a)
          wordBuilder.add(lexSpec.featuresForTag(aI), wordSpec.featuresForWord(span.begin))
          span.begin
        case UnaryTree(a, b, chain, _) =>
          val h = rec(b)
          val r = ruleIndex(UnaryRule(a, b.label, chain))
          wordBuilder.add(lexSpec.featuresForHead(r), wordSpec.featuresForWord(h))
          h
        case t@BinaryTree(a, bt@Tree(b, _, _), Tree(c, _, _), span) =>
          val log = math.random < 1E-4
          val (leftHead,rightHead) = (rec(t.leftChild), rec(t.rightChild))
          val headIsLeft = headFinder.findHeadChild(t) == 0
          val (head, dep) = if(headIsLeft) leftHead -> rightHead else rightHead -> leftHead
          val r = ruleIndex(BinaryRule(a, b, c))
          wordBuilder.add(lexSpec.featuresForHead(r), wordSpec.featuresForWord(head))
          wordBuilder.add(lexSpec.featuresForDep(r), wordSpec.featuresForWord(dep))
          val bilexFeatures = bilexSpec.featuresForAttachment(head, dep)
          assert(bilexFeatures.nonEmpty, BinaryRule(a,b,c) + " " + words(head) + " " + words(dep))
          if(log) {
            logger.debug(BinaryRule(a,b,c) + " " + words(head) + " " + words(dep) + " " + Arrays.crossProduct(lexSpec.featuresForBilex(head, dep).map(lexFeatureIndex.get(_)), bilexFeatures.map(bilexFeatureIndex.get _))( _ -> _).toIndexedSeq)
          }
          bilexBuilder.add(lexSpec.featuresForBilex(head, dep), bilexFeatures)
          bilexBuilder.add(lexSpec.featuresForAttach(r, head, dep), bilexSpec.featuresForAttachment(head, dep))
          head
      }
      rec(tree)
    }

    val wfi = wordBuilder.result()
    val bfi = bilexBuilder.result()

    new IndexedLexFeaturizer(lexFeaturizer,
      wordFeaturizer,
      bilexFeaturizer,
      labelIndex,
      ruleIndex,
      wfi, bfi)
  }
}

case class LexModelFactory(baseParser: ParserParams.XbarGrammar,
                           @Help(text= "The kind of annotation to do on the refined grammar. Defaults to xbar.")
                           annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = StripAnnotations(),
                           @Help(text="Old weights to initialize with. Optional")
                           oldWeights: File = null,
                           @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                           dummyFeats: Double = 1.0,
                           @Help(text="How common must a feature be before we remember it?")
                           minFeatCutoff: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] with SafeLogging {
  type MyModel = LexModel[AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: CoreGrammar[AnnotatedLabel, String])(implicit broker: CacheBroker) ={
    val trees = trainTrees.map(annotator)
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)


    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trainTrees)
    val wordIndex = Index(trainTrees.iterator.flatMap(_.words))
    val summedCounts = sum(initLexicon, Axis._0)

    val cFactory = constrainer

    val minimalWordFeauturizer = new MinimalWordFeaturizer(summedCounts) + new TagDictionaryFeaturizer(initLexicon)
    val surfaceFeaturizer = new ContextWordFeaturizer(minimalWordFeauturizer) + new WordShapeFeaturizer(summedCounts)
    val indexedWordFeaturizer = IndexedWordFeaturizer.fromData(surfaceFeaturizer, trees.map(_.words))
    val indexedMinimalWordFeauturizer = IndexedWordFeaturizer.fromData(minimalWordFeauturizer, trees.map(_.words))
    val indexedBilexicalFeaturizer = IndexedBilexicalFeaturizer.fromData(indexedMinimalWordFeauturizer, indexedMinimalWordFeauturizer, trees.map{DependencyTree.fromTreeInstance[AnnotatedLabel, String](_, HeadFinder.collins)})

    def ruleGen(r: Rule[AnnotatedLabel]) = IndexedSeq(RuleFeature(r))

    val headFinder = HeadFinder.collins
    val feat = new StandardLexFeaturizer(xbarGrammar.labelIndex, xbarGrammar.index, ruleGen)

    type W = String
    type L = AnnotatedLabel
    val indexed =  IndexedLexFeaturizer.extract[AnnotatedLabel, TreeInstance[L, W], W](feat,
      indexedBilexicalFeaturizer,
      indexedWordFeaturizer,
      headFinder,
      xbarGrammar.index,
      xbarGrammar.labelIndex,
      HashFeature.Relative(dummyFeats),
      trees)

     // logger.info(s"Num features: Indexed Features: ${indexed.index.size} Label features: ${indexed.index.labelFeatureIndex.size} Surface: ${indexed.index.surfaceFeatureIndex.size} HashFeatures: ${indexed.index.numHashFeatures}")

    val bundle = new LexGrammarBundle[AnnotatedLabel, String](xbarGrammar,
      xbarLexicon,
      headFinder,
      wordIndex
    )

    val featureCounter = readWeights(oldWeights)

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: IndexedSeq[String]) = tree.map(_.baseAnnotatedLabel)
    val model = new LexModel[AnnotatedLabel, String](bundle, reannotate, indexed, cFactory, xbarGrammar, xbarLexicon, featureCounter.get)

    model


  }
}