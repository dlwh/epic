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
import epic.framework._
import breeze.collection.mutable.OpenAddressHashArray
import breeze.linalg._
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import epic.trees._
import annotations.{StripAnnotations, TreeAnnotator}
import java.io.File
import features._
import epic.parser._
import features.RuleFeature
import breeze.util._
import collection.mutable.ArrayBuffer
import breeze.config.Help
import collection.mutable

class LexModel[L, W](bundle: LexGrammarBundle[L, W],
                     reannotate: (BinarizedTree[L], Seq[W])=>BinarizedTree[L],
                     indexed: IndexedLexFeaturizer[L, W],
                     baseFactory: CoreGrammar[L, W],
                     coarse: BaseGrammar[L],
                     coarseLex: Lexicon[L, W],
                     initFeatureValue: Feature=>Option[Double]) extends ParserModel[L, W] with Serializable with ParserExtractable[L, W] {

  val featureIndex = indexed.index

  def initialValueForFeature(f: Feature) = initFeatureValue(f).getOrElse(0)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram = bundle.makeGrammar(indexed, weights)
    def ann(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[(L, Int)] = {
      val reannotated = reannotate(tree, words)
      val headed = bundle.headFinder.annotateHeadIndices(reannotated)
      headed

    }
    new AnnotatedParserInference(indexed, ann _, gram, baseFactory)
  }

  type Inference = AnnotatedParserInference[L, W]

  def emptyCounts = new epic.parser.ExpectedCounts(indexed.index)

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.counts)
  }

}

trait LexFeaturizer[L, W] extends Serializable {
  def anchor(words: Seq[W]): Anchoring

  /**
   * Specialization assumes that features are of several kinds, so that we can efficiently cache them.
   */
  trait Anchoring {

    // Features for this rule with this head (and corresponding head child, as appropriate). For unaries.
    def featuresForHead(rule: Int, head: Int): Array[Feature]

    // Features for this rule with this dependent (and corresponding head child, as appropriate)
    def featuresForDep(rule: Int, dep: Int): Array[Feature]

    // Features for the unlabeled attachment of these two words
    def featuresForBilex(head: Int, dep: Int): Array[Feature]

    // all features for this attachment not captured by the above.
    def featuresForAttach(r: Int, head: Int, dep: Int): Array[Feature]


    def featuresForTag(tag: Int, head: Int): Array[Feature]
  }

}



/**
 * Indexes and caches features for more efficient access.
 * Two kinds of features: features we observed in a gold tree (true Features), and features
 * we haven't. The latter are binned by hashcode into the higher-end of the feature index
 * @param f base featureizer
 * @param trueFeatureIndex index of featuers we saw in gold.
 * @param dummyFeatures how many buckets in the hash part of the feature vector
 * @tparam L Label
 * @tparam W Word
 */
class IndexedLexFeaturizer[L, W](f: LexFeaturizer[L, W],
                                 labelIndex: Index[L],
                                 ruleIndex: Index[Rule[L]],
                                 val trueFeatureIndex: Index[Feature],
                                 lowCountFeatures: Set[Feature],
                                 dummyFeatures: Int,
                                 useGlobalBinaryFeatureCache: Boolean = false) extends RefinedFeaturizer[L, W, Feature] with Serializable {
  def anchor(words: Seq[W]):Anchoring = new Spec(words)

  val (index:Index[Feature], lowCountFeature) = {
    val r: MutableIndex[Feature] = Index[Feature]()
    (trueFeatureIndex) foreach (r.index(_))
    val lowCount = r.index(LowCount)
    (0 until dummyFeatures) map {HashFeature(_)} foreach {r.index _}
    r -> lowCount
  }

  val globalBinaryFeatureCache = mutable.Map.empty[Seq[W], Array[OpenAddressHashArray[Array[Int]]]]

  case class Spec(words: Seq[W]) extends super.Anchoring {

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

    private val fspec = f.anchor(words)
    def featuresForTag(tag: Int, head: Int): Array[Int] = {
      var rcache = wordCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](labelIndex.size, null:Array[Int], 2)
        wordCache(head) = rcache
      }
      var cache = rcache(tag)
      if(cache == null) {
        cache = stripEncode(index, fspec.featuresForTag(tag, head))
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

      var rcache = binaryCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size * words.size)
        binaryCache(head) = rcache
      }
      val i = rule * words.size + dep
      var cache = rcache(i)
      if(cache == null)  {
        val ruleHead = indexedFeaturesForRuleHead(rule, head)
        val ruleDep = indexedFeaturesForRuleDep(rule, dep)
        val bilex = indexedFeaturesForBilex(head, dep)
        val brule = stripEncode(index, fspec.featuresForAttach(rule, head, dep))

        cache = new Array[Int](brule.length + ruleHead.length + ruleDep.length + bilex.length)
        var destPos = 0
        System.arraycopy(brule, 0,cache, destPos, brule.length)
        destPos += brule.length
        System.arraycopy(ruleHead, 0,cache, destPos, ruleHead.length)
        destPos += ruleHead.length
        System.arraycopy(ruleDep, 0,cache, destPos, ruleDep.length)
        destPos += ruleDep.length
        System.arraycopy(bilex, 0,cache, destPos, bilex.length)
        rcache(i) = cache
      }
      cache
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
    val binaryCache = globalBinaryFeatureCache.getOrElse(words, {
        val empty = new Array[OpenAddressHashArray[Array[Int]]](words.length)
        if (useGlobalBinaryFeatureCache) globalBinaryFeatureCache.put(words, empty)
        empty
      })
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
        cache = stripEncode(index, fspec.featuresForHead(r, w))
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
        cache = stripEncode(index, fspec.featuresForDep(r, w))
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
        cache = stripEncode(index, feats)
        bilexCache(i) = cache
      }
      cache
    }
  }

  private def stripEncode(index: Index[Feature], arr: Array[Feature], dest: Array[Int] = null, destStart: Int = 0) = {
    val res = if(dest eq null) new Array[Int](arr.length) else dest
    var i = destStart
    while( i < arr.length) {
      val fi = index(arr(i))
      if(fi >= 0) {
        res(destStart + i)  = fi
      } else if (lowCountFeatures.contains(arr(i))) {
        res(destStart + i) = lowCountFeature
      } else if(dummyFeatures > 0) {
        res(destStart + i) = (trueFeatureIndex.size + math.abs(arr(i).hashCode) % dummyFeatures)
      }
      i += 1
    }
    res
  }

}


case class StandardFeaturizer[L, W>:Null, P](wordIndex: Index[W],
                                             labelIndex: Index[L],
                                             ruleIndex: Index[Rule[L]],
                                             ruleFeatGen: Rule[L]=>IndexedSeq[Feature],
                                             featGen: (W) => IndexedSeq[P],
                                             tagFeatGen: ((Seq[W],Int)=>IndexedSeq[P])) extends LexFeaturizer[L, W] {

  def anchor(words: Seq[W]) = new Spec(words)


  // word parts are things like suffixes, etc.
  val wordPartIndex = Index[P]()

  // binned distances
  private def binDistance(dist2:Int) = {
    val dist = dist2.abs - 1
    if (dist >= 10) 3
    else if (dist >= 5) 2
    else if (dist >= 2) 1
    else 0
  }

  //////////////
  /// caches ///
  //////////////

  // wordIndex -> seq of feature indices
  val wordCache: Array[Array[Int]] = Encoder.fromIndex(wordIndex).tabulateArray(w => featGen(w).map(wordPartIndex.index(_)).toArray)
  // used by rule
  val ruleCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).toArray)

  def indexedFeaturesForWord(w: Int, ww: W): Array[Int] = {
    if(w >= 0) wordCache(w)
    else {
      featGen(ww).view.map(wordPartIndex).filter(_ >= 0).toArray
    }
  }

  final class Spec(words: Seq[W]) extends Anchoring {
    val indexed = words.map(wordIndex).toArray

    def featuresForHead(rule: Int, head: Int) = {
      val rc = ruleCache(rule)
      val ifw = indexedFeaturesForWord(indexed(head), words(head))
      def join(f: Feature, part: Int) = HeadFeature(f, wordPartIndex.get(part))
      crossProduct(rc, ifw, join _)
    }


    def featuresForDep(rule: Int, dep: Int) = {
      val rc = ruleCache(rule)
      val ifw = indexedFeaturesForWord(indexed(dep), words(dep))
      def join(f: Feature, part: Int) = DepFeature(f, wordPartIndex.get(part))
      crossProduct(rc, ifw, join _)
    }


    def featuresForBilex(head: Int, dep: Int) = {
      val ifw1 = indexedFeaturesForWord(indexed(head), words(head))
      val ifw2 = indexedFeaturesForWord(indexed(dep), words(dep))
      val dir = if(head < dep) 'Right else 'Left
      val binned = binDistance(head - dep)

      val arr = new Array[Feature](ifw1.length * ifw2.length * 2)
      var t = 0
      var i = 0
      while (i < ifw1.length) {
        var j = 0
        val b = ifw1(i)
        while (j < ifw2.length) {
          val f = ifw2(j)
          arr(t) = BilexicalFeature(wordPartIndex.get(b), wordPartIndex.get(f), dir)
          arr(t + 1) = DistFeature(binned, arr(t))
          t += 2
          j += 1
        }
        i += 1
      }

      arr
    }


    def featuresForAttach(r: Int, head: Int, dep: Int): Array[Feature] = {
      val f = for(rf <- ruleCache(r)) yield DistFeature(binDistance(head-dep), rf):Feature
//      val rc = ruleCache(r)
//      val ah = indexedFeaturesForWord(indexed(head), words(head)).take(2)
//      val ad = indexedFeaturesForWord(indexed(head), words(head)).take(2)
//
//      val f2 = for(rf <- rc; h <- ah; d <- ad) yield {
//        HeadDepFeature(rf, h, d)
//      }
//      f ++ f2
      f ++ ruleCache(r)
    }


    def featuresForTag(tag: Int, head: Int) = {
      (for(f <- tagFeatGen(words, head)) yield TagFeature(labelIndex.get(tag), f):Feature).toArray
    }

    private def crossProduct(rc: Array[Feature], ifw: Array[Int], join: (Feature, Int) => Feature): Array[Feature] = {
      val arr = new Array[Feature](rc.length * ifw.length)
      var t = 0
      var i = 0
      while (i < rc.length) {
        var j = 0
        val b = rc(i)
        while (j < ifw.length) {
          val f = ifw(j)
          arr(t) = join(b, f)
          t += 1
          j += 1
        }
        i += 1
      }

      arr
    }
  }
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

  def isRightRule(r: Int) = rightRules(r)

  def anchor(sent: Seq[W]) = new Spec(sent)

  // refinement scheme:
  // binaryRule is (head * words.length + dep)
  // unaryRule is (head)
  // parent/leftchild/rightchild is (head)
  final class Spec(val words: Seq[W]) extends RefinedAnchoring[L, W] {
    override def annotationTag: Int = 1

    val grammar = LexGrammar.this.grammar
    val lexicon = LexGrammar.this.lexicon
    val indexed = words.map(wordIndex)
    private val f = fi.anchor(words)
    val indexedValidTags: Seq[Array[Int]] = words.map(lexicon.tagsForWord(_)).map(_.map(labelIndex).toArray)

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

    def headIndex(ruleRef: Int) = {
      ruleRef / words.length
    }

    def depIndex(ruleRef: Int) = {
      ruleRef % words.length
    }

    def spanHeadIndex(ref: Int) = {
      ref
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = Array.range(begin,end)

    def numValidRefinements(label: Int) = words.length

    def numValidRuleRefinements(rule: Int) = words.length * words.length

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      if(!binaries(rule)) {
        Array(parentRef:Int)
      } else if(isHeadOnLeftForRule(rule)) {
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
        val result = new Array[Int](parentRef - begin)
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
      if(isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](completionEnd - completionBegin)
        var ref = lc * words.length + completionBegin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
        val result = new Array[Int](completionEnd - completionBegin)
        var ref = completionBegin * words.length +lc
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
        result
      }
    }


    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      if(!isHeadOnLeftForRule(rule)) {
        val result = new Array[Int](completionEnd - completionBegin)
        var ref = childRef * words.length + completionBegin
        var i = 0
        while(i < result.length) {
          result(i) = ref
          ref += 1
          i += 1
        }
        result
      } else {
        val result = new Array[Int](completionEnd - completionBegin)
        var ref = completionBegin * words.length + childRef
        var i = 0
        while(i < result.length) {
          result(i) = ref
          i += 1
          ref += words.length
        }
        result
      }
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      Array(childRef)
    }

    def validTagsFor(pos: Int) = {
      indexedValidTags(pos)
    }


    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      if(isHeadOnLeftForRule(rule)) headIndex(ruleRef)
      else depIndex(ruleRef)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      if(isRightRule(rule)) headIndex(ruleRef)
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
      if(isHeadOnLeftForRule(r)) {
        require(refA == refB)
        refA * words.length + refC
      } else {
        require(refA == refC)
        refA * words.length + refB
      }
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

object IndexedLexFeaturizer {
  def extract[L, W](featurizer: LexFeaturizer[L, W],
                    headFinder: HeadFinder[L],
                    ruleIndex: Index[Rule[L]],
                    labelIndex: Index[L],
                    dummyFeatScale: Double,
                    minFeatCutoff: Int,
                    trees: Traversable[TreeInstance[L, W]]): IndexedLexFeaturizer[L, W] = {

    def add(ctr: Counter[Feature, Int], feats: Array[Feature]) {
      for (f <- feats) {
        ctr(f) += 1
      }
    }
    val goldFeatures = trees.par.aggregate(null: Counter[Feature, Int])( {(feats, ti) =>
      val set = if(feats eq null) Counter[Feature, Int]() else feats
      val spec = featurizer.anchor(ti.words)
      // returns head
      def rec(t: BinarizedTree[L]):Int= t match {
        case NullaryTree(a, span) =>
          val aI = labelIndex(a)
          add(set, spec.featuresForTag(aI, span.start))
          span.start
        case UnaryTree(a, b, chain, _) =>
          val h = rec(b)
          val r = ruleIndex(UnaryRule(a, b.label, chain))
          add(set, spec.featuresForHead(r, h))
          h
        case t@BinaryTree(a, bt@Tree(b, _, _), Tree(c, _, _), span) =>
          val childHeads = IndexedSeq(rec(t.leftChild), rec(t.rightChild))
          val headIsLeft = headFinder.findHeadChild(t) == 0
          val (head, dep) = if(headIsLeft) childHeads(0) -> childHeads(1) else childHeads(1) -> childHeads(0)
          val r = ruleIndex(BinaryRule(a, b, c))
          add(set, spec.featuresForHead(r, head))
          add(set, spec.featuresForDep(r, dep))
          add(set, spec.featuresForBilex(head, dep))
          add(set, spec.featuresForAttach(r, head, dep))
          head
      }
      rec(ti.tree)
      set
    }, {(a, b) => a += b})


    val goldFeatureIndex = Index[Feature]()
    val lowCountFeatures = collection.mutable.Set[Feature]()
    for( (f, v) <- goldFeatures.activeIterator) {
      if(v >= minFeatCutoff) {
        goldFeatureIndex.index(f)
      } else {
        lowCountFeatures += f
      }

    }

    new IndexedLexFeaturizer(featurizer, labelIndex, ruleIndex, goldFeatureIndex, Set.empty ++ lowCountFeatures, (goldFeatureIndex.size * dummyFeatScale).toInt)
  }
}

class SimpleWordShapeGen[L](tagWordCounts: Counter2[L, String, Double],
                            counts: Counter[String, Double], noShapeThreshold: Int = 100, minCountThreshold: Int = 5) extends (String=>IndexedSeq[String]) with Serializable {
  def apply(w: String) = {
    if(counts(w) > noShapeThreshold) {
      ArrayBuffer(w, ("T-"+tagWordCounts(::, w).argmax))
    } else {
      val buf = ArrayBuffer[String](//IndicatorFeature(w),
        EnglishWordClassGenerator(w),
        WordShapeGenerator(w)
      )
      if(counts(w) > minCountThreshold) {
        buf += w
      }

      if(counts(w) > 0) {
        buf += ("T-"+tagWordCounts(::, w).argmax)
      }
      //      if(w.length > 5) {
      //        buf += w.substring(w.length-3)
      //        buf += w.substring(w.length-2)
      //      }
      buf
    }

  }


}

case class LexModelFactory(baseParser: ParserParams.XbarGrammar,
                           constraints: ParserParams.Constraints[String],
                           @Help(text= "The kind of annotation to do on the refined grammar. Defaults to ~KM2003")
                           annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = StripAnnotations(),
                           @Help(text="Old weights to initialize with. Optional")
                           oldWeights: File = null,
                           @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                           dummyFeats: Double = 0.5,
                           @Help(text="How common must a feature be before we remember it?")
                           minFeatCutoff: Int = 1) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = LexModel[AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val trees = trainTrees.map(annotator)
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trainTrees)
    val wordIndex = Index(trainTrees.iterator.flatMap(_.words))
    val summedCounts = sum(initLexicon, Axis._0)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
    val tagShapeGen = new TagAwareWordShapeFeaturizer(initLexicon)

    val lexicon:Lexicon[AnnotatedLabel, String] = initLexicon

    val baseFactory = RefinedGrammar.generative(xbarGrammar,
      xbarLexicon, initBinaries, initUnaries, initLexicon)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

    def ruleGen(r: Rule[AnnotatedLabel]) = IndexedSeq(RuleFeature(r))
    def validTag(w: String) = lexicon.tagsForWord(w).toArray

    val headFinder = HeadFinder.collins
    val feat = new StandardFeaturizer(wordIndex,
    xbarGrammar.labelIndex,
    xbarGrammar.index,
    ruleGen,
    shapeGen,
    { (w:Seq[String], pos: Int) => tagShapeGen.featuresFor(w, pos)})

    val indexed =  IndexedLexFeaturizer.extract[AnnotatedLabel, String](feat,
      headFinder,
      xbarGrammar.index,
      xbarGrammar.labelIndex,
      dummyFeats,
      minFeatCutoff,
      trees)

    val bundle = new LexGrammarBundle[AnnotatedLabel, String](xbarGrammar,
      xbarLexicon,
      headFinder,
      wordIndex
    )

    val featureCounter = readWeights(oldWeights)

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    val model = new LexModel[AnnotatedLabel, String](bundle, reannotate, indexed, cFactory, xbarGrammar, xbarLexicon, {featureCounter.get(_)})

    model


  }
}