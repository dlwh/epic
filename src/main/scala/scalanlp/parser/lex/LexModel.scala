package scalanlp.parser
package lex

import scalanlp.optimize.FirstOrderMinimizer.OptParams
import java.io.File
import scalanlp.parser.ParseEval.Statistics
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalala.tensor.Counter2
import scalala.tensor.mutable.Counter
import scalanlp.collection.mutable.OpenAddressHashArray
import scalala.tensor.dense.DenseVector
import collection.immutable.BitSet
import scalanlp.trees._
import epic._
import features._
import projections.GrammarProjections
import scalanlp.util._
import scalala.tensor.::
import scalanlp.optimize.{RandomizedGradientCheckingFunction, BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}
import scalala.tensor.sparse.SparseVector
import scalanlp.tensor.sparse.OldSparseVector
import collection.mutable.{ArrayBuilder, ArrayBuffer}
import java.util.Arrays

trait LexFeaturizer[L, W] extends Serializable {
  def specialize(words: Seq[W]):Specialization

  /**
   * Specialization assumes that features are of several kinds, so that we can efficiently cache them.
   */
  trait Specialization {

    // Features for this rule with this head (and corresponding head child, as appropriate). For unaries.
    def featuresForHead(rule: Int, head: Int): Array[Feature]
    // Features for this rule with this dependent (and corresponding head child, as appropriate)
    def featuresForDep(rule: Int, dep: Int): Array[Feature]
    // Features for the unlabeled attachment of these two words
    def featuresForBilex(head: Int, dep: Int):Array[Feature]
    // all features for this attachment not captured by the above.
    def featuresForAttach(r: Int, head: Int, dep: Int):Array[Feature]


    def featuresForTag(tag: Int, head: Int): Array[Feature]
  }
}

case class HashFeature(hashBucket: Int) extends Feature
case object LowCount extends Feature

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
class IndexedFeaturizer[L,W](f: LexFeaturizer[L,W],
                             labelIndex: Index[L],
                             ruleIndex: Index[Rule[L]],
                             val trueFeatureIndex: Index[Feature],
                             lowCountFeatures: Set[Feature],
                             dummyFeatures: Int) extends Serializable {
  def specialize(words: Seq[W]):Specialization = new Specialization(words)

  val (featureIndex:Index[Feature],lowCountFeature) = {
    val r: MutableIndex[Feature] = Index[Feature]()
    (trueFeatureIndex) foreach (r.index(_))
    val lowCount = r.index(LowCount)
    (0 until dummyFeatures) map {HashFeature(_)} foreach {r.index _}
    r -> lowCount
  }


  case class Specialization(words: Seq[W]) {
    private val fspec = f.specialize(words)
    def featuresForUnary(rule: Int, head: Int): Array[Int] = indexedFeaturesForRuleHead(rule, head)
    def featuresForTag(tag: Int, head: Int): Array[Int] = {
      var rcache = wordCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](labelIndex.size)
        wordCache(head) = rcache
      }
      val index = tag
      var cache = rcache(index)
      if(cache == null) {
        cache = stripEncode(featureIndex, fspec.featuresForTag(tag, head))
        rcache(index) = cache
      }
      cache
    }
    def featuresForBinary(rule: Int, head: Int, dep: Int): Array[Int] = {
      var rcache = binaryCache(head)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size * words.size)
        headCache(head) = rcache
      }
      val index = rule * words.size + dep
      var cache = rcache(index)
      if(cache == null)  {
        val ruleHead = indexedFeaturesForRuleHead(rule,head)
        val ruleDep = indexedFeaturesForRuleDep(rule,dep)
        val bilex = indexedFeaturesForBilex(head,dep)
        val brule = stripEncode(featureIndex, fspec.featuresForAttach(rule, head, dep))

        cache = new Array[Int](brule.length + ruleHead.length + ruleDep.length + bilex.length)
        var destPos = 0
        System.arraycopy(brule,0,cache,destPos,brule.length)
        destPos += brule.length
        System.arraycopy(ruleHead,0,cache,destPos,ruleHead.length)
        destPos += ruleHead.length
        System.arraycopy(ruleDep,0,cache,destPos,ruleDep.length)
        destPos += ruleDep.length
        System.arraycopy(bilex,0,cache,destPos,bilex.length)
        rcache(index) = cache
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
    val binaryCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)
    // for tags. word -> tag -> Array[Int]
    val wordCache = new Array[OpenAddressHashArray[Array[Int]]](words.length)

    private def indexedFeaturesForRuleHead(r: Int, w: Int) = {
      var rcache = headCache(w)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size)
        headCache(w) = rcache
      }
      val index = r
      var cache = rcache(index)
      if(cache == null)  {
        cache = stripEncode(featureIndex, fspec.featuresForHead(r, w))
        rcache(index) = cache
      }
      cache
    }

    private def indexedFeaturesForRuleDep(r: Int, w: Int) = {
      var rcache = depCache(w)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](ruleIndex.size)
        depCache(w) = rcache
      }
      val index = r
      var cache = rcache(index)
      if(cache == null)  {
        cache = stripEncode(featureIndex, fspec.featuresForDep(r, w))
        rcache(index) = cache
      }
      cache
    }

    private def indexWordPair(hw: Int, dw: Int) = {
      hw * words.size + dw
    }

    private def indexedFeaturesForBilex(hw: Int, dw: Int):Array[Int] = {
      val index = indexWordPair(hw,dw)
      var cache = bilexCache(index)
      if(cache == null) {
        val feats = fspec.featuresForBilex(hw, dw)
        cache = stripEncode(featureIndex,feats)
        bilexCache(index) = cache
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

case class HeadFeature[P](r: Feature, head: P) extends Feature

case class DepFeature[P](r: Feature, dep: P) extends Feature
case class HeadDepFeature[P](r: Feature, head: P, dep: P) extends Feature

case class BilexicalFeature[W](head: W, dep: W, dir: Symbol) extends Feature
case class TagFeature[L,W](tag: L, dep: W) extends Feature
case class DistFeature(dist: Int, f: Feature) extends Feature
case class PartFeature[P](feature: Feature, part: P) extends Feature

case class StandardFeaturizer[L, W>:Null, P](wordIndex: Index[W],
                                             labelIndex: Index[L],
                                             ruleIndex: Index[Rule[L]],
                                             ruleFeatGen: Rule[L]=>IndexedSeq[Feature],
                                             featGen: (W) => IndexedSeq[P],
                                             tagFeatGen: (W=>IndexedSeq[P])) extends LexFeaturizer[L, W] {

  def specialize(words: Seq[W]) = new Spec(words)


  // word parts are things like suffixes, etc.
  val wordPartIndex = Index[P]()

  // binned distances
  val distBins = Array(0,1,5,10)
  val minimums = Array(1,2,5,10)
  val numDistBins = distBins.length

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
  // these are features used by the tagger
  val tagCache = Encoder.fromIndex(wordIndex).tabulateArray(w => tagFeatGen(w).map(wordPartIndex.index(_)).toArray)
  // used by rule
  val ruleCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).toArray)

  def indexedFeaturesForWord(w: Int, ww: W): Array[Int] = {
    if(w >= 0) wordCache(w)
    else {
      featGen(ww).view.map(wordPartIndex).filter(_ >= 0).toArray
    }
  }

  def indexedTagFeaturesForWord(w: Int, ww: W): Array[Int] = {
    if(w >= 0) tagCache(w)
    else {
      tagFeatGen(ww).view.map(wordPartIndex).filter(_ >= 0).toArray
    }
  }

  final class Spec(words: Seq[W]) extends Specialization {
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


    def featuresForAttach(r: Int, head: Int, dep: Int) = {
      val f = for(rf <- ruleCache(r)) yield DistFeature(binDistance(head-dep),rf):Feature
//      val rc = ruleCache(r)
//      val ah = indexedFeaturesForWord(indexed(head), words(head)).take(2)
//      val ad = indexedFeaturesForWord(indexed(head), words(head)).take(2)
//
//      val f2 = for(rf <- rc; h <- ah; d <- ad) yield {
//        HeadDepFeature(rf, h, d)
//      }
//      f ++ f2
      f
    }


    def featuresForTag(tag: Int, head: Int) = {
      for(f <- indexedTagFeaturesForWord(indexed(head), words(head))) yield TagFeature(labelIndex.get(tag), wordPartIndex.get(f)):Feature
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

case class LexGrammarBundle[L,W](baseGrammar: Grammar[L],
                                 tags: IndexedSeq[L],
                                 headFinder: HeadFinder[L],
                                 wordIndex: Index[W],
                                 validTags: W=>Array[L]) { bundle =>
  val bg = baseGrammar
  val leftRules = new Array[Boolean](bg.index.size)
  val rightRules = new Array[Boolean](bg.index.size)

  for( (rule@BinaryRule(a,b,c),r) <- bg.index.iterator.zipWithIndex) {
    val headChild = headFinder.findHeadChild(rule)
    if(headChild == 0) {
      leftRules(r) = true
    } else {
      rightRules(r) = true
    }
  }

  def makeGrammar(fi: IndexedFeaturizer[L, W], weights: DenseVector[Double]): LexGrammar[L, W] = {
    val wi = wordIndex
    new LexGrammar[L, W] {
      final val index = bg.index
      def labelIndex = bg.labelIndex
      def wordIndex = wi

      val indexedRules = bg.indexedRules

      def maxNumBinaryRulesForParent = bg.maxNumBinaryRulesForParent

      def ruleIndex(a: Int, b: Int, c: Int) = bg.ruleIndex(a, b, c)

      def ruleIndex(a: Int, b: Int) = bg.ruleIndex(a, b)

      def indexedBinaryRulesWithParent(l: Int) = bg.indexedBinaryRulesWithParent(l)

      def indexedUnaryRulesWithChild(l: Int) = bg.indexedUnaryRulesWithChild(l)

      def indexedUnaryRulesWithParent(l: Int) = bg.indexedUnaryRulesWithParent(l)

      val tags = bundle.tags
      val indexedTags = BitSet() ++ tags.map(bg.labelIndex)

      def isLeftRule(r: Int) = leftRules(r)

      def isRightRule(r: Int) = rightRules(r)

      def specialize(sent: Seq[W]) = new Specialization(sent)

      final class Specialization(val words: Seq[W]) extends super.Specialization {
        val indexed = words.map(wordIndex)
        val f = fi.specialize(words)
        val indexedValidTags = words.map(validTags).map(_.map(labelIndex))



        //        override def finalize() {
        //          println("bcache desnity: " + bCache.activeSize * 1.0 / bCache.size)
        //          println("uCache density: " + uCache.activeSize * 1.0 / bCache.size)
        //
        //        }

        private def dot(features: Array[Int]) = {
          var i = 0
          var score = 0.0
          while(i < features.length) {
            score += weights(features(i))
            i += 1
          }
          score
        }

        def tagScores(head: Int) = {
          val sv = new OldSparseVector(labelIndex.size,Double.NegativeInfinity,indexedValidTags(head).size)
          for (l <- indexedValidTags(head)) {
            sv(l) = dot(f.featuresForTag(l, head))
          }
          sv
        }

        val bCache = new OpenAddressHashArray[Double](words.size * words.size * index.size, Double.NaN, words.size * index.size)
        val uCache = new OpenAddressHashArray[Double](words.size * index.size, Double.NaN, words.size * index.size)

        def scoreLeftComplement(rule: Int, head: Int, leftHead: Int): Double = {
          val cacheIndex = head + words.size * (leftHead + words.size * rule)
          val score = bCache(cacheIndex)

          if (!score.isNaN)
            score
          else {
            val score = dot(f.featuresForBinary(rule, head, leftHead))
            bCache(cacheIndex) = score
            score
          }
        }

        def scoreRightComplement(rule: Int, head: Int, rightHead: Int): Double = {
          val cacheIndex = head + words.size * (rightHead + words.size * rule)
          val score = bCache(cacheIndex)

          if (!score.isNaN)
            score
          else {
            val score = dot(f.featuresForBinary(rule, head, rightHead))
            bCache(cacheIndex) = score
            score
          }
        }


        def scoreUnary(rule: Int, head: Int): Double = {
          val cacheIndex = head + words.size * rule
          val score = uCache(cacheIndex)

          if (!score.isNaN)
            score
          else {
            val score = dot(f.featuresForUnary(rule, head))
            uCache(cacheIndex) = score
            score
          }
        }

      }
    }

  }
}

object IndexedFeaturizer {
  def extract[L, W](featurizer: LexFeaturizer[L, W],
                    headFinder: HeadFinder[L],
                    ruleIndex: Index[Rule[L]],
                    labelIndex: Index[L],
                    dummyFeatScale: Double,
                    minFeatCutoff: Int,
                    trees: Traversable[TreeInstance[L, W]]): IndexedFeaturizer[L, W] = {

    def add(ctr: Counter[Feature,Int], feats: Array[Feature]) {
      for (f <- feats) {
        ctr(f) += 1
      }
    }
    val goldFeatures = trees.par.aggregate(null: Counter[Feature,Int])( {(feats, ti) =>
      val set = if(feats eq null) Counter[Feature,Int]() else feats
      val spec = featurizer.specialize(ti.words)
      // returns head
      def rec(t: BinarizedTree[L]):Int= t match {
        case n@NullaryTree(a) =>
          val aI = labelIndex(a)
          add(set,spec.featuresForTag(aI,n.span.start))
          n.span.start
        case UnaryTree(a, b) =>
          val h = rec(b)
          val r = ruleIndex(UnaryRule(a, b.label))
          add(set,spec.featuresForHead(r,h))
          h
        case t@BinaryTree(a, bt@Tree(b, _), Tree(c, _)) =>
          val childHeads = IndexedSeq(rec(t.leftChild), rec(t.rightChild))
          val headIsLeft = headFinder.findHeadChild(t) == 0
          val (head, dep) = if(headIsLeft) childHeads(0) -> childHeads(1) else childHeads(1) -> childHeads(0)
          val r = ruleIndex(BinaryRule(a, b, c))
          add(set,spec.featuresForHead(r,head))
          add(set,spec.featuresForDep(r,dep))
          add(set,spec.featuresForBilex(head, dep))
          add(set,spec.featuresForAttach(r, head, dep))
          head
      }
      rec(ti.tree)
      set
    }, {(a,b) => a += b})
    

    val goldFeatureIndex = Index[Feature]()
    val lowCountFeatures = collection.mutable.Set[Feature]()
    for( (f,v) <- goldFeatures.pairsIteratorNonZero) {
      if(v >= minFeatCutoff) {
        goldFeatureIndex.index(f)
      } else {
        lowCountFeatures += f
      }

    }

    new IndexedFeaturizer(featurizer, labelIndex, ruleIndex, goldFeatureIndex, Set.empty ++ lowCountFeatures, (goldFeatureIndex.size * dummyFeatScale).toInt)
  }
}



class LexModel[L, W](bundle: LexGrammarBundle[L,W],
                     reannotate: (BinarizedTree[L],Seq[W])=>BinarizedTree[L],
                     indexed: IndexedFeaturizer[L,W],
                     coarse: ChartBuilder[ParseChart, L, W],
                     initFeatureValue: Feature=>Option[Double]) extends Model[TreeInstance[L, W]] with Serializable with ParserExtractable[L,W] {

  def extractParser(weights: DenseVector[Double]) = {
    val inf = inferenceFromWeights(weights)
    new LexMaxVChartParser(coarse.grammar, coarse.lexicon, GrammarProjections.identity(coarse.grammar), inf.builder)
  }

  val featureIndex = indexed.featureIndex


  import bundle._

  def initialValueForFeature(f: Feature) = initFeatureValue(f).getOrElse(0)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram: LexGrammar[L, W] = bundle.makeGrammar(indexed, weights)
    val builder = new LexCKYChartBuilder(coarse.root, gram, ParseChart.logProb)
    new LexInference(reannotate, coarse, builder, indexed, headFinder)
  }

  type Inference = LexInference[L, W]
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]

  def emptyCounts = new ExpectedCounts(numFeatures)

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.features)
  }

}

case class LexInference[L, W](reannotate: (BinarizedTree[L],Seq[W])=>BinarizedTree[L],
                              coarseParser: ChartBuilder[ParseChart, L, W],
                              builder: LexCKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                              featurizer: IndexedFeaturizer[L,W],
                              headFinder: HeadFinder[L]) extends ProjectableInference[TreeInstance[L, W], SpanScorerFactor[L, W]] {
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]
  type Marginal = LexChartPair[ParseChart.LogProbabilityParseChart, L, W]

  def marginal(v: TreeInstance[L, W], aug: SpanScorerFactor[L, W]) = {
    val r = builder.buildCharts(v.words, aug.scorer)
    r -> r.partition
  }

  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: SpanScorerFactor[L, W]) = {
    val root_score = marg.partition
    val ec = new LexInsideOutside(featurizer, builder).expectedCounts(marg.spec, marg.inside,
      marg.outside, root_score, marg.scorer)
    ec
  }

  def goldCounts(ti: TreeInstance[L, W], augment: SpanScorerFactor[L, W]) = {
    val g = builder.grammar
    val spec = g.specialize(ti.words)
    val fspec = featurizer.specialize(ti.words)
    val counts = new ExpectedCounts(featurizer.featureIndex.size)
    val words = ti.words
    var score = 0.0
    // rec returns head index
    def rec(t: BinarizedTree[L]):Int= t match {
      case n@NullaryTree(a) =>
        val aI = g.labelIndex(a)
        addMultiple(counts.features, fspec.featuresForTag(aI,n.span.start), 1.0)
        score += spec.tagScores(n.span.start)(aI) + ti.spanScorer.scoreSpan(t.span.start, t.span.end, aI)
        assert(!score.isInfinite)
        n.span.start
      case UnaryTree(a, b) =>
        val h = rec(b)
        val r = g.index(UnaryRule(a, b.label))
        addMultiple(counts.features, fspec.featuresForUnary(r,h), 1.0)
        score += ( spec.scoreUnary(r, h)
          //          + augment.scoreUnaryRule(t.span.start, t.span.end, r)
          )
        h
      case t@BinaryTree(a, bt@Tree(b, _), Tree(c, _)) =>
        val childHeads = IndexedSeq(rec(t.leftChild), rec(t.rightChild))
        val headIsLeft = headFinder.findHeadChild(t) == 0
        val (head, dep) = if(headIsLeft) childHeads(0) -> childHeads(1) else childHeads(1) -> childHeads(0)
        val r = g.index(BinaryRule(a, b, c))
        if(headIsLeft) {
          score += ( spec.scoreRightComplement(r, head, dep)
            //            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
            //            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
        } else {
          score += ( spec.scoreLeftComplement(r, head, dep)
            //            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
            //            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
        }
        addMultiple(counts.features, fspec.featuresForBinary(r,head,dep), 1.0)
        if(headIsLeft) childHeads(0) else childHeads(1)
    }
    rec(reannotate(ti.tree,ti.words))
    assert(!score.isInfinite)
    counts.logProb = score
    counts
  }


  def baseAugment(v: TreeInstance[L,W]) = new SpanScorerFactor(zeroParser, v.words, v.spanScorer)

  val projector = new LexRuleProjector(coarseParser.grammar, builder, GrammarProjections.identity(coarseParser.grammar), Double.NegativeInfinity)
  def project(v: TreeInstance[L, W], m: Marginal, oldAugment: SpanScorerFactor[L, W]) = {
    val scorer = projector.createSpanScorer(m)
    oldAugment.copy(scorer = scorer)
  }

  private val zeroParser = new CKYChartBuilder(coarseParser.root,
    new ZeroLexicon(coarseParser.lexicon),
    Grammar.zero(coarseParser.grammar),
    ParseChart.logProb)

  private def addMultiple(vec: DenseVector[Double], feats: Array[Int], d: Double) = {
    var i = 0
    while(i < feats.length) {
      vec(feats(i)) += d
      i += 1
    }
    vec
  }

}

case class SubstringFeature(w: String) extends Feature

class SimpleWordShapeGen[L](tagWordCounts: Counter2[L,String,Double],
                            counts: Counter[String,Double], noShapeThreshold: Int = 100, minCountThreshold: Int = 5) extends (String=>IndexedSeq[String]) with Serializable {
  def apply(w: String) = {
    if(counts(w) > noShapeThreshold) {
      ArrayBuffer(w, ("T-"+tagWordCounts(::, w).argmax))
    } else {
      val buf = ArrayBuffer[String](//IndicatorFeature(w),
        EnglishWordClassGenerator.signatureFor(w),
        makeShapeFeature(w)
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

  def makeShapeFeature(word: String) = {
    val result = new StringBuilder(word.length);
    var i = 0;
    while(i < word.length) {
      val c = word(i);
      val x = if(c.isLetter && c.isUpper) 'X' else if(c.isLetter) 'x' else if(c.isDigit) 'd' else c;
      if(result.length > 1 && (result.last == x) && result(result.length - 2) == x) {
        result += 'e'
      } else if (result.length > 1 && result.last == 'e' && result(result.length - 2) == x) {
        () // nothing
      }else {
        result += x;
      }
      i +=1;
    }
    result.toString
  }
}

case class LexParserModelFactory(baseParser: ParserParams.BaseParser,
                                 oldWeights: File = null,
                                 dummyFeats: Double = 0.5,
                                 minFeatCutoff: Int = 1) extends ParserExtractableModelFactory[AnnotatedLabel,String] {
  type MyModel = LexModel[AnnotatedLabel,String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val xbarParser = baseParser.xbarParser(trees)
    val wordIndex = Index(trainTrees.iterator.flatMap(_.words))
    val summedCounts = Library.sum(initLexicon)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
    val tagShapeGen = new WordShapeFeaturizer(summedCounts)


    def ruleGen(r: Rule[AnnotatedLabel]) = IndexedSeq(RuleFeature(r))
    def validTag(w: String) = xbarParser.lexicon.tagScores(w).keysIterator.toArray

    val headFinder = HeadFinder.collins
    val feat = new StandardFeaturizer(wordIndex,
    xbarParser.grammar.labelIndex,
    xbarParser.grammar.index,
    ruleGen,
    shapeGen,
    { (w:String) => tagShapeGen.featuresFor(w)})

    val indexed =  IndexedFeaturizer.extract[AnnotatedLabel,String](feat,
      headFinder,
      xbarParser.grammar.index,
      xbarParser.grammar.labelIndex,
      dummyFeats,
      minFeatCutoff,
      trees)

    val bundle = new LexGrammarBundle[AnnotatedLabel, String](xbarParser.grammar,
      initLexicon.keys.map(_._1).toSet.toIndexedSeq,
      headFinder,
      wordIndex,
      validTag _
    )

    val featureCounter = if(oldWeights ne null) {
      readObject[Counter[Feature,Double]](oldWeights)
    } else {
      Counter[Feature,Double]()
    }

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    val model = new LexModel[AnnotatedLabel,String](bundle, reannotate, indexed, xbarParser, {featureCounter.get(_)})

    model


  }
}
