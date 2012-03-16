package scalanlp.parser
package lex

import scalanlp.optimize.FirstOrderMinimizer.OptParams
import java.io.File
import scalanlp.parser.ParseEval.Statistics
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalala.tensor.{Counter2, Counter}
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

trait LexFeaturizer[L, W] {
  def specialize(words: Seq[W]):Specialization

  val featureIndex: Index[Feature]

  trait Specialization {
    def featuresForUnary(rule: Int, head: Int): Array[Int]

    def featuresForLeft(rule: Int, head: Int, leftHead: Int): Array[Int]

    def featuresForRight(rule: Int, head: Int, rightHead: Int): Array[Int]

    def featuresForTag(tag: Int, head: Int): Array[Int]
  }
}

case class HeadFeature[P](r: Feature, head: P) extends Feature

case class DepFeature[P](r: Feature, dep: P) extends Feature

case class BilexicalFeature[W](head: W, dep: W, dir: Symbol) extends Feature
case class TagFeature[L,W](tag: L, dep: W) extends Feature
case class DistFeature(dist: Int, f: Feature) extends Feature
case class PartFeature[P](feature: Feature, part: P) extends Feature

case class StandardFeaturizer[L, W>:Null, P](wordIndex: Index[W],
                                    cooccurringWords: Set[(W,W)],
                                    ruleIndex: Index[Rule[L]],
                                    labelIndex: Index[L],
                                    validTags: W=>Array[L],
                                    ruleFeatGen: Rule[L]=>IndexedSeq[Feature],
                                    featGen: (W) => IndexedSeq[P],
                                    tagFeatGen: (W=>IndexedSeq[P])) extends LexFeaturizer[L, W] {

  def specialize(words: Seq[W]) = new Spec(words)


  /* precompute features. Such a mess. */
  // word parts are things like suffixes, etc.
  val wordPartIndex = Index[P]()
  val featureIndex = Index[Feature]() // actual features

  // binned distances
//  val distBins = Array(0,1,2,3)
//  val minimums = Array(0,2,4,10)
    val distBins = Array(0)
    val minimums = Array(0)
  val numDistBins = distBins.length

  private def binDistance(dist2:Int) = 0
  /*
  {
    val dist = dist2.abs
    if(dist >= 10) 3
    else if (dist > 3) 2
    else if(dist > 1) 1
    else 0
  }
  */

  def getHeadRuleFeatures(r: Int, w: Int, ww: W, distBin: Int): Array[Feature] = {
    if(ruleIndex.get(r).children.size == 2) {
      val rawFeatures = for (rf <- ruleFeatures(r); wf <- indexedFeaturesForWord(w, ww)) yield HeadFeature(featureIndex.get(rf), wordPartIndex.get(rf))
      val distedFeatures = for (f <- rawFeatures) yield DistFeature(distBins(distBin), f)
      (rawFeatures ++ distedFeatures)
    } else if(distBin == 0) {
      val rawFeatures = for( rf <- ruleFeatures(r); wf <- indexedFeaturesForWord(w, ww)) yield (HeadFeature(featureIndex.get(rf),wordPartIndex.get(rf)):Feature)
      rawFeatures
    } else {
      null
    }
  }

  def getDepRuleFeatures(r: Int, w: Int, ww: W, distBin: Int): Array[Feature] = {
    if(ruleIndex.get(r).children.size == 2) {
      val rawFeatures = for (rf <- ruleFeatures(r); wf <- indexedFeaturesForWord(w, ww)) yield DepFeature(featureIndex.get(rf), wordPartIndex.get(rf))
      val distedFeatures = for (f <- rawFeatures) yield DistFeature(distBins(distBin), f)
      (rawFeatures ++ distedFeatures)
    } else {
      null
    }
  }

  def getBilexFeatures(head: Int, hww: W, dep: Int, dww: W, sym: Symbol, d: Int): Array[Feature] = {
    val rawFeatures = for (rf <- indexedFeaturesForWord(head, hww); wf <- indexedFeaturesForWord(dep, dww)) yield BilexicalFeature(wordPartIndex.get(rf), wordPartIndex.get(wf), sym)
    val distedFeatures = for (f <- rawFeatures) yield DistFeature(distBins(d), f)
    (rawFeatures ++ distedFeatures)
  }

  //////////////
  /// caches ///
  //////////////

  // wordIndex -> seq of feature indices
  val wordCache: Array[Array[Int]] = Encoder.fromIndex(wordIndex).tabulateArray(w => featGen(w).map(wordPartIndex.index(_)).toArray)
  // these are features used by the tagger
  val tagCache = Encoder.fromIndex(wordIndex).tabulateArray(w => tagFeatGen(w).map(wordPartIndex.index(_)).toArray)
  // Rule -> Array[Feature Index]
  val ruleFeatures = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatGen(r).map(featureIndex.index(_)).toArray)
  // binned distance -> head word -> dependency word -> array of int features
  val headDepFeaturesLeft = Array.fill(distBins.size)(new OpenAddressHashArray[Array[Int]](wordIndex.size * wordIndex.size, null:Array[Int], cooccurringWords.size * 3))
  val headDepFeaturesRight = Array.fill(distBins.size)(new OpenAddressHashArray[Array[Int]](wordIndex.size * wordIndex.size, null:Array[Int], cooccurringWords.size * 3))
  for( (head,dep) <- cooccurringWords; d <- 0 until distBins.length) {
    val headIndex = wordIndex(head)
    val depIndex = wordIndex(dep)
    val index = wordIndex.size * headIndex + wordIndex(dep)
    headDepFeaturesLeft(d)(index) = getBilexFeatures(headIndex, head, depIndex, dep, 'Left, minimums(d)).map(featureIndex.index(_))
    headDepFeaturesRight(d)(index) = getBilexFeatures(headIndex, head, depIndex, dep, 'Right, minimums(d)).map(featureIndex.index(_))
  }

  // binned distance -> head word -> rule -> array of int features
  val headRuleFeatures = Array.tabulate(numDistBins,wordIndex.size,ruleIndex.size){ (d,w,r) =>
    val feats = getHeadRuleFeatures(r, w, null, minimums(d))
    if(feats eq null) null
    else feats.map(featureIndex.index(_))
  }
  // dist 0 also holds unary features
  // binned distance -> dep word -> rule -> array of int features
  val depRuleFeatures = Array.tabulate(numDistBins,wordIndex.size,ruleIndex.size){ (d,w,r) =>
    val feats = getDepRuleFeatures(r, w, null, minimums(d))
    if(feats eq null) null
    else feats.map(featureIndex.index(_))
  }

  // word -> Hash array of (Tag -> Array[Feature]). If the Tag isn't there, it's not allowed
  val tagFeatures: Array[OpenAddressHashArray[Array[Int]]] = Array.tabulate(wordIndex.size){ (w) =>
    val validTags = this.validTags(wordIndex.get(w))
    val oah = new OpenAddressHashArray(labelIndex.size, null: Array[Int], validTags.length * 2)
    for(t <- validTags) {
      val itag = labelIndex(t)
      oah(itag) = for( rf <- tagCache(w)) yield featureIndex.index(TagFeature(t,wordPartIndex.get(w)))
    }
    oah
  }

  def indexedFeaturesForUnary(r: Int, w: Int, ww: W) = {
    if(w >= 0) headRuleFeatures(0)(w)(r)
    else {
      stripEncode(featureIndex, getHeadRuleFeatures(r,w,ww,0))
    }
  }

  def indexedFeaturesForBinaryRule(r: Int, w: Int, ww: W, dep: Int, dww: W, dist: Int) = {
    val rule = ruleFeatures(r)
    val ruleHead = indexedFeaturesForRuleHead(r,w,ww, dist)
    val ruleDep = indexedFeaturesForRuleDep(r,dep,dww, dist)
    val bilex = indexedFeaturesForBilex(w,ww,dep,dww,dist)
    val arr = new Array[Int](rule.length + ruleHead.length + ruleDep.length + bilex.length)
    var destPos = 0
    System.arraycopy(rule,0,arr,destPos,rule.length)
    destPos += rule.length
    System.arraycopy(ruleHead,0,arr,destPos,ruleHead.length)
    destPos += ruleHead.length
    System.arraycopy(ruleDep,0,arr,destPos,ruleDep.length)
    destPos += ruleDep.length
    System.arraycopy(bilex,0,arr,destPos,bilex.length)
    arr
  }

  def indexedFeaturesForRuleDep(r: Int, w: Int, ww: W, dist: Int) = {
    if(w >= 0) depRuleFeatures(binDistance(dist))(w)(r)
    else {
      stripEncode(featureIndex, getDepRuleFeatures(r, w, ww, binDistance(dist)))
    }
  }

  private def indexWordPair(hw: Int, dw: Int) = {
    hw * wordIndex.size + dw
  }

  def indexedFeaturesForBilex(hw: Int, hww: W, dw: Int, dww: W, dist: Int):Array[Int] = {
    val binned = binDistance(dist)
    val indexed = indexWordPair(hw,dw)
    if(hw >= 0 && dw >= 0 && headDepFeaturesLeft(binned)(indexed) != null)
      if(dist < 0) headDepFeaturesLeft(binned)(indexed)
      else headDepFeaturesRight(binned)(indexed)
    else {
      val feats = getBilexFeatures(hw, hww, dw, dww, if(dist < 0) 'Left else 'Right, binned)
      stripEncode(featureIndex,feats)
    }
  }

  def indexedFeaturesForWord(w: Int, ww: W) = {
    if(w >= 0) wordCache(w)
    else {
      featGen(ww).map(wordPartIndex).filter(_ >= 0).toArray
    }
  }


  def indexedFeaturesForRuleHead(r: Int, w: Int, ww: W, dist: Int) = {
    if(w >= 0) headRuleFeatures(binDistance(dist))(w)(r)
    else {
      stripEncode(featureIndex, getHeadRuleFeatures(r, w, ww, binDistance(dist)))
    }
  }


  private def stripEncode[F](index: Index[F], arr: Array[F]) = {
    val res = ArrayBuilder.make[Int]
    res.sizeHint(arr)
    var i = 0
    while(i < arr.length) {
      val fi = index(arr(i))
      if(fi >= 0) {
        res += fi
      }
      i += 1
    }
    res.result()
  }

  final class Spec(words: Seq[W]) extends Specialization {
    val indexed = words.map(wordIndex).toArray
    /*
    val cache = Array.tabulate(words.length){ wi =>
      val w = words(wi)
      val i = wordIndex(w)
      if(i >= 0) wordCache(i)
      else featGen(w).map(intern)
    }
    val tagcache = Array.tabulate(words.length) { wi =>
      val w = words(wi)
      val i = wordIndex(w)
      if(i >= 0) tagCache(i)
      else tagFeatGen(w).map(intern)
    }
    */

    private def mkOnesVector(array: Array[Int]) = {
      array
    }

    def featuresForUnary(rule: Int, head: Int) = {
      val array = (ruleFeatures(rule) ++ indexedFeaturesForUnary(rule,indexed(head),words(head)))
      mkOnesVector(array)
    }

    def featuresForLeft(rule: Int, head: Int, leftHead: Int) = {
      val array = indexedFeaturesForBinaryRule(rule,indexed(head),words(head),indexed(leftHead),words(leftHead),head-leftHead)
      mkOnesVector(array)
    }

    def featuresForRight(rule: Int, head: Int, rightHead: Int) = {
      val array = indexedFeaturesForBinaryRule(rule,indexed(head),words(head),indexed(rightHead),words(rightHead),head-rightHead)
      mkOnesVector(array)
    }

    def featuresForTag(tag: Int, head: Int) = {
      val w = indexed(head)
      if(w >= 0) {
        mkOnesVector(tagFeatures(w)(tag))
      } else {
        val arr = ArrayBuilder.make[Int]
        val feats = tagFeatGen(words(head))
        arr.sizeHint(feats)
        for(p <- feats) {
          val pi = wordPartIndex(p)
          if(pi >= 0) {
            arr += pi
          }
        }
        val r = arr.result()
        mkOnesVector(r)
      }
    }
  }
}

trait FeatureIndexer[L, W] extends Encoder[Feature] with Serializable {
  val index: Index[Feature]
  val baseGrammar: Grammar[L]
  val headFinder: HeadFinder[L]
  val tags: IndexedSeq[L]
  val wordIndex: Index[W]
  val featurizer: LexFeaturizer[L, W]
  def validTags(w: W):Array[L]

  val leftRules: Array[Boolean]
  val rightRules: Array[Boolean]
}

object FeatureIndexer {
  def extract[L, W](featurizer: LexFeaturizer[L, W], headFinder: HeadFinder[L],
                    builder: ChartBuilder[ParseChart, L, W],
                    wordIndex: Index[W],
                    validTags: W=>Array[L],
                    trees: Traversable[TreeInstance[L, W]]): FeatureIndexer[L, W] = {
    val bg = builder.grammar

    val lRules = new Array[Boolean](bg.index.size)
    val rRules = new Array[Boolean](bg.index.size)

    for(ti <- trees;  t <- ti.tree.allChildren if t.children.size > 1) t match {
      case t:BinaryTree[L] =>
        val headChild = headFinder.findHeadChild(t,identity[L])
        val r = builder.grammar.index(BinaryRule(t.label, t.leftChild.label, t.rightChild.label))
        if(headChild == 0) {
          lRules(r) = true
        } else {
          rRules(r) = true
        }
      case _ =>
    }

    val wi = wordIndex
    val vt = validTags

    val f = featurizer
    val hf = headFinder
    new FeatureIndexer[L, W] {
      val featurizer = f
      val index = featurizer.featureIndex
      val baseGrammar = builder.grammar
      val headFinder = hf
      val tags = builder.lexicon.tags.toIndexedSeq
      val wordIndex = wi


      def validTags(w: W) = vt(w)

      val leftRules = lRules
      val rightRules = rRules

    }

  }
}

object FeaturizedLexGrammar {
  def apply[L, W](fi: FeatureIndexer[L, W],
                  weights: DenseVector[Double]): LexGrammar[L, W] = {
    val bg = fi.baseGrammar
    val wi = fi.wordIndex
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

      val tags = fi.tags
      val indexedTags = BitSet() ++ fi.tags.map(bg.labelIndex)

      def isLeftRule(r: Int) = fi.leftRules(r)

      def isRightRule(r: Int) = fi.rightRules(r)


      def specialize(sent: Seq[W]) = new Specialization(sent)

      final class Specialization(val words: Seq[W]) extends super.Specialization {
        val indexed = words.map(wordIndex)
        val f = fi.featurizer.specialize(words)
        val indexedValidTags = words.map(fi.validTags).map(_.map(labelIndex))



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
            val score = dot(f.featuresForLeft(rule, head, leftHead))
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
            val score = dot(f.featuresForRight(rule, head, rightHead))
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


class LexModel[L, W](indexed: FeatureIndexer[L,W],
                     coarse: ChartBuilder[ParseChart, L, W]) extends Model[TreeInstance[L, W]] with Serializable {

  def extractParser(weights: DenseVector[Double]) = {
    val inf = inferenceFromWeights(weights)
    new LexChartParser(coarse.grammar, inf.builder)
  }

  def numFeatures = indexed.index.size
  import indexed._

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram: LexGrammar[L, W] = FeaturizedLexGrammar(indexed, weights)
    val builder = new LexCKYChartBuilder(coarse.root, gram, ParseChart.logProb)
    new LexInference(builder, indexed.featurizer, headFinder)
  }

  type Inference = LexInference[L, W]
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]

  def emptyCounts = new ExpectedCounts(numFeatures)

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    (ecounts.loss, ecounts.features)
  }

}

case class LexInference[L, W](builder: LexCKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                              featurizer: LexFeaturizer[L,W],
                              headFinder: HeadFinder[L]) extends MarginalInference[TreeInstance[L, W], SpanScorer[L]] {
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]
  type Marginal = LexChartPair[ParseChart.LogProbabilityParseChart, L, W]


  def marginal(v: TreeInstance[L, W], aug: SpanScorer[L]) = {
    val r = builder.buildCharts(v.words, aug)
    r -> r.partition
  }


  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: SpanScorer[L]) = {
    val root_score = marg.partition
    val ec = new LexInsideOutside(featurizer, builder).expectedCounts(marg.spec, marg.inside,
      marg.outside, root_score, marg.scorer)
    ec
  }


  def goldCounts(ti: TreeInstance[L, W], augment: SpanScorer[L]) = {
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
        val headIsLeft = headFinder.findHeadChild(t, identity[L]) == 0
        val (head, dep) = if(headIsLeft) childHeads(0) -> childHeads(1) else childHeads(1) -> childHeads(0)
        val r = g.index(BinaryRule(a, b, c))
        if(headIsLeft) {
          score += ( spec.scoreRightComplement(r, head, dep)
//            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
//            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
          addMultiple(counts.features, fspec.featuresForRight(r,head,dep), 1.0)
        } else {
          score += ( spec.scoreLeftComplement(r, head, dep)
//            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
//            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
          addMultiple(counts.features, fspec.featuresForLeft(r,head,dep), 1.0)
        }
        if(headIsLeft) childHeads(0) else childHeads(1)
    }
    rec(ti.tree)
    assert(!score.isInfinite)
    counts.logProb = score
    counts
  }

  def baseAugment(v: TreeInstance[L, W]) = v.spanScorer


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

class SimpleWordShapeGen(tagWordCounts: Counter2[String,String,Double], counts: Counter[String,Double], noShapeThreshold: Int = 100, minCountThreshold: Int = 5) extends (String=>IndexedSeq[String]) with Serializable {
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


/**
 *
 * @author dlwh
 */
object LexDiscrimPipeline extends ParserPipeline {

  protected val paramManifest = manifest[Params];

  case class Params(parser: ParserParams.BaseParser[String],
                    opt: OptParams,
                    cachedIndex: File = new File("features.ser.gz"),
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    oldWeights: File = null);

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  validate: (Parser[String, String]) => Statistics,
                  params: Params) = {
    import params._

    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trainTrees)

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries), Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart, String, String]("", lexicon, grammar, ParseChart.logProb);
    }
    val wordIndex = Index(trainTrees.iterator.flatMap(_.words))
    val summedCounts = Library.sum(initLexicon)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
    val tagShapeGen = new WordShapeFeaturizer(summedCounts)


    def ruleGen(r: Rule[String]) = IndexedSeq(RuleFeature(r))
    def validTag(w: String) = xbarParser.lexicon.tagScores(w).keysIterator.toArray



    //    def featGen(w: String) = {
//      if(summedCounts(w) < 30)  Counter( (IndicatorFeature(EnglishWordClassGenerator.signatureFor(w)):Feature) -> 1.0)
//      else Counter( (IndicatorFeature(w):Feature) -> 1.0)
//    }

    println("Wordindex size: " + wordIndex.size)

    val headFinder = HeadFinder.collinsHeadFinder
    val indexed = if(cachedIndex.exists()) {
      scalanlp.util.readObject[FeatureIndexer[String,String]](cachedIndex)
    } else {
      val cooccurringWords = trainTrees.par.aggregate(Set.empty[(String,String)])({ (agg,ti) =>
        agg ++ (for(w1 <- ti.words; w2 <- ti.words) yield (w1,w2))
      }, (_ ++ _))
      println("cooc word size: " + cooccurringWords.size)
      val feat = new StandardFeaturizer(wordIndex, cooccurringWords,
                                        xbarParser.grammar.index,
                                        xbarParser.grammar.labelIndex,
                                        validTag,
                                        ruleGen,
                                        shapeGen,
                                        { (w:String) => tagShapeGen.featuresFor(w)})
      val f = FeatureIndexer.extract(feat, headFinder, xbarParser, wordIndex, validTag, trainTrees)
      scalanlp.util.writeObject(cachedIndex,f)
      f
    }

    val model = new LexModel(indexed, xbarParser)

    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
//    val checking = new RandomizedGradientCheckingFunction(cachedObj)
    val init = obj.initialWeightVector

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState, Int) ) {
      val (state, iter) = pair
      val weights = state.x
      if(iter % iterPerValidate == 0) {
        println("Validating...")
        val parser = model.extractParser(weights)
        println(validate(parser))
      }
    }

    for( (state, iter) <- params.opt.iterations(cachedObj, init).take(maxIterations).zipWithIndex.tee(evalAndCache _)
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x)
      ("LexDiscrim-" + iter.toString, parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }


  }
}