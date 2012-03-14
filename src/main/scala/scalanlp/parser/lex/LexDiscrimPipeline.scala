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
import collection.mutable.ArrayBuffer

trait LexFeaturizer[L, W] {
  def specialize(words: Seq[W]):Specialization

  def featuresForUnary(rule: Int, head: W): IndexedSeq[Feature] = specialize(Seq(head)).featuresForUnary(rule,0)

  def featuresForLeft(rule: Int, head: W, leftHead: W): IndexedSeq[Feature] = specialize(Seq(head,leftHead)).featuresForLeft(rule,0,1)

  def featuresForRight(rule: Int, head: W, rightHead: W): IndexedSeq[Feature] = specialize(Seq(head,rightHead)).featuresForRight(rule,0,1)

  def featuresForTag(tag: Int, head: W): IndexedSeq[Feature] = specialize(Seq(head)).featuresForTag(tag,0)

  trait Specialization {
    def featuresForUnary(rule: Int, head: Int): IndexedSeq[Feature]

    def featuresForLeft(rule: Int, head: Int, leftHead: Int): IndexedSeq[Feature]

    def featuresForRight(rule: Int, head: Int, rightHead: Int): IndexedSeq[Feature]

    def featuresForTag(tag: Int, head: Int): IndexedSeq[Feature]
  }
}

case class HeadFeature(r: Int, head: AnyRef) extends Feature {
  override def hashCode()  = {
    (r * 37 + head.hashCode)
  }
}

case class DepFeature(r: Int, dep: AnyRef) extends Feature {
  override def hashCode()  = {
    (r * 37 + dep.hashCode)
  }
}

case class BilexicalFeature[W](head: W, dep: W, dir: Symbol) extends Feature
case class TagFeature(label: Int, f: Any) extends Feature
case class DistFeature(dist: Int) extends Feature {
  override def hashCode = dist * 37
}
case class IntRuleFeature(x: Int) extends Feature {
  override def hashCode = x
}

case class StandardFeaturizer[L, W](wordIndex: Index[W],
                                    featGen: (W) => IndexedSeq[AnyRef],
                                    tagFeatGen: (W=>IndexedSeq[AnyRef])) extends LexFeaturizer[L, W] {

  def specialize(words: Seq[W]) = new Spec(words)

  val intern = new Interner[AnyRef]
  val wordCache = Encoder.fromIndex(wordIndex).tabulateArray(w => featGen(w).map(intern))
  val tagCache = Encoder.fromIndex(wordIndex).tabulateArray(w => tagFeatGen(w).map(intern))

  final class Spec(words: Seq[W]) extends Specialization {
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

    def featuresForUnary(rule: Int, head: Int) = {
      val ctr = ArrayBuffer[Feature]()
      val ruleFeature = IntRuleFeature(rule)
      ctr += ruleFeature
      for (f <- cache(head)) {
        ctr += HeadFeature(rule, f)
      }
      ctr
    }

    private def binDistance(head: Int, dep: Int) = {
      val dist = (head - dep).abs
      if(dist > 10) 10
      else if (dist > 5) 5
      else dist
    }

    def featuresForLeft(rule: Int, head: Int, leftHead: Int) = {
      val ctr = ArrayBuffer[Feature]()
      val ruleFeature = IntRuleFeature(rule)
      ctr += ruleFeature
      val headFeats = cache(head)
      val depFeats = cache(leftHead)
      val dist = new DistFeature(binDistance(head,leftHead))
      for (f <- headFeats) {
        val headRfeature = HeadFeature(rule, f)
        ctr += headRfeature
        ctr += PairFeature(headRfeature,dist)
        for (f2 <- depFeats) {
//          ctr += JointFeature(rule, f, f2)
          val bilex = BilexicalFeature(f, f2, 'Left)
          ctr += PairFeature(bilex,dist)
        }
      }
      for (f2 <- depFeats) {
        val depRuleFeature = DepFeature(rule, f2)
        ctr += depRuleFeature
        ctr += PairFeature(depRuleFeature,dist)
      }
      ctr
    }

    def featuresForRight(rule: Int, head: Int, rightHead: Int) = {
      val ctr = ArrayBuffer[Feature]()
      val ruleFeature = IntRuleFeature(rule)
      ctr += ruleFeature
      val headFeats = cache(head)
      val depFeats = cache(rightHead)
      val dist = new DistFeature(binDistance(head,rightHead))
      for (f <- headFeats) {
        val headRfeature = HeadFeature(rule, f)
        ctr += headRfeature
        ctr += PairFeature(headRfeature,dist)
        for (f2 <- depFeats) {
//          ctr += JointFeature(rule, f, f2)
          ctr += BilexicalFeature(f, f2, 'Right)
        }
      }
      for (f2 <- depFeats) {
        val depRuleFeature = DepFeature(rule, f2)
        ctr += depRuleFeature
        ctr += PairFeature(depRuleFeature,dist)
      }
      ctr
    }

    def featuresForTag(tag: Int, head: Int) = {
      val headFeats = tagcache(head).map(TagFeature(tag,_):Feature)
      headFeats
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

  val leftRules: Array[Boolean]
  val rightRules: Array[Boolean]
}

object FeatureIndexer {
  def extract[L, W](featurizer: LexFeaturizer[L, W], headFinder: HeadFinder[L],
                    builder: ChartBuilder[ParseChart, L, W],
                    wordIndex: Index[W],
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
    val numTrainingWords = wi.size

    case class Bundle(touchedWords: collection.mutable.BitSet = collection.mutable.BitSet(),
                    features: collection.mutable.Set[Feature] = collection.mutable.Set[Feature]()) {
      def ++=(bundle: Bundle) = {
        touchedWords ++= bundle.touchedWords
        features ++= bundle.features
        this
      }
    }

    def cachingGram(bundle: Bundle) = new LexGrammar[L, W] {
      import bundle._
      val touchedRules = new collection.mutable.HashSet[Any]()

      val index = bg.index
      val labelIndex = bg.labelIndex
      val indexedRules = bg.indexedRules

      def isLeftRule(r: Int) = lRules(r)
      def isRightRule(r: Int) = rRules(r)
      val wordIndex = wi

      val tags = builder.lexicon.tags.toIndexedSeq
      val indexedTags = BitSet() ++ tags.map(labelIndex)

      def maxNumBinaryRulesForParent = bg.maxNumBinaryRulesForParent

      def ruleIndex(a: Int, b: Int, c: Int) = bg.ruleIndex(a, b, c)

      def ruleIndex(a: Int, b: Int) = bg.ruleIndex(a, b)

      def indexedBinaryRulesWithParent(l: Int) = bg.indexedBinaryRulesWithParent(l)

      def indexedUnaryRulesWithChild(l: Int) = bg.indexedUnaryRulesWithChild(l)

      def indexedUnaryRulesWithParent(l: Int) = bg.indexedUnaryRulesWithParent(l)


      def specialize(sent: Seq[W]) = new this.Specialization(sent)

      class Specialization(val words: Seq[W]) extends super.Specialization {
        val indexed = words.map(wordIndex)
        val f = featurizer.specialize(words)

        def scoreLeftComplement(rule: Int, head: Int, leftHead: Int) = {
          val cacheIndex = indexed(head) + numTrainingWords * (indexed(leftHead))
          assert(cacheIndex >= 0)
          if(!touchedRules(rule -> cacheIndex)) {
            touchedRules += (rule ->  cacheIndex)
            val feats = f.featuresForLeft(rule,  head, leftHead)
            features ++= feats
          }
          0.0
        }


        def scoreRightComplement(rule: Int, head: Int, rightHead: Int) = {
          val cacheIndex = indexed(head) + numTrainingWords * (indexed(rightHead))
          if(!touchedRules(rule -> cacheIndex)) {
            touchedRules += (rule ->  cacheIndex)
            val feats = f.featuresForRight(rule, head, rightHead)
            features ++= feats
          }

          0.0
        }

        def scoreUnary(rule: Int, head: Int) = {
          val cacheIndex = indexed(head)
          if(!touchedRules(rule -> cacheIndex)) {
            touchedRules += (rule ->  cacheIndex)
            val feats = f.featuresForUnary(rule, head)
            features ++= feats
          }

          0.0
        }

        def tagScores(head: Int) = {
          val bundle = indexed(head)
          val scores = builder.lexicon.tagScores(words(head))
          if(!touchedWords(bundle)) {
            touchedWords += bundle
            for ((l, v) <- scores.pairsIterator if !v.isNegInfinity) {
              val feats = f.featuresForTag(labelIndex(l), head)
              features ++= feats
            }
          }

          builder.grammar.labelEncoder.encodeOldSparse(scores, Double.NegativeInfinity)
        }

      }
    }

//    val bundle = new Bundle()
//    val lexGram = cachingGram(bundle)
//    val lexbuilder = new LexCKYChartBuilder[ParseChart.ViterbiParseChart, L, W](builder.root, lexGram, ParseChart.viterbi)
//    for(ti <- trees) {
//      println(ti.words)
//      lexbuilder.buildInsideChart(ti.words, ti.spanScorer)
//
//    }


    val bundle = trees.par.aggregate(null:Bundle)( { (set,ti) =>
      val bundle = if(set eq null) Bundle() else set
      println(ti.words)
      val lexGram = cachingGram(bundle)
      val lexbuilder = new LexCKYChartBuilder[ParseChart.ViterbiParseChart, L, W](builder.root, lexGram, ParseChart.viterbi)
      lexbuilder.buildInsideChart(ti.words, ti.spanScorer)
      bundle
    }, { (a,b) => b ++= a})

    val featureIndex = Index[Feature](bundle.features)

    val f = featurizer
    val hf = headFinder
    new FeatureIndexer[L, W] {
      val index = featureIndex
      val baseGrammar = builder.grammar
      val headFinder = hf
      val tags = builder.lexicon.tags.toIndexedSeq
      val wordIndex = wi
      val featurizer = f

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

      private def dotProduct(features: IndexedSeq[Feature]): Double = {
        var score = 0.0
        for (f <- features) {
          val i = fi.index(f)
          if (i >= 0) {
            score += weights(i)
          }
        }
        score
      }


      def specialize(sent: Seq[W]) = new Specialization(sent)

      final class Specialization(val words: Seq[W]) extends super.Specialization {
        val indexed = words.map(wordIndex)
        val f = fi.featurizer.specialize(words)


//        override def finalize() {
//          println("bcache desnity: " + bCache.activeSize * 1.0 / bCache.size)
//          println("uCache density: " + uCache.activeSize * 1.0 / bCache.size)
//
//        }

        def tagScores(head: Int) = {
          val sv = bg.mkOldSparseVector(Double.NegativeInfinity)
          for (l <- indexedTags) {
            val features = f.featuresForTag(l, head)
            val score = dotProduct(features)
            sv(l) = score
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
            val score = dotProduct(f.featuresForLeft(rule, head, leftHead))
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
            val score = dotProduct(f.featuresForRight(rule, head, rightHead))
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
            val score = dotProduct(f.featuresForUnary(rule, head))
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
    val decoder = new MaxConstituentDecoder[L, L,W](GrammarProjections.identity(coarse.grammar))
    new Parser[L, W] with Serializable {
      def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
        val marg = inf.marginal(new TreeInstance("", null, s, spanScorer), spanScorer)._1
        val pi = LexParseProjector.projectChart(marg.inside, inf.builder.chartFactory)
        val po = LexParseProjector.projectChart(marg.outside, inf.builder.chartFactory)
        val bestParse = decoder.extractBestParse(coarse.root, coarse.grammar, pi, po, s, marg.scorer);
        bestParse
      }
    }
  }

  def numFeatures = indexed.index.size
  import indexed._

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val gram: LexGrammar[L, W] = FeaturizedLexGrammar(indexed, weights)
    val builder = new LexCKYChartBuilder(coarse.root, gram, ParseChart.logProb)
    new LexInference(builder, headFinder)
  }

  type Inference = LexInference[L, W]
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]

  def emptyCounts = new ExpectedCounts(coarse.grammar.index.size, coarse.grammar.labelIndex.size)

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    val counts = expectedCountsToFeatureVector(ecounts)
    (ecounts.loss, counts)
  }

  def expectedCountsToFeatureVector(ecounts: ExpectedCounts) = {
    val result = indexed.mkDenseVector();

    def sumVectorIntoResults(vec: IndexedSeq[Feature], v: Double) {
      if(v != 0.0)
        for ( k <- vec) {
          val j = indexed.index(k)
          if(j >= 0)
            result(j) += v
        }
    }
    import indexed.baseGrammar.{index=>ruleIndex}

    // rules
    for((counter, r) <- ecounts.bCounts.iterator.zipWithIndex) counter.foreachTriple{ (head, dep, v) =>
      sumVectorIntoResults(indexed.featurizer.featuresForLeft(r, head, dep), v)
    }

    for((counter, t) <- ecounts.unaryCounts.iterator.zipWithIndex) counter.foreachPair{ (head, v) =>
      sumVectorIntoResults(indexed.featurizer.featuresForUnary(t, head), v)
    }

    // lex
    for( (ctr, a) <- ecounts.wordCounts.iterator.zipWithIndex; (w, v) <- ctr.nonzero.pairs) {
      val vec = indexed.featurizer.featuresForTag(a, w)
      sumVectorIntoResults(vec, v)
    }

    result;
  }
}

case class LexInference[L, W](builder: LexCKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                              headFinder: HeadFinder[L]) extends MarginalInference[TreeInstance[L, W], SpanScorer[L]] {
  type ExpectedCounts = LexInsideOutside.ExpectedCounts[W]
  type Marginal = LexChartPair[ParseChart.LogProbabilityParseChart, L, W]


  def marginal(v: TreeInstance[L, W], aug: SpanScorer[L]) = {
    val r = builder.buildCharts(v.words, aug)
    r -> r.partition
  }


  def guessCountsFromMarginals(v: TreeInstance[L, W], marg: Marginal, aug: SpanScorer[L]) = {
    val root_score = marg.partition
    val ec = new LexInsideOutside(builder).expectedCounts(marg.spec, marg.inside,
      marg.outside, root_score, marg.scorer)
    ec
  }


  def goldCounts(ti: TreeInstance[L, W], augment: SpanScorer[L]) = {
    val g = builder.grammar
    val spec = g.specialize(ti.words)
    val counts = new ExpectedCounts(builder.grammar.index.size, builder.grammar.labelIndex.size)
    val words = ti.words
    var score = 0.0
    def rec(t: BinarizedTree[L]):Int= t match {
      case n@NullaryTree(a) =>
        val aI = g.labelIndex(a)
        val w = ti.words(n.span.start);
        counts.wordCounts(aI)(w) += 1
        score += spec.tagScores(n.span.start)(aI) + ti.spanScorer.scoreSpan(t.span.start, t.span.end, aI)
        assert(!score.isInfinite)
        n.span.start
      case UnaryTree(a, b) =>
        val h = rec(b)
        val headW = ti.words(h)
        val r = g.index(UnaryRule(a, b.label))
        counts.unaryCounts(r)(headW) += 1
        score += ( spec.scoreUnary(r, h)
//          + augment.scoreUnaryRule(t.span.start, t.span.end, r)
          )
        h
      case t@BinaryTree(a, bt@Tree(b, _), Tree(c, _)) =>
        val childHeads = IndexedSeq(rec(t.leftChild), rec(t.rightChild))
        val headIsLeft = headFinder.findHeadChild(t, identity[L]) == 0
        val (head, dep) = if(headIsLeft) childHeads(0) -> childHeads(1) else childHeads(1) -> childHeads(0)
        val cache = counts.bCounts
        val r = g.index(BinaryRule(a, b, c))
        cache(r)(ti.words(head), ti.words(dep)) += 1
        if(headIsLeft) {
          score += ( spec.scoreRightComplement(r, head, dep)
//            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
//            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
          if(score.isInfinite)
            println('HL,BinaryRule(a,b,c),r,head,dep,spec.scoreRightComplement(r,head,dep), ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a)))
        } else {
          score += ( spec.scoreLeftComplement(r, head, dep)
//            + ti.spanScorer.scoreSpan(t.span.start, t.span.end, g.labelIndex(a))
//            + ti.spanScorer.scoreBinaryRule(t.span.start, bt.span.end, t.span.end, r)
            )
        }
        if(headIsLeft) childHeads(0) else childHeads(1)
    }
    rec(ti.tree)
    assert(!score.isInfinite)
    counts.logProb = score
    counts
  }

  def baseAugment(v: TreeInstance[L, W]) = v.spanScorer

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
    val feat = new StandardFeaturizer[String, String](wordIndex, shapeGen, { (w:String) => tagShapeGen.featuresFor(w)})

//    def featGen(w: String) = {
//      if(summedCounts(w) < 30)  Counter( (IndicatorFeature(EnglishWordClassGenerator.signatureFor(w)):Feature) -> 1.0)
//      else Counter( (IndicatorFeature(w):Feature) -> 1.0)
//    }

    val headFinder = HeadFinder.collinsHeadFinder
    val indexed = if(cachedIndex.exists()) {
      scalanlp.util.readObject[FeatureIndexer[String,String]](cachedIndex)
    } else {
      val f = FeatureIndexer.extract(feat, headFinder, xbarParser, wordIndex, trainTrees)
      scalanlp.util.writeObject(cachedIndex,f)
      f
    }

    val model = new LexModel(indexed, xbarParser)

    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
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