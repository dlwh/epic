package scalanlp.parser
package models

import features._
import scalala.tensor.dense.DenseVector
import scalanlp.collection.mutable.{TriangularArray, OpenAddressHashArray}
import scalala.tensor.mutable.Counter
import scalanlp.trees._
import collection.mutable.ArrayBuilder
import java.io.File
import scalala.library.Library
import scalanlp.util._
import scalanlp.epic.Feature

/**
 *
 * @author dlwh
 */
class SpanModel[L, W](featurizer: RefinedFeaturizer[L, W, Feature],
                      val featureIndex: Index[Feature],
                      ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                      baseFactory: CoreGrammar[L, W],
                      grammar: BaseGrammar[L],
                      lexicon: Lexicon[L, W],
                      initialFeatureVal: (Feature => Option[Double]) = {
                        _ => None
                      }) extends ParserModel[L, W] {
  type Inference = DiscParserInference[L, W]

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0
  def emptyCounts = new ExpectedCounts(featureIndex)

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val factory = new DotProductGrammar(grammar, lexicon, weights, featurizer)
    def reannotate(bt: BinarizedTree[L], words: Seq[W]) = {
      ann(bt, words).map { l =>
        l -> 0
      }
    }
    new DiscParserInference(featurizer, reannotate, factory, baseFactory)
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    ecounts.loss -> ecounts.counts
  }
}


class DotProductGrammar[L, W, Feature](val grammar: BaseGrammar[L],
                                       val lexicon: Lexicon[L, W],
                                       val weights: DenseVector[Double],
                                       val featurizer: RefinedFeaturizer[L, W, Feature]) extends RefinedGrammar[L, W] {
  def specialize(w: Seq[W]):RefinedAnchoring[L, W] = LiftedCoreAnchoring(new CoreAnchoring[L, W] {

    val grammar = DotProductGrammar.this.grammar
    val lexicon = DotProductGrammar.this.lexicon

    def words = w

    val fspec = featurizer.specialize(w)
    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
      dot(fspec.featuresForBinaryRule(begin, split, end, rule, 0))
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
      dot(fspec.featuresForUnaryRule(begin, end, rule, 0))
    }

    def scoreSpan(begin: Int, end: Int, tag: Int) = {
      dot(fspec.featuresForSpan(begin, end, tag, 0))
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

  })
}

trait SpanFeaturizer[L, W] extends Serializable {
  def specialize(words: Seq[W]): Specialization

  /**
   * Specialization assumes that features are of several kinds, so that we can efficiently cache them.
   */
  trait Specialization {
    def words: Seq[W]
    def featuresForSpan(begin: Int, end: Int, label: Int): Array[Feature]
    def featuresForRule(begin: Int, end: Int, rule: Int): Array[Feature]
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Array[Feature]
  }

}


class IndexedSpanFeaturizer[L, W](f: SpanFeaturizer[L, W],
                                  grammar: BaseGrammar[L],
                                  val trueFeatureIndex: Index[Feature],
                                  dummyFeatures: Int) extends RefinedFeaturizer[L, W, Feature] with Serializable {
  val index:Index[Feature] = {
    val r = Index[Feature]()
    (trueFeatureIndex) foreach (r.index(_))
    (0 until dummyFeatures) map {HashFeature(_)} foreach {r.index _}
    r
  }

  def specialize(words: Seq[W]):Anchoring = new Spec(words)

  case class Spec(words: Seq[W]) extends super.Anchoring {
    def length = words.length
    private val fspec = f.specialize(words)

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      require(ref == 0)
      val ind = TriangularArray.index(begin, end)
      var rcache = spanCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](grammar.labelIndex.size)
        spanCache(ind) = rcache
      }
      var cache = rcache(tag)
      if(cache == null)  {
        cache = stripEncode(index, fspec.featuresForSpan(begin, end, tag))
        rcache(tag) = cache
      }
      cache
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val ind = TriangularArray.index(begin, end)
      var rcache = unaryCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](grammar.index.size)
        unaryCache(ind) = rcache
      }
      var cache = rcache(rule)
      if(cache == null)  {
        cache = stripEncode(index, fspec.featuresForRule(begin, end, rule))
        rcache(rule) = cache
      }
      cache
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val ind = TriangularArray.index(begin, end)
      var rcache = binaryCache(ind)
      if(rcache eq null) {
        rcache = new Array[OpenAddressHashArray[Array[Int]]](end - begin)
        binaryCache(ind) = rcache
      }
      var scache = rcache(split - begin)
      if(scache eq null) {
        scache = new OpenAddressHashArray[Array[Int]](grammar.index.size)
        rcache(split - begin) = scache
      }
      var cache = scache(rule)
      if(cache == null)  {
        cache = stripEncode(index, fspec.featuresForBinaryRule(begin, split, end, rule)) ++ featuresForUnaryRule(begin, end, rule, 0)
        scache(rule) = cache
      }
      cache

    }

    // caches:
    // (beg,end) -> label ->  Array[Int]
    val spanCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (beg,end) -> rule -> Array[Int]
    val unaryCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (beg, end) -> (split - beg) -> Array[Int]
    val binaryCache = TriangularArray.raw[Array[OpenAddressHashArray[Array[Int]]]](length + 1, null)


    private def stripEncode(index: Index[Feature], arr: Array[Feature], dest: Array[Int] = null, destStart: Int = 0) = {
      val res = if(dest eq null) new Array[Int](arr.length) else dest
      var i = destStart
      while( i < arr.length) {
        val fi = index(arr(i))
        if(fi >= 0) {
          res(destStart + i)  = fi
//        } else if (lowCountFeatures.contains(arr(i))) {
//          res(destStart + i) = lowCountFeature
        } else if(dummyFeatures > 0) {
          res(destStart + i) = (trueFeatureIndex.size + math.abs(arr(i).hashCode) % dummyFeatures)
        }
        i += 1
      }
      res
    }

  }

}

object IndexedSpanFeaturizer {
  def extract[L, W](featurizer: SpanFeaturizer[L, W],
                    ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                    grammar: BaseGrammar[L],
                    dummyFeatScale: Double,
                    trees: Traversable[TreeInstance[L, W]]): IndexedSpanFeaturizer[L, W] = {

    import grammar.labelIndex
    import grammar.{index=>ruleIndex}
    def add(ctr: Counter[Feature, Int], feats: Array[Feature]) {
      for (f <- feats) {
        ctr(f) += 1
      }
    }
    val goldFeatures = trees.par.aggregate(null: Counter[Feature, Int])( {(feats, ti) =>
      val set = if(feats eq null) Counter[Feature, Int]() else feats
      val spec = featurizer.specialize(ti.words)
      // returns head
      def rec(t: BinarizedTree[L]):Unit = t match {
        case n@NullaryTree(a) =>
          val aI = labelIndex(a)
          add(set, spec.featuresForSpan(n.span.start, n.span.end, aI))
        case u@UnaryTree(a, b) =>
          val r = ruleIndex(UnaryRule(a, b.label))
          rec(u.child)
          add(set, spec.featuresForRule(u.span.start, u.span.end, r))
        case t@BinaryTree(a, bt@Tree(b, _), ct@Tree(c, _)) =>
          rec(t.leftChild)
          rec(t.rightChild)
          val r = ruleIndex(BinaryRule(a, b, c))
          val aI = labelIndex(a)
          add(set, spec.featuresForSpan(t.span.start, t.span.end, aI))
          add(set, spec.featuresForRule(t.span.start, t.span.end, r))
          add(set, spec.featuresForBinaryRule(t.span.start, bt.span.end, t.span.end, r))
      }
      rec(ann(ti.tree, ti.words))
      set
    }, {(a, b) => a += b})

    val goldFeatureIndex = Index[Feature]()
    for( (f, v) <- goldFeatures.pairsIteratorNonZero) {
       goldFeatureIndex.index(f)
    }
    println(goldFeatureIndex.size)

    new IndexedSpanFeaturizer(featurizer, grammar, goldFeatureIndex, (goldFeatureIndex.size * dummyFeatScale).toInt)
  }
}

class StandardSpanFeaturizer[L, W](grammar: BaseGrammar[L],
                                   wordGen: W=>Traversable[String],
                                   labelFeatures: Array[Array[Feature]],
                                   ruleFeatures: Array[Array[Feature]]) extends SpanFeaturizer[L, W] {
  def specialize(w: Seq[W]):Specialization = new Specialization {
    def words = w
    val length = w.length
    val wordFeats = w.toIndexedSeq.map(wordGen).map(_.map(IndicatorFeature(_)))
    import features.StandardSpanFeatures._

    private def binDistance(dist2:Int) = {
      val dist = dist2.abs - 1
      if (dist >= 20) 4
      else if (dist >= 10) 3
      else if (dist >= 5) 2
      else if (dist >= 2) 1
      else 0
    }

    def featuresForSpan(begin: Int, end: Int, label: Int) = {
      val builder = ArrayBuilder.make[Feature]
      builder ++=  labelFeatures(label)

      val lf = LabelFeature(label)
      val lengthFeature = SpanLengthFeature(binDistance(end - begin))
      builder += PairFeature(lf, lengthFeature)
      if(begin + 1 == end) {
        for(w <- wordFeats(begin))
          builder += PairFeature(lf, w)
      } else {
        if(end == length)
          builder += BeginSentFeature(lf)
        else for(w <- wordFeats(end)) {
          val tf = TaggedFeature(w, 'NextWord)
          builder += tf
          builder += PairFeature(lf, tf)
        }

        if(begin == 0)
          builder += BeginSentFeature(lf)
        else for(w <- wordFeats(begin - 1)) {
          val tf = TaggedFeature(w, 'PrevWord)
          builder += tf
          builder += PairFeature(lf, tf)
        }

        for(w <- wordFeats(begin)) {
          val tf = TaggedFeature(w, 'FirstWord)
          builder += tf
          builder += PairFeature(lf, tf)
        }

        for(w <- wordFeats(end-1)) {
          val tf = TaggedFeature(w, 'LastWord)
          builder += tf
          builder += PairFeature(lf, tf)
        }

        if(begin > 0 && end < length) {
          for(wA <- wordFeats(begin-1);
              wB <- wordFeats(end)) {
            val edge = WordEdges(label, wA, wB)
            builder += edge
            builder += PairFeature(edge, lengthFeature)
          }
        }

      }

      builder.result()
    }

    def featuresForRule(begin: Int, end: Int, rule: Int) = {
      val builder = ArrayBuilder.make[Feature]
      builder ++=  ruleFeatures(rule)

      val rf = RuleFeature(grammar.index.get(rule))
      if(begin+1 == end) {
        for(w <- wordFeats(begin))
          builder += LexicalFeature(rf, w)
      }

      for(w <- wordFeats(begin)) {
        builder += TaggedFeature(PairFeature(rf, w), 'BeginWord)
      }

      for(w <- wordFeats(end-1)) {
        builder += TaggedFeature(PairFeature(rf, w), 'EndWord)
      }

      builder.result()
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
      val builder = ArrayBuilder.make[Feature]

      val rf = RuleFeature(grammar.index.get(rule))
      for(w <- wordFeats(split)) {
        builder += TaggedFeature(PairFeature(rf, w), 'SplitWord)
        builder += TaggedFeature(w, 'SplitWord)
      }

      builder.result()
    }
  }
}

case class SpanModelFactory(baseParser: ParserParams.BaseParser,
                                 constraints: ParserParams.Constraints[AnnotatedLabel, String],
                                 oldWeights: File = null,
                                 dummyFeats: Double = 0.5,
                                 minFeatCutoff: Int = 1) extends ParserExtractableModelFactory[AnnotatedLabel, String] {
  type MyModel = SpanModel[AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trees)
    val summedCounts = Library.sum(initLexicon)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
//    val tagShapeGen = new WordShapeFeaturizer(summedCounts)

    val lexicon:Lexicon[AnnotatedLabel, String] = initLexicon

    val baseFactory = RefinedGrammar.generative(xbarGrammar,
      xbarLexicon, initBinaries, initUnaries, initLexicon)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

    val labelFeatures = xbarGrammar.labelEncoder.tabulateArray(l => Array(LabelFeature(l):Feature))
    val ruleFeatures = xbarGrammar.tabulateArray(l => Array(RuleFeature(l):Feature))

    val feat = new StandardSpanFeaturizer[AnnotatedLabel, String](
    xbarGrammar,
      shapeGen,
    labelFeatures, ruleFeatures)

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, String](feat,
      reannotate,
      xbarGrammar,
      dummyFeats,
      trees)

    val featureCounter = if(oldWeights ne null) {
      readObject[Counter[Feature, Double]](oldWeights)
    } else {
      Counter[Feature, Double]()
    }


    new SpanModel[AnnotatedLabel, String](indexed, indexed.index, reannotate _, cFactory, xbarGrammar, xbarLexicon, {
      featureCounter.get(_)
    })
  }
}
