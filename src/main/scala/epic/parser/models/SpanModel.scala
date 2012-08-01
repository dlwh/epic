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

import features._
import breeze.collection.mutable.{TriangularArray, OpenAddressHashArray}
import breeze.linalg._
import epic.trees._
import annotations.{FilterAnnotations, TreeAnnotator}
import collection.mutable.ArrayBuilder
import java.io.File
import breeze.util._
import epic.framework.Feature
import projections.GrammarRefinements

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SpanModel[L, W](featurizer: RefinedFeaturizer[L, W, Feature],
                      val featureIndex: Index[Feature],
                      ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[L],
                      baseFactory: CoreGrammar[L, W],
                      grammar: BaseGrammar[L],
                      lexicon: Lexicon[L, W],
                      initialFeatureVal: (Feature => Option[Double]) = {
                        _ => None
                      }) extends ParserModel[L, W] with Serializable {
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
  def anchor(w: Seq[W]):RefinedAnchoring[L, W] = LiftedCoreAnchoring(new CoreAnchoring[L, W] {

    val grammar = DotProductGrammar.this.grammar
    val lexicon = DotProductGrammar.this.lexicon

    def words = w

    val fspec = featurizer.anchor(w)
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
  def anchor(words: Seq[W]): Anchoring

  /**
   * Specialization assumes that features are of several kinds, so that we can efficiently cache them.
   */
  trait Anchoring {
    def words: Seq[W]
    def featuresForSpan(begin: Int, end: Int, label: Int): Array[Feature]
    def featuresForRule(begin: Int, end: Int, rule: Int): Array[Feature]
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Array[Feature]
  }

}


class IndexedSpanFeaturizer[L, L2, W](f: SpanFeaturizer[L2, W],
                                  refinements: GrammarRefinements[L, L2],
                                  grammar: BaseGrammar[L],
                                  val trueFeatureIndex: Index[Feature],
                                  dummyFeatures: Int) extends RefinedFeaturizer[L, W, Feature] with Serializable {
  val index:Index[Feature] = {
    val r = Index[Feature]()
    (trueFeatureIndex) foreach (r.index(_))
    (0 until dummyFeatures) map {HashFeature(_)} foreach {r.index _}
    r
  }

  def anchor(words: Seq[W]):Anchoring = new Spec(words)

  case class Spec(words: Seq[W]) extends super.Anchoring {
    def length = words.length
    private val fspec = f.anchor(words)

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      val globalized = refinements.labels.globalize(tag, ref)

      val ind = TriangularArray.index(begin, end)
      var rcache = spanCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](grammar.labelIndex.size)
        spanCache(ind) = rcache
      }
      var cache = rcache(tag)
      if(cache == null)  {
        cache = stripEncode(index, fspec.featuresForSpan(begin, end, globalized))
        rcache(tag) = cache
      }
      cache
    }

    def featuresForUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      val globalized = refinements.rules.globalize(rule, ref)
      val ind = TriangularArray.index(begin, end)
      var rcache = unaryCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](grammar.index.size)
        unaryCache(ind) = rcache
      }
      var cache = rcache(rule)
      if(cache == null)  {
        cache = stripEncode(index, fspec.featuresForRule(begin, end, globalized))
        rcache(rule) = cache
      }
      cache
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      val globalized = refinements.rules.globalize(rule, ref)
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
        cache = stripEncode(index, fspec.featuresForBinaryRule(begin, split, end, globalized)) ++ featuresForUnaryRule(begin, end, globalized, globalized)
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
  def extract[L, L2, W](featurizer: SpanFeaturizer[L2, W],
                    ann: (BinarizedTree[L], Seq[W]) => BinarizedTree[L2],
                    refinements: GrammarRefinements[L, L2],
                    grammar: BaseGrammar[L],
                    dummyFeatScale: Double,
                    trees: Traversable[TreeInstance[L, W]]): IndexedSpanFeaturizer[L, L2, W] = {

    val labelIndex = refinements.labels.fineIndex
    val ruleIndex = refinements.rules.fineIndex

    def add(ctr: Counter[Feature, Int], feats: Array[Feature]) {
      for (f <- feats) {
        ctr(f) += 1
      }
    }
    val goldFeatures = trees.par.aggregate(null: Counter[Feature, Int])( {(feats, ti) =>
      val set = if(feats eq null) Counter[Feature, Int]() else feats
      val spec = featurizer.anchor(ti.words)
      def rec(t: BinarizedTree[L2]):Unit = t match {
        case NullaryTree(a, span) =>
          val aI = labelIndex(a)
          add(set, spec.featuresForSpan(span.start, span.end, aI))
        case UnaryTree(a, b, chain, span) =>
          val r = ruleIndex(UnaryRule(a, b.label, chain))
          rec(b)
          add(set, spec.featuresForRule(span.start, span.end, r))
        case BinaryTree(a, b, c, span) =>
          rec(b)
          rec(c)
          val r = ruleIndex(BinaryRule(a, b.label, c.label))
          val aI = labelIndex(a)
          add(set, spec.featuresForSpan(span.start, span.end, aI))
          add(set, spec.featuresForRule(span.start, span.end, r))
          add(set, spec.featuresForBinaryRule(span.start, b.span.end, span.end, r))
      }
      rec(ann(ti.tree, ti.words))
      set
    }, {(a, b) => a += b})

    val goldFeatureIndex = Index[Feature]()
    for( (f, v) <- goldFeatures.activeIterator) {
       goldFeatureIndex.index(f)
    }
    println(goldFeatureIndex.size)

    new IndexedSpanFeaturizer(featurizer, refinements, grammar, goldFeatureIndex, (goldFeatureIndex.size * dummyFeatScale).toInt)
  }
}

class StandardSpanFeaturizer[L, W](grammar: BaseGrammar[L],
                                   wordGen: W=>Traversable[String],
                                   labelFeatures: Array[Array[Feature]],
                                   ruleFeatures: Array[Array[Feature]]) extends SpanFeaturizer[L, W] {
  def anchor(w: Seq[W]):Anchoring = new Anchoring {
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
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                            constraints: ParserParams.Constraints[AnnotatedLabel, String],
                            oldWeights: File = null,
                            dummyFeats: Double = 0.5,
                            minFeatCutoff: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = SpanModel[AnnotatedLabel, String]

  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]]) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trees)
    val summedCounts = sum(initLexicon, Axis._0)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, {
      (_: AnnotatedLabel).baseAnnotatedLabel
    })

    val baseFactory = RefinedGrammar.generative[AnnotatedLabel, String](xbarGrammar,
      xbarLexicon,
      initBinaries, initUnaries, initLexicon)
    val cFactory = constraints.cachedFactory(AugmentedGrammar.fromRefined(baseFactory))

    val labelFeatures = xbarGrammar.labelEncoder.tabulateArray(l => Array(LabelFeature(l):Feature))
    val ruleFeatures = xbarGrammar.tabulateArray(l => Array(RuleFeature(l):Feature))

    val feat = new StandardSpanFeaturizer[AnnotatedLabel, String](
      xbarGrammar,
      shapeGen,
      labelFeatures, ruleFeatures)


    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](feat,
      annotator,
      indexedRefinements,
      xbarGrammar,
      dummyFeats,
      trees)

    val featureCounter = readWeights(oldWeights)

    new SpanModel[AnnotatedLabel, String](indexed, indexed.index, annotator, cFactory, xbarGrammar, xbarLexicon, {
      featureCounter.get(_)
    })
  }
}
