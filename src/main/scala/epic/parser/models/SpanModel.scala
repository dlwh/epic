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
import epic.parser.projections.{ConstraintCoreGrammarAdaptor, GrammarRefinements}
import breeze.config.Help
import epic.lexicon.Lexicon
import epic.features._
import epic.features.HashFeature
import epic.constraints.ChartConstraints
import epic.util.CacheBroker
import epic.constraints.ChartConstraints.Factory
import epic.parser.features.RuleFeature
import epic.parser.features.LabelFeature
import epic.trees.annotations.FilterAnnotations

/**
 * A rather more sophisticated discriminative parser. Uses features on
 * the underlying span.
 * @author dlwh
 */
@SerialVersionUID(1L)
class SpanModel[L, L2, W](featurizer: RefinedFeaturizer[L, W, Feature],
                          val featureIndex: Index[Feature],
                          ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                          baseFactory: ChartConstraints.Factory[L, W],
                          val baseGrammar: BaseGrammar[L],
                          val lexicon: Lexicon[L, W],
                          val refinedGrammar: BaseGrammar[L2],
                          val refinements: GrammarRefinements[L, L2],
                          initialFeatureVal: (Feature => Option[Double]) = {
                            _ => None
                          }) extends ParserModel[L, W] with Serializable {
  type Inference = AnnotatedParserInference[L, W]

  override def initialValueForFeature(f: Feature) = initialFeatureVal(f) getOrElse 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    val factory = new DotProductGrammar(baseGrammar, lexicon, refinedGrammar, refinements, weights, featurizer)
    def reannotate(bt: BinarizedTree[L], words: IndexedSeq[W]) = {
      val annotated = ann(bt, words)

      val localized = annotated.map { l =>
        refinements.labels.project(l) -> refinements.labels.localize(l)
      }

      localized
    }
    new AnnotatedParserInference(featurizer, reannotate, factory, new ConstraintCoreGrammarAdaptor(baseGrammar, lexicon, baseFactory))
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts) = {
    ecounts.loss -> ecounts.counts
  }
}


class DotProductGrammar[L, L2, W, Feature](val grammar: BaseGrammar[L],
                                       val lexicon: Lexicon[L, W],
                                       val refinedGrammar: BaseGrammar[L2],
                                       val refinements: GrammarRefinements[L, L2],
                                       val weights: DenseVector[Double],
                                       val featurizer: RefinedFeaturizer[L, W, Feature]) extends RefinedGrammar[L, W] {


  // rule -> parentRef -> [ruleRef]
  private val parentCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    val parent = grammar.parent(r)
    val parentRefs = Array.fill(refinements.labels.refinementsOf(parent).length){ArrayBuffer[Int]()}
    for(ruleRef <- refinements.rules.refinementsOf(r)) {
      val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).parent)
      parentRefs(refParent) += refinements.rules.localize(ruleRef)
    }
    parentRefs.map(_.toArray)
  }

  private val leftChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val leftChild = grammar.leftChild(r)
      val leftChildRefs = Array.fill(refinements.labels.refinementsOf(leftChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].left)
        leftChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      leftChildRefs.map(_.toArray)
    }
  }

  private val rightChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val rightChild = grammar.rightChild(r)
      val rightChildRefs = Array.fill(refinements.labels.refinementsOf(rightChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].right)
        rightChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      rightChildRefs.map(_.toArray)
    }
  }

  // rule -> parentRef -> [ruleRef]
  private val childCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      val child = grammar.child(r)
      val childRefs = Array.fill(refinements.labels.refinementsOf(child).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refChild = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[UnaryRule[L2]].child)
        childRefs(refChild) += refinements.rules.localize(ruleRef)
      }
      childRefs.map(_.toArray)
    } else {
      null
    }
  }

  private val coarseRulesGivenParentRefinement = Array.tabulate(grammar.labelIndex.size) { p =>
  // refinement -> rules
    val result = Array.fill(refinements.labels.refinementsOf(p).size)(ArrayBuffer[Int]())
    for(r <- grammar.indexedBinaryRulesWithParent(p); ref <- 0 until result.length) {
      if(parentCompatibleRefinements(r)(ref).nonEmpty) {
        result(ref) += r
      }
    }

    result.map(_.toArray)
  }

  private val parentRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
  // rules -> parent refinements
    refinements.rules.refinementsOf(r).map(refinedGrammar.parent(_)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val leftChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.leftChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val rightChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.rightChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }


  def anchor(w: IndexedSeq[W]):RefinedAnchoring[L, W] = new RefinedAnchoring[L, W] {


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

    def validLabelRefinements(begin: Int, end: Int, label: Int) = {
      refinements.labels.localRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      parentCompatibleRefinements(rule)(parentRef)
    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
      leftChildCompatibleRefinements(rule)(childRef)
    }

    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      rightChildCompatibleRefinements(rule)(childRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      childCompatibleRefinements(rule)(childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.child(refinedRuleId))
    }

    // TODO: make this not terminally slow!
    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      val a = grammar.parent(r)
      val b = grammar.child(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), grammar.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      if(refinedRuleIndex < 0) {
        -1
      } else {
        refinements.rules.localize(refinedRuleIndex)
      }
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val a = grammar.parent(r)
      val b = grammar.leftChild(r)
      val c = grammar.rightChild(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val c2 = refinements.labels.globalize(c, refC)
      refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      ))  )
    }

    def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
    def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = coarseRulesGivenParentRefinement(a)(refA)

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = parentRefinementsGivenCoarseRule(rule)
    def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
      leftChildRefinementsGivenCoarseRule(rule)
    }
    def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
      rightChildRefinementsGivenCoarseRule(rule)
    }
  }
}

trait SpanFeaturizer[L, W] extends Serializable {
  def featureIndex: Index[Feature]
  def anchor(words: IndexedSeq[W]): Anchoring

  /**
   * Specialization assumes that features are of several kinds, so that we can efficiently cache them.
   */
  trait Anchoring {
    def words: IndexedSeq[W]
    def featuresForSpan(begin: Int, end: Int, label: Int): Array[Int]
    def featuresForRule(begin: Int, end: Int, rule: Int):Array[Int]
    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int):Array[Int]
    def featuresForSplit(begin: Int, split: Int, end: Int, rule: Int):Array[Int]
  }

}


class IndexedSpanFeaturizer[L, L2, W](val index: FeatureIndex[Feature],
                                  f: SpanFeaturizer[L2, W],
                                  surfaceFeaturizer: IndexedSurfaceFeaturizer[W],
                                  refinements: GrammarRefinements[L, L2],
                                  grammar: BaseGrammar[L]) extends RefinedFeaturizer[L, W, Feature] with Serializable {


  def anchor(words: IndexedSeq[W]):Anchoring = new Spec(words)

  case class Spec(words: IndexedSeq[W]) extends super.Anchoring {
    def length = words.length
    private val fspec = f.anchor(words)
    private val wspec = surfaceFeaturizer.anchor(words)

    def featuresForSpan(begin: Int, end: Int, tag: Int, ref: Int) = {
      val globalized = refinements.labels.globalize(tag, ref)

      val ind = TriangularArray.index(begin, end)
      var rcache = spanCache(ind)
      if(rcache eq null) {
        rcache = new OpenAddressHashArray[Array[Int]](refinements.labels.fineIndex.size)
        spanCache(ind) = rcache
      }
      var cache = rcache(globalized)
      if(cache == null)  {
        cache = index.crossProduct(fspec.featuresForSpan(begin, end, globalized), if(end-begin == 1) wspec.featuresForWord(begin) else wspec.featuresForSpan(begin, end))
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
        cache = index.crossProduct(fspec.featuresForRule(begin, end, globalized), wspec.featuresForSpan(begin, end))
        rcache(globalized) = cache
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
        scache = new OpenAddressHashArray[Array[Int]](refinements.rules.fineIndex.size)
        rcache(split - begin) = scache
      }
      var cache = scache(globalized)
      if(cache == null)  {
        cache = (
          index.crossProduct(fspec.featuresForBinaryRule(begin, split, end, globalized), wspec.featuresForSpan(begin, end))
            ++ index.crossProduct(fspec.featuresForSplit(begin, split, end, globalized), wspec.featuresForWord(split, FeaturizationLevel.BasicFeatures))
          )
        scache(globalized) = cache
      }
      cache

    }

    // caches:
    // (begin,end) -> label ->  Array[Int]
    val spanCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (begin,end) -> rule -> Array[Int]
    val unaryCache = TriangularArray.raw[OpenAddressHashArray[Array[Int]]](length + 1, null)
    // (begin, end) -> (split - begin) -> Array[Int]
    val binaryCache = TriangularArray.raw[Array[OpenAddressHashArray[Array[Int]]]](length + 1, null)
  }

}

object IndexedSpanFeaturizer {
  def extract[L, L2, W](featurizer: SpanFeaturizer[L2, W],
                        surfaceFeaturizer: IndexedSurfaceFeaturizer[W],
                    ann: (BinarizedTree[L], IndexedSeq[W]) => BinarizedTree[L2],
                    refinements: GrammarRefinements[L, L2],
                    grammar: BaseGrammar[L],
                    dummyFeatScale: HashFeature.Scale,
                    trees: Traversable[TreeInstance[L, W]]): IndexedSpanFeaturizer[L, L2, W] = {

    val labelIndex = refinements.labels.fineIndex
    val ruleIndex = refinements.rules.fineIndex

    val fi = FeatureIndex.build(featurizer.featureIndex, surfaceFeaturizer.featureIndex, dummyFeatScale) { enumerator =>
      trees.foreach { ti =>
        val spec = featurizer.anchor(ti.words)
        val wspec = surfaceFeaturizer.anchor(ti.words)
        ann(ti.tree, ti.words).allChildren.foreach {
          case NullaryTree(a, span) =>
            val aI = labelIndex(a)
            enumerator(spec.featuresForSpan(span.begin, span.end, aI), wspec.featuresForWord(span.begin))
          case UnaryTree(a, b, chain, span) =>
            val r = ruleIndex(UnaryRule(a, b.label, chain))
            enumerator(spec.featuresForRule(span.begin, span.end, r), wspec.featuresForSpan(span.begin, span.end))
          case t@BinaryTree(a, b, c, span) =>
            val aI = labelIndex(a)
            val r = ruleIndex(BinaryRule(a, b.label, c.label))
            enumerator(spec.featuresForBinaryRule(span.begin, t.splitPoint, span.end, r), wspec.featuresForSpan(span.begin, span.end))
            enumerator(spec.featuresForSpan(span.begin, span.end, aI), wspec.featuresForSpan(span.begin, span.end))
            enumerator(spec.featuresForSplit(span.begin, t.splitPoint, span.end, r), wspec.featuresForWord(t.splitPoint,FeaturizationLevel.BasicFeatures))
        }

      }
    }

    new IndexedSpanFeaturizer(fi, featurizer, surfaceFeaturizer, refinements, grammar)
  }
}

case object SplitPointFeature extends Feature

class StandardSpanFeaturizer[L, W](grammar: BaseGrammar[L],
                                   labelFeatures: L => Array[Feature],
                                   ruleFeatures: Rule[L]=>Array[Feature]) extends SpanFeaturizer[L, W] {

  private val _featureIndex= Index[Feature]()
  def featureIndex : Index[Feature] = _featureIndex
  import grammar._
  def ruleIndex = index

  private val ruleCache = Encoder.fromIndex(ruleIndex).tabulateArray(r => ruleFeatures(r).map(_featureIndex.index(_)).toArray)
  private val labelCache: Array[Array[Int]] = Encoder.fromIndex(labelIndex).tabulateArray(l => labelFeatures(l).map(_featureIndex.index(_)).toArray)
  private val splitFeatures = Array(_featureIndex.index(SplitPointFeature))


  def anchor(w: IndexedSeq[W]):Anchoring = new Anchoring {
    def words = w
    val length = w.length

    def featuresForSpan(begin: Int, end: Int, label: Int) = {
      labelCache(label)
    }

    def featuresForRule(begin: Int, end: Int, rule: Int) = {
      ruleCache(rule)
    }

    def featuresForBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
      ruleCache(rule)
    }

    def featuresForSplit(begin: Int, split: Int, end: Int, rule: Int):Array[Int] = {
      splitFeatures
    }
  }
}

case class SpanModelFactory(baseParser: ParserParams.XbarGrammar,
                            @Help(text=
                              """The kind of annotation to do on the refined grammar. Default uses no annotations.
You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                              """)
                            annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                            @Help(text="Old weights to initialize with. Optional")
                            oldWeights: File = null,
                            @Help(text="For features not seen in gold trees, we bin them into dummyFeats * numGoldFeatures bins using hashing.")
                            dummyFeats: Double = 0.5,
                            @Help(text="How common must a feature be before we remember it?")
                            minFeatCutoff: Int = 1) extends ParserModelFactory[AnnotatedLabel, String] {
  type MyModel = SpanModel[AnnotatedLabel, AnnotatedLabel, String]


  def make(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]], constrainer: Factory[AnnotatedLabel, String])(implicit broker: CacheBroker) = {
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))
    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)
    val refGrammar = BaseGrammar(AnnotatedLabel.TOP, annBinaries, annUnaries)

    val trees = trainTrees.map(_.mapLabels(_.baseAnnotatedLabel))
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trees)
    val summedCounts = sum(initLexicon, Axis._0)

    val indexedRefinements = GrammarRefinements(xbarGrammar, refGrammar, (_: AnnotatedLabel).baseAnnotatedLabel)

    def labelFeatures(ann: AnnotatedLabel) = Array[Feature](LabelFeature(ann))
    def ruleFeatures(ann: Rule[AnnotatedLabel]) = Array[Feature](RuleFeature(ann))

    val surface = IndexedSurfaceFeaturizer.fromData(new ContextSurfaceFeaturizer(new StandardSurfaceFeaturizer(summedCounts)), annTrees.map{_.words}, constrainer)
    val feat = new StandardSpanFeaturizer[AnnotatedLabel, String](
      refGrammar,
      labelFeatures _, ruleFeatures _)


    val indexed =  IndexedSpanFeaturizer.extract[AnnotatedLabel, AnnotatedLabel, String](feat,
      surface,
      annotator,
      indexedRefinements,
      xbarGrammar,
      HashFeature.Relative(dummyFeats),
      trees)

    val featureCounter = readWeights(oldWeights)

    new SpanModel[AnnotatedLabel, AnnotatedLabel, String](indexed, indexed.index, annotator, constrainer, xbarGrammar, xbarLexicon, refGrammar, indexedRefinements,featureCounter.get(_))
  }
}
