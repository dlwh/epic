package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import epic.sequences.SemiCRF.TransitionVisitor
import collection.mutable.ArrayBuffer
import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import epic.features.IndexedSpanFeaturizer
import epic.constraints.{SpanConstraints, TagConstraints, LabeledSpanConstraints}
import epic.constraints.LabeledSpanConstraints.NoConstraints
import epic.lexicon.{SimpleLexicon, Lexicon}
import epic.trees.AnnotatedLabel

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         val lexicon: Lexicon[L, W],
                         maxSegmentLength: Array[Int],
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0},
                         cacheFeatures: Boolean = false) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def extractCRF(weights: DenseVector[Double]) = {
    inferenceFromWeights(weights)
  }

  type Inference = SemiCRFInference[L, W]
  type Marginal = SemiCRF.Marginal[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new SemiCRFInference(weights, featureIndex, featurizer, lexicon, maxSegmentLength)

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]): BIEOAnchoredFeaturizer[L, W]
  }

  trait BIEOAnchoredFeaturizer[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

    def featuresForBegin(prev: Int, cur: Int, pos: Int):FeatureVector
    def featuresForInterior(cur: Int, pos: Int):FeatureVector
    def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int):FeatureVector

    def featuresForTransition(prev: Int, cur: Int, begin: Int, end: Int): FeatureVector = {
      val acc = new ArrayBuffer[FeatureVector]()
      val _begin = featuresForBegin(prev, cur, begin)
      acc += _begin
      var p = begin+1
      while (p < end) {
        val w = featuresForInterior(cur, p)
        acc += w
        p += 1
      }

      val forSpan = featuresForSpan(prev, cur, begin, end)
      acc += forSpan

      val result = acc.foldLeft(Array.empty[Int])(_ ++ _.data)
      new FeatureVector(result)
    }
  }
}

@SerialVersionUID(1)
class SemiCRFInference[L, W](weights: DenseVector[Double],
                             featureIndex: Index[Feature],
                             featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                             val lexicon: Lexicon[L, W],
                             val maxLength: Array[Int]) extends AugmentableInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] with SemiCRF[L, W] with Serializable {
  def viterbi(sentence: IndexedSeq[W], anchoring: SemiCRF.Anchoring[L, W]): Segmentation[L, W] = {
    SemiCRF.viterbi(new Anchoring(sentence, anchoring))
  }


  type Marginal = SemiCRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def emptyCounts = StandardExpectedCounts.zero(this.featureIndex)

  def anchor(w: IndexedSeq[W]) = new Anchoring(w, new IdentityAnchoring(w))


  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def marginal(v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    val m = SemiCRF.Marginal(new Anchoring(v.words, aug))
    m
  }

  def goldMarginal(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]): SemiCRF.Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal[L, W](new Anchoring(v.words, augment), v.segments)
  }

  def countsFromMarginal(v: Segmentation[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double) {
        import localization._
        axpy(count * scale, featuresForBegin(prev, cur, begin), counts.counts)
        var p = begin+1
        while (p < end) {
          axpy(count * scale, featuresForInterior(cur, p), counts.counts)
          p += 1
        }

        axpy(count * scale, featuresForSpan(prev, cur, begin, end), counts.counts)
      }
    }
    marg.visit(visitor)
    counts

  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = new IdentityAnchoring(v.words)

  class IdentityAnchoring(val words: IndexedSeq[W], val constraints: LabeledSpanConstraints[L]) extends SemiCRF.Anchoring[L, W] {
    def this(words: IndexedSeq[W]) = this(words, LabeledSpanConstraints.layeredFromTagConstraints(lexicon.anchor(words), maxLength))

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol

    def canStartLongSegment(pos: Int): Boolean = true

  }

  class Anchoring(val words: IndexedSeq[W],
                  val constraints: LabeledSpanConstraints[L],
                  val tagsConstraints: TagConstraints[L],
                  augment: SemiCRF.Anchoring[L, W]) extends SemiCRF.Anchoring[L, W] {
    def this(words: IndexedSeq[W], aug: SemiCRF.Anchoring[L, W]) = this(words, LabeledSpanConstraints.layeredFromTagConstraints(lexicon.anchor(words), maxLength), lexicon.anchor(words), aug)
    val localization = featurizer.anchor(words)

    val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, length){ (p,c,w) =>
      val f = localization.featuresForBegin(p, c, w)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }
    val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      val f = localization.featuresForInterior(l, w)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }


    private def okSpan(beg: Int, end: Int, cur: Int) = (end - beg <= maxLength(cur)) && isValidSegment(beg, end)

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      val score = if (beg + 1 != end && !okSpan(beg, end, cur)) {
        Double.NegativeInfinity
      } else {
        var score = augment.scoreTransition(prev, cur, beg, end)
        if (score != Double.NegativeInfinity) {
          val spanScore: Double = cachedSpanScore(prev, cur, beg, end)
          score += spanScore
          if (score != Double.NegativeInfinity) {
            score += beginCache(prev)(cur)(beg)
            var pos = beg + 1
            while (pos < end) {
              score += wordCache(cur)(pos)
              pos += 1
            }
          }
        }
        score
      }
      score
    }

    private val spanCache = new Array[Array[Array[Double]]](TriangularArray.arraySize(length+1))

    private def cachedSpanScore(prev: Int, cur: Int, beg: Int, end: Int):Double = {
      val tind: Int = TriangularArray.index(beg, end)
      var cc = spanCache(tind)
      if(spanCache(tind) == null) {
        cc = new Array[Array[Double]](labelIndex.size)
        spanCache(tind) = cc
      }

      var xx = cc(cur)
      if(xx == null) {

        val span = localization.featuresForSpan(prev, cur, beg, end)
        if (span eq null) {
          cc(cur) = negInfArray
          Double.NegativeInfinity
        } else {
          xx = java.util.Arrays.copyOf(nanArray, nanArray.length)
          xx(prev) = weights dot span
          cc(cur) = xx
          xx(prev)
        }
      } else {
        if (java.lang.Double.isNaN(xx(prev))) {
          val span = localization.featuresForSpan(prev, cur, beg, end)
          xx(prev) = weights dot span
        }
        xx(prev)
      }
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }



  private val negInfArray = Array.fill(labelIndex.size)(Double.NegativeInfinity)
  private val nanArray = Array.fill(labelIndex.size)(Double.NaN)



  def posteriorDecode(m: Marginal):Segmentation[L, W] = {
    SemiCRF.posteriorDecode(m)
  }
}

class SegmentationModelFactory[L](val startSymbol: L,
                                  val outsideSymbol: L,
                                  pruningModel: Option[SemiCRF.ConstraintSemiCRF[L, String]] = None,
                                  gazetteer: Gazetteer[Any, String] = Gazetteer.empty[String, String],
                                  weights: Feature=>Double = { (f:Feature) => 0.0}) {

  import SegmentationModelFactory._

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol, outsideSymbol) ++ train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    println(maxLengthMap)


    val counts: Counter2[L, String, Double] = Counter2.count(train.map(_.asFlatTaggedSequence(outsideSymbol)).map{seg => seg.label zip seg.words}.flatten).mapValues(_.toDouble)
    val lexicon = new SimpleLexicon(labelIndex, counts, openTagThreshold = 10, closedWordThreshold = 20)

    val allowedSpanClassifier = pruningModel
      .map(cg => {(seg: Segmentation[L, String]) => cg.constraints(seg) })
      .getOrElse{(seg: Segmentation[L, String]) =>
      val cons = LabeledSpanConstraints.layeredFromTagConstraints(lexicon.anchor(seg.words), maxLengthArray)
      for( (l,span) <- seg.label) {
        assert(cons.isAllowedLabeledSpan(span.start, span.end, labelIndex(l)), cons.decode(labelIndex) + " " + seg)
      }
      cons
    }
    val trainWithAllowedSpans = train.map(seg => seg.words -> allowedSpanClassifier(seg))
    val f = IndexedSpanFeaturizer.forTrainingSet(trainWithAllowedSpans, lexicon, gazetteer)

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = new IndexedStandardFeaturizer[L](f, startSymbol, lexicon, labelIndex, maxLengthArray, pruningModel)
    val model = new SemiCRFModel(indexed.featureIndex, indexed, lexicon, maxLengthArray, weights(_))

    model
  }

}

object SegmentationModelFactory {
  case class Label1Feature[L](label: L, f: Feature, kind: Symbol) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature


  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer[L](f: IndexedSpanFeaturizer[(IndexedSeq[String], LabeledSpanConstraints[L])],
                                     val startSymbol: L,
                                     val lexicon: Lexicon[L, String],
                                     val labelIndex: Index[L],
                                     val maxLength: Array[Int],
                                     val pruningModel: Option[SemiCRF.ConstraintSemiCRF[L, String]] = None) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable {

    def baseWordFeatureIndex = f.wordFeatureIndex
    def baseSpanFeatureIndex = f.spanFeatureIndex


    val kinds = Array('Begin, 'Interior)
    println(baseWordFeatureIndex.size + " " + baseSpanFeatureIndex.size)
//    println(baseSpanFeatureIndex)

    val (featureIndex: Index[Feature], wordFeatures, spanFeatures, transitionFeatures) = {
      val featureIndex = Index[Feature]()
      val labelFeatures = Array.tabulate(labelIndex.size, kinds.length, baseWordFeatureIndex.size) { (l, k, f) =>
        featureIndex.index(Label1Feature(labelIndex.get(l), baseWordFeatureIndex.get(f), kinds(k)))
      }

      val spanFeatures = Array.tabulate(labelIndex.size, baseSpanFeatureIndex.size) { (l, f) =>
        featureIndex.index(Label1Feature(labelIndex.get(l), baseSpanFeatureIndex.get(f), 'Span))
      }

      val transitionFeatures = Array.tabulate(labelIndex.size, labelIndex.size) { (l1, l2) =>
        featureIndex.index(TransitionFeature(labelIndex.get(l1), labelIndex.get(l2)))
      }

      (featureIndex, labelFeatures, spanFeatures, transitionFeatures)
    }
    println(featureIndex.size)


    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOAnchoredFeaturizer[L, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[L, String] {
      val constraints = pruningModel.map(_.constraints(w)).getOrElse{LabeledSpanConstraints.layeredFromTagConstraints(lexicon.anchor(w), maxLength)}

      val loc = f.anchor(w -> constraints)
      def length = w.length


      def featureIndex: Index[Feature] = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, l: Int, w: Int): FeatureVector = smartMap(loc.featuresForWord(w), wordFeatures(l)(0))
      def featuresForInterior(cur: Int, pos: Int): FeatureVector = smartMap(loc.featuresForWord(pos), wordFeatures(cur)(1))


      private def smartMap(rawFeats: Array[Int], mapArr: Array[Int]):FeatureVector = {
        val ret = new Array[Int](rawFeats.length)
        var i = 0
        while(i < rawFeats.length) {
          ret(i) = mapArr(rawFeats(i))
          i += 1
        }
        new FeatureVector(ret)
      }

      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): FeatureVector = {
        if (!constraints.isAllowedLabeledSpan(beg, end, cur)) {
          null
        } else {
          val f = loc.featuresForSpan(beg, end)
          val ret = new Array[Int](f.length+ 1)
          var i = 0
          while(i < f.length) {
            ret(i) = spanFeatures(cur)(f(i))
            i += 1
          }
          ret(i) = transitionFeatures(prev)(cur)
          new FeatureVector(ret)
        }
      }
    }
  }


}
