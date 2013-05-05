package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import epic.sequences.SemiCRF.{SpanConstraints, TransitionVisitor}
import collection.mutable.ArrayBuffer
import epic.parser.features.{SpanShapeGenerator}
import breeze.collection.mutable.TriangularArray
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges
import collection.mutable
import breeze.features.FeatureVector
import epic.features.{IndexedSpanFeaturizer, IndexedWordFeaturizer}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         maxSegmentLength: Int=>Int,
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0},
                         cacheFeatures: Boolean = false) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def extractCRF(weights: DenseVector[Double]) = {
    val grammar = inferenceFromWeights(weights)
    new SemiCRF(grammar)
  }

  type Inference = SemiCRFInference[L, W]
  type Marginal = SemiCRF.Marginal[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  val compressedFeatureCache = mutable.Map.empty[IndexedSeq[W], CompressedFeatureCache]

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new SemiCRFInference(weights, featureIndex, featurizer, maxSegmentLength)

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]): BIEOAnchoredFeaturizer[L, W]
  }

  trait BIEOAnchoredFeaturizer[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

    def canStartRealSpan(beg: Int):Boolean
    def canBeInterior(pos: Int):Boolean
    def featuresForBegin(prev: Int, cur: Int, pos: Int):FeatureVector
    def featuresForEnd(cur: Int, pos: Int):FeatureVector
    def featuresForInterior(cur: Int, pos: Int):FeatureVector
    def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int):FeatureVector

    def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int): FeatureVector = {
      val acc = new ArrayBuffer[FeatureVector]()
      val _begin = featuresForBegin(prev, cur, start)
      acc += _begin
      val _end = featuresForEnd(cur, end)
      acc += _end
      var p = start+1
      while (p < end) {
        val w = featuresForInterior(cur, p)
        acc += w
        p += 1
      }

      val forSpan = featuresForSpan(prev, cur, start, end)
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
                             val maxLength: Int=>Int) extends AugmentableInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] with SemiCRF.Grammar[L, W] with Serializable {
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
    SemiCRF.Marginal(new Anchoring(v.words, aug))
  }

  def goldMarginal(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]): SemiCRF.Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal[L, W](new Anchoring(v.words, augment), v.segments)
  }

  def countsFromMarginal(v: Segmentation[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def apply(prev: Int, cur: Int, start: Int, end: Int, count: Double) {
        import localization._
        axpy(count, featuresForBegin(prev, cur, start), counts.counts)
        axpy(count, featuresForEnd(cur, end), counts.counts)
        var p = start+1
        while (p < end) {
          axpy(count, featuresForInterior(cur, p), counts.counts)
          p += 1
        }

        axpy(count, featuresForSpan(prev, cur, start, end), counts.counts)
      }
    }
    marg.visit(visitor)
    counts

  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = new IdentityAnchoring(v.words)

  class IdentityAnchoring(val words: IndexedSeq[W]) extends SemiCRF.Anchoring[L, W] {
    def maxSegmentLength(l: Int): Int = maxLength(l)

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol
  }

  class Anchoring(val words: IndexedSeq[W], augment: SemiCRF.Anchoring[L, W]) extends SemiCRF.Anchoring[L, W] {
    val localization = featurizer.anchor(words)
    def maxSegmentLength(l: Int): Int = SemiCRFInference.this.maxLength(l)

    val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, length){ (p,c,w) =>
      val f = localization.featuresForBegin(p, c, w)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }
    val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      val f = localization.featuresForEnd(l, w + 1)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }
    val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      val f = localization.featuresForInterior(l, w)
      if (f eq null) Double.NegativeInfinity
      else weights dot f
    }


    private def okSpan(beg: Int, end: Int) = {
      var ok = localization.canStartRealSpan(beg)
      var pos = beg + 1
      while(pos < end && ok) {
        ok = localization.canBeInterior(pos)
        pos += 1
      }
      ok
    }

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      if (beg + 1 != end && !okSpan(beg, end)) {
        Double.NegativeInfinity
      } else {
        var score = augment.scoreTransition(prev, cur, beg, end)
        if (score != Double.NegativeInfinity) {
          val spanScore: Double = cachedSpanScore(prev, cur, beg, end)
          score += spanScore
          if (score != Double.NegativeInfinity) {
            score += beginCache(prev)(cur)(beg)
            score += endCache(cur)(end-1)
            var pos = beg + 1
            while (pos < end) {
              score += wordCache(cur)(pos)
              pos += 1
            }
          }
        }
        score
      }
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
                                  pruningModel: Option[SemiCRF.ConstraintGrammar[L, String]] = None,
                                  gazetteer: Gazetteer[Any, String] = Gazetteer.empty[String, String],
                                  weights: Feature=>Double = { (f:Feature) => 0.0}) {

  import SegmentationModelFactory._

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol) ++ train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    println(maxLengthMap)

    val starters = collection.mutable.Set[String]()
    val interiors = collection.mutable.Set[String]()
    for (t <- train; seg <- t.segments if seg._1 != outsideSymbol) {
      starters += t.words(seg._2.start)
      interiors ++= seg._2.map(t.words)
    }


    val counts: Counter2[BIOETag[L], String, Double] = Counter2.count(train.map(_.asBIOSequence(outsideSymbol)).map{seg => seg.label zip seg.words}.flatten).mapValues(_.toDouble)
    val wordCounts:Counter[String, Double] = sum(counts,Axis._0)
    val nonStarters = wordCounts.activeIterator.filter(_._2 > 5).map(_._1).toSet -- starters
    println(nonStarters)
    val nonInteriors = wordCounts.activeIterator.filter(_._2 > 8).map(_._1).toSet -- interiors
    println(nonInteriors)
    val allowedSpanClassifier = pruningModel.map(cg => {(seg: Segmentation[L, String]) =>
      val cons:SpanConstraints = cg.constraints(seg);
      {(b:Int,e:Int) => cons.spanAllowed(b, e)}
    }).getOrElse((seg: Segmentation[L, String]) =>(b:Int,e:Int)=>true)
    val trainWithAllowedSpans = train.map(seg => seg.words -> allowedSpanClassifier(seg))
    val f = IndexedSpanFeaturizer.forTrainingSet(trainWithAllowedSpans, counts, gazetteer)

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = new IndexedStandardFeaturizer[L](f, startSymbol, nonStarters, nonInteriors, labelIndex, maxLengthArray(_), pruningModel)
    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray, weights(_))

    model
  }

}

object SegmentationModelFactory {
  case class Label1Feature[L](label: L, f: Feature, kind: Symbol) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature

  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer[L](f: IndexedSpanFeaturizer,
                                     val startSymbol: L,
                                     val nonStarters: Set[String],
                                     val nonInteriors: Set[String],
                                     val labelIndex: Index[L],
                                     val maxLength: Int=>Int,
                                     val pruningModel: Option[SemiCRF.ConstraintGrammar[L, String]] = None) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable {

    def baseWordFeatureIndex = f.wordFeatureIndex
    def baseSpanFeatureIndex = f.spanFeatureIndex

    val kinds = Array('Begin, 'Interior, 'End)

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


    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOAnchoredFeaturizer[L, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[L, String] {
      val constraints = pruningModel.map(_.constraints(w))
      val loc = f.anchor(w)
      def length = w.length

      val starters = Array.tabulate(w.length)(i => !nonStarters(w(i)))
      val interiors = Array.tabulate(w.length)(i => !nonInteriors(w(i)))

      def canStartRealSpan(beg: Int): Boolean = starters(beg)
      def canBeInterior(i: Int): Boolean = interiors(i)


      private def okSpan(beg: Int, end: Int) = (beg < length) && {
        (beg + 1 == end) || {
          var ok = canStartRealSpan(beg)
          var pos = beg + 1
          while(pos < end && ok) {
            ok = canBeInterior(pos)
            pos += 1
          }
          ok
        }
      }

      def featureIndex: Index[Feature] = IndexedStandardFeaturizer.this.featureIndex

      val beginCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
        new FeatureVector(loc.featuresForWord(w).map(wordFeatures(l)(0)(_)))
      }
      val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
        new FeatureVector(loc.featuresForWord(w).map(wordFeatures(l)(1)(_)))
      }
      val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
        new FeatureVector(loc.featuresForWord(w).map(wordFeatures(l)(2)(_)))
      }

      val spanCache = TriangularArray.tabulate(length+1){ (b, e) =>
        if(okSpan(b, e)) {
          val f = loc.featuresForSpan(b, e)
          Array.tabulate(labelIndex.size)(l => new FeatureVector(f.map(spanFeatures(l))))
        } else {
          null
        }

      }

      def featuresForBegin(prev: Int, cur: Int, pos: Int): FeatureVector = beginCache(cur)(pos)

      def featuresForEnd(cur: Int, pos: Int): FeatureVector = endCache(cur)(pos-1)

      def featuresForInterior(cur: Int, pos: Int): FeatureVector = wordCache(cur)(pos)

      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): FeatureVector = {
        new FeatureVector(spanCache(beg, end)(cur).data :+ transitionFeatures(prev)(cur))
      }
    }
  }


}
