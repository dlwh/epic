package epic
package sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import epic.sequences.SemiCRF.TransitionVisitor
import collection.mutable.ArrayBuffer
import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import epic.constraints.LabeledSpanConstraints
import epic.lexicon.SimpleLexicon
import epic.features._
import epic.util.{SafeLogging, CacheBroker, NotProvided, Optional}
import com.typesafe.scalalogging.log4j.Logging
import epic.sequences.SemiCRFModel.BIEOFeatureAnchoring

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRFModel[L, W](val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         val constraintsFactory: LabeledSpanConstraints.Factory[L, W],
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0},
                         cacheFeatures: Boolean = false) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def featureIndex = featurizer.featureIndex

  def extractCRF(weights: DenseVector[Double]) = {
    inferenceFromWeights(weights)
  }

  type Inference = SemiCRFInference[L, W]
  type Marginal = SemiCRF.Marginal[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new SemiCRFInference(weights, featureIndex, featurizer, constraintsFactory)

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]): BIEOFeatureAnchoring[L, W]
  }

  trait BIEOFeatureAnchoring[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

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
                             val constraintsFactory: LabeledSpanConstraints.Factory[L, W]) extends AugmentableInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] with SemiCRF[L, W] with Serializable {
  def viterbi(sentence: IndexedSeq[W], anchoring: SemiCRF.Anchoring[L, W]): Segmentation[L, W] = {
    SemiCRF.viterbi(new Anchoring(featurizer.anchor(sentence),
      constraintsFactory.constraints(sentence),
      anchoring))
  }


  type Marginal = SemiCRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def emptyCounts = StandardExpectedCounts.zero(this.featureIndex)

  def anchor(w: IndexedSeq[W]) = {
    val constraints = constraintsFactory.constraints(w)
    new Anchoring(featurizer.anchor(w), constraints, new IdentityAnchoring(w, constraints))
  }


  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def marginal(v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    val m = SemiCRF.Marginal(new Anchoring(featurizer.anchor(v.words), constraintsFactory.constraints(v.words), aug))
    m
  }

  def goldMarginal(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]): SemiCRF.Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal[L, W](new Anchoring(featurizer.anchor(v.words), constraintsFactory.constraints(v.words), augment), v.segments)
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


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = new IdentityAnchoring(v.words, constraintsFactory.constraints(v.words))

  class IdentityAnchoring(val words: IndexedSeq[W], val constraints: LabeledSpanConstraints[L]) extends SemiCRF.Anchoring[L, W] {
    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol

    def canStartLongSegment(pos: Int): Boolean = true

  }

  class Anchoring(val localization: BIEOFeatureAnchoring[L, W],
                  _constraints: LabeledSpanConstraints[L],
                  augment: SemiCRF.Anchoring[L, W]) extends SemiCRF.Anchoring[L, W] {

    val constraints: LabeledSpanConstraints[L] = _constraints & augment.constraints

    def words: IndexedSeq[W] = augment.words

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


    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      val score = if (beg + 1 != end && !constraints.isAllowedLabeledSpan(beg, end, cur)) {
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
                                  pruningModel: Optional[SemiCRF.ConstraintSemiCRF[L, String]] = NotProvided,
                                  gazetteer: Optional[Gazetteer[Any, String]] = NotProvided,
                                  weights: Feature=>Double = { (f:Feature) => 0.0})(implicit broker: CacheBroker) extends Logging {

  import SegmentationModelFactory._

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol, outsideSymbol) ++ train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    logger.info("Maximum lengths for segments: " + maxLengthMap)


    val counts: Counter2[L, String, Double] = Counter2.count(train.map(_.asFlatTaggedSequence(outsideSymbol)).map{seg => seg.label zip seg.words}.flatten).mapValues(_.toDouble)
    val lexicon = new SimpleLexicon(labelIndex, counts, openTagThreshold = 10, closedWordThreshold = 20)

    val allowedSpanClassifier: LabeledSpanConstraints.Factory[L, String] = pruningModel.getOrElse(new LabeledSpanConstraints.LayeredTagConstraintsFactory(lexicon, maxLengthArray))
    val standardFeaturizer = new StandardSurfaceFeaturizer(sum(counts, Axis._0))
    val featurizers = gazetteer.foldLeft(IndexedSeq[SurfaceFeaturizer[String]](new ContextSurfaceFeaturizer[String](standardFeaturizer, 3, 3)))(_ :+ _)
    val featurizer = new MultiSurfaceFeaturizer[String](featurizers)
    val f = IndexedSurfaceFeaturizer.fromData(featurizer, train.map(_.words), allowedSpanClassifier)

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = IndexedStandardFeaturizer.make(f, startSymbol, labelIndex, allowedSpanClassifier)(train)
    val model = new SemiCRFModel(indexed, allowedSpanClassifier, weights(_))

    model
  }

}

object SegmentationModelFactory {
  case class Label1Feature[L](label: L, f: Feature, kind: Symbol) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature

  val kinds = Array('Begin, 'Interior, 'Span)



  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer[L] private (f: IndexedSurfaceFeaturizer[String],
                                     val featureIndex: FeatureIndex[Feature],
                                     bioeFeatures: Array[Array[Array[Int]]], // label -> kind -> indexes into f.labelFeatureIndex
                                     transitionFeatures: Array[Array[Array[Int]]], // prev -> cur -> indexes into f.labelFeatureIndex
                                     val startSymbol: L,
                                     val labelIndex: Index[L],
                                     val constraintFactory: LabeledSpanConstraints.Factory[L, String]) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable with SafeLogging {


    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOFeatureAnchoring[L, String] = new SemiCRFModel.BIEOFeatureAnchoring[L, String] {
      val constraints = constraintFactory.constraints(w)

      val loc = f.anchor(w)
      def length = w.length


      def featureIndex = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, l: Int, w: Int): FeatureVector = {
        val features = featureIndex.crossProduct(bioeFeatures(l)(0), loc.featuresForWord(w))
        new FeatureVector(features)
      }
      def featuresForInterior(cur: Int, pos: Int): FeatureVector = {
        val features = featureIndex.crossProduct(bioeFeatures(cur)(1), loc.featuresForWord(pos))
        new FeatureVector(features)
      }

      def featuresForSpan(prev: Int, cur: Int, begin: Int, end: Int): FeatureVector = {
        if (!constraints.isAllowedLabeledSpan(begin, end, cur)) {
          null
        } else {
          val features = featureIndex.crossProduct(bioeFeatures(cur)(2), loc.featuresForSpan(begin, end))
          val features2 = featureIndex.crossProduct(transitionFeatures(prev)(cur), loc.featuresForSpan(begin, end, FeaturizationLevel.MinimalFeatures))
          new FeatureVector(epic.util.Arrays.concatenate(features, features2))
        }
      }
    }
  }

  object IndexedStandardFeaturizer {
    def make[L](f: IndexedSurfaceFeaturizer[String],
                startSymbol: L,
                labelIndex: Index[L],
                constraintFactory: LabeledSpanConstraints.Factory[L, String],
                hashFeatures: HashFeature.Scale = HashFeature.Absolute(0))
               (data: IndexedSeq[Segmentation[L, String]]):IndexedStandardFeaturizer[L] = {
      val labelPartIndex = Index[Feature]()
      val bioeFeatures = Array.tabulate(labelIndex.size, kinds.length)((i,j) => Array(labelPartIndex.index(Label1Feature(labelIndex.get(i), null, kinds(j)))))
      val transitionFeatures = Array.tabulate(labelIndex.size, labelIndex.size)((i,j) => Array(labelPartIndex.index(TransitionFeature(labelIndex.get(i), labelIndex.get(j)))))

      val fi = FeatureIndex.build(labelPartIndex, f.featureIndex, hashFeatures) { addToIndex =>
        for (d <- data){
          val feats = f.anchor(d.words)
          var last = labelIndex(startSymbol)
          for ((l, span) <- d.segments) {
            val li = labelIndex(l)
            // featuresForBegin
            addToIndex(bioeFeatures(li)(0), feats.featuresForWord(span.begin))
            addToIndex(transitionFeatures(last)(li), feats.featuresForWord(span.begin, FeaturizationLevel.MinimalFeatures))
            // interior
            for(i <- (span.begin+1) until span.end) {
              addToIndex(bioeFeatures(li)(1), feats.featuresForWord(i))
            }
            // span
            addToIndex(bioeFeatures(li)(2), feats.featuresForSpan(span.begin, span.end))
            addToIndex(transitionFeatures(last)(li), feats.featuresForSpan(span.begin, span.end, FeaturizationLevel.MinimalFeatures))
            last = li
          }
        }
      }

      new IndexedStandardFeaturizer(f, fi, bioeFeatures, transitionFeatures, startSymbol, labelIndex, constraintFactory)

    }

  }


}
