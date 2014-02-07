package epic
package sequences

import epic.framework._
import breeze.util._
import breeze.linalg._
import epic.sequences.SemiCRF.{IdentityAnchoring, TransitionVisitor}
import collection.mutable.ArrayBuffer
import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import epic.constraints.LabeledSpanConstraints
import epic.lexicon.SimpleLexicon
import epic.features._
import epic.util.{SafeLogging, CacheBroker, NotProvided, Optional}
import com.typesafe.scalalogging.slf4j.Logging
import epic.sequences.SemiCRFModel.BIEOFeatureAnchoring

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRFModel[L, W](val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         val constraintsFactory: LabeledSpanConstraints.Factory[L, W],
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0},
                         cacheFeatures: Boolean = false) extends StandardExpectedCounts.Model[Segmentation[L, W]] with Serializable {
  def labelIndex: Index[L] = featurizer.labelIndex

  def featureIndex = featurizer.featureIndex

  def extractCRF(weights: DenseVector[Double]) = {
    inferenceFromWeights(weights)
  }

  type Inference = SemiCRFInference[L, W]
  type Scorer = SemiCRF.Anchoring[L, W]
  type Marginal = SemiCRF.Marginal[L, W]

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference =
    new SemiCRFInference(weights, featureIndex, featurizer, constraintsFactory)


  def accumulateCounts(s: Scorer, d: Segmentation[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): Unit = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Inference#Anchoring].localization
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
  }

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]): BIEOFeatureAnchoring[L, W]
  }

  trait BIEOFeatureAnchoring[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

    def words: IndexedSeq[W]

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
    SemiCRF.viterbi(anchoring * new Anchoring(featurizer.anchor(sentence),
      constraintsFactory.constraints(sentence)))
  }

  type Marginal = SemiCRF.Marginal[L, W]
  type Scorer = SemiCRF.Anchoring[L, W]

  def scorer(w: IndexedSeq[W]): Anchoring = {
    new Anchoring(featurizer.anchor(w), constraintsFactory.constraints(w))
  }

  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def scorer(v: Segmentation[L, W]): Scorer = scorer(v.words)


  def marginal(scorer: Scorer, v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    val m = SemiCRF.Marginal(aug * scorer)
    m
  }


  def goldMarginal(scorer: Scorer, v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    SemiCRF.Marginal.goldMarginal[L, W](scorer * aug, v.label)
  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = {
    new IdentityAnchoring(v.words, featurizer.labelIndex, featurizer.startSymbol, constraintsFactory.constraints(v.words))
  }

  case class Anchoring(localization: BIEOFeatureAnchoring[L, W],
                       constraints: LabeledSpanConstraints[L]) extends SemiCRF.Anchoring[L, W] {

    def words: IndexedSeq[W] = localization.words

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
      if (beg + 1 != end && !constraints.isAllowedLabeledSpan(beg, end, cur)) {
        Double.NegativeInfinity
      } else {
        var score = 0.0
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

/**
 * Factory class for making a [[epic.sequences.SemiCRFModel]] based
 * on some data and an optional gazetteer.
 * @param startSymbol
 * @param outsideSymbol
 * @param pruningModel
 * @param gazetteer
 * @param weights
 * @param broker
 * @tparam L
 */
class SegmentationModelFactory[L](val startSymbol: L,
                                  val outsideSymbol: L,
                                  wordFeaturizer: Optional[WordFeaturizer[String]] = NotProvided,
                                  spanFeaturizer: Optional[SurfaceFeaturizer[String]] = NotProvided,
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

    lazy val featurizerPair = goodNERFeaturizers(counts)
    var wfeat = wordFeaturizer.getOrElse(featurizerPair._1)
    var sfeat = spanFeaturizer.getOrElse(featurizerPair._2)
    wfeat = gazetteer.foldLeft(wfeat)(_ + _)
    sfeat = gazetteer.foldLeft(sfeat)(_ + _)

    val wf = IndexedWordFeaturizer.fromData(wfeat, train.map(_.words))
    val sf = IndexedSurfaceFeaturizer.fromData(sfeat, train.map(_.words), allowedSpanClassifier)

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = IndexedStandardFeaturizer.make(wf, sf, startSymbol, labelIndex, allowedSpanClassifier)(train)
    val model = new SemiCRFModel(indexed, allowedSpanClassifier, weights)

    model
  }



}

object SegmentationModelFactory {

  def goodNERFeaturizers[L](counts: Counter2[L, String, Double]) = {
    val dsl = new WordFeaturizer.DSL[L](counts) with SurfaceFeaturizer.DSL
    import dsl._

    val featurizer = (
      word
        + unigrams(clss, 1)
        + unigrams(shape, 2)
        + bigrams(clss, 1)
        // + bigrams(tagDict, 2)
        // + bigrams(shape, 1)
        + shape(-1) * shape * shape(1)
        + prefixes()
        + suffixes()
        + props
      )
    val spanFeatures = length + spanShape + sent //+ (sent + spanShape) * length
    featurizer -> spanFeatures
  }


  case class Label1Feature[L](label: L, kind: Symbol) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature

  val kinds = Array('Begin, 'Interior, 'Span)



  @SerialVersionUID(2L)
  class IndexedStandardFeaturizer[L] private (wordFeaturizer: IndexedWordFeaturizer[String],
                                              surfaceFeaturizer: IndexedSurfaceFeaturizer[String],
                                              wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                              spanFeatureIndex: CrossProductIndex[Feature, Feature],
                                              bioeFeatures: Array[Array[Array[Int]]], // label -> kind -> indexes into surfaceFeaturizer.labelFeatureIndex
                                              transitionFeatures: Array[Array[Array[Int]]], // prev -> cur -> indexes into surfaceFeaturizer.labelFeatureIndex
                                              val startSymbol: L,
                                              val labelIndex: Index[L],
                                              val constraintFactory: LabeledSpanConstraints.Factory[L, String]) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable with SafeLogging {

    val featureIndex = SegmentedIndex(wordFeatureIndex, spanFeatureIndex)
    private val wordOffset = featureIndex.componentOffset(0)
    private val spanOffset = featureIndex.componentOffset(1)

    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOFeatureAnchoring[L, String] = new SemiCRFModel.BIEOFeatureAnchoring[L, String] {
      val constraints = constraintFactory.constraints(w)

      def words: IndexedSeq[String] = w

      val loc = surfaceFeaturizer.anchor(w)
      val wloc = wordFeaturizer.anchor(w)
      def length = w.length

      def featureIndex = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, l: Int, w: Int): FeatureVector = {
        val features = wordFeatureIndex.crossProduct(bioeFeatures(l)(0) ++  transitionFeatures(prev)(l), wloc.featuresForWord(w), wordOffset)
        new FeatureVector(features)
      }
      def featuresForInterior(cur: Int, pos: Int): FeatureVector = {
        val features = wordFeatureIndex.crossProduct(bioeFeatures(cur)(1), wloc.featuresForWord(pos), wordOffset)
        new FeatureVector(features)
      }

      def featuresForSpan(prev: Int, cur: Int, begin: Int, end: Int): FeatureVector = {
        if (!constraints.isAllowedLabeledSpan(begin, end, cur)) {
          null
        } else {
          val features = spanFeatureIndex.crossProduct(bioeFeatures(cur)(2), loc.featuresForSpan(begin, end), spanOffset)
          //val features2 = spanFeatureIndex.crossProduct(transitionFeatures(prev)(cur), loc.featuresForSpan(begin, end), spanOffset)
          new FeatureVector(features)
        }
      }
    }
  }

  object IndexedStandardFeaturizer {
    def make[L](wordFeaturizer: IndexedWordFeaturizer[String],
                spanFeaturizer: IndexedSurfaceFeaturizer[String],
                startSymbol: L,
                labelIndex: Index[L],
                constraintFactory: LabeledSpanConstraints.Factory[L, String],
                hashFeatures: HashFeature.Scale = HashFeature.Absolute(0))
               (data: IndexedSeq[Segmentation[L, String]]):IndexedStandardFeaturizer[L] = {
      val labelPartIndex = Index[Feature]()
      val bioeFeatures = Array.tabulate(labelIndex.size, kinds.length)((i,j) => Array(labelPartIndex.index(Label1Feature(labelIndex.get(i), kinds(j)))))
      val transitionFeatures = Array.tabulate(labelIndex.size, labelIndex.size)((i,j) => Array(labelPartIndex.index(TransitionFeature(labelIndex.get(i), labelIndex.get(j)))))

      val spanBuilder = new CrossProductIndex.Builder(labelPartIndex, spanFeaturizer.featureIndex, hashFeatures, includeLabelOnlyFeatures = true)
      val wordBuilder = new CrossProductIndex.Builder(labelPartIndex, wordFeaturizer.featureIndex, hashFeatures, includeLabelOnlyFeatures = true)

      for (d <- data){
        val feats = spanFeaturizer.anchor(d.words)
        val wordFeats = wordFeaturizer.anchor(d.words)
        var last = labelIndex(startSymbol)
        for ((l, span) <- d.segments) {
          val li = labelIndex(l)
          // featuresForBegin
          wordBuilder.add(bioeFeatures(li)(0), wordFeats.featuresForWord(span.begin))
          wordBuilder.add(transitionFeatures(last)(li), wordFeats.featuresForWord(span.begin))
          // interior
          for(i <- (span.begin+1) until span.end) {
            wordBuilder.add(bioeFeatures(li)(1), wordFeats.featuresForWord(i))
          }
          // span
          spanBuilder.add(bioeFeatures(li)(2), feats.featuresForSpan(span.begin, span.end))
          last = li
        }
      }

      val spanFeatures = spanBuilder.result()
      val wordFeatures = wordBuilder.result()

      new IndexedStandardFeaturizer(wordFeaturizer, spanFeaturizer, wordFeatures, spanFeatures, bioeFeatures, transitionFeatures, startSymbol, labelIndex, constraintFactory)

    }

  }


}
