package epic
package sequences

import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector
import breeze.linalg._
import breeze.util._
import com.typesafe.scalalogging.slf4j.LazyLogging
import epic.constraints.LabeledSpanConstraints
import epic.features._
import epic.framework._
import epic.lexicon.SimpleLexicon
import epic.sequences.SemiCRF.{IdentityAnchoring, TransitionVisitor}
import epic.sequences.SemiCRFModel.BIEOFeatureAnchoring
import epic.util.{Optional, SafeLogging}

import scala.collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRFModel[L, W](val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         val constraintsFactory: LabeledSpanConstraints.Factory[L, W],
                         initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends StandardExpectedCounts.Model[Segmentation[L, W]] with Serializable {

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


  def accumulateCounts(inf: Inference, s: Scorer, d: Segmentation[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): Unit = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[inf.Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double) {
        import localization._
        axpy(count * scale, featuresForBegin(prev, cur, begin), counts)
        var p = begin+1
        while (p < end) {
          axpy(count * scale, featuresForInterior(cur, p), counts)
          p += 1
        }

        axpy(count * scale, featuresForSpan(prev, cur, begin, end), counts)
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

  val labelIndex = featurizer.labelIndex

  def scorer(v: Segmentation[L, W]): Scorer = scorer(v.words)


  def marginal(scorer: Scorer, v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    val m = SemiCRF.Marginal(aug * scorer)
    val partition: Double = m.logPartition
    val partition1: Double = SemiCRF.Marginal.goldMarginal[L, W](scorer * aug, v.label).logPartition
    if(partition1 > partition)
      println(v + " " + SemiCRF.posteriorDecode(m).render + " " + v.render + " " + partition + " " +  partition1)
    m
  }


  def goldMarginal(scorer: Scorer, v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): Marginal = {
    SemiCRF.Marginal.goldMarginal[L, W](scorer * aug, v.label)
  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = {
    new IdentityAnchoring(v.words, labelIndex, constraintsFactory.constraints(v.words))
  }

  case class Anchoring(localization: BIEOFeatureAnchoring[L, W],
                       constraints: LabeledSpanConstraints[L]) extends SemiCRF.Anchoring[L, W] {
    val labelIndex = featurizer.labelIndex

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
      var spanCell = spanCache(tind)
      if(spanCache(tind) == null) {
        spanCell = new Array[Array[Double]](labelIndex.size)
        spanCache(tind) = spanCell
      }

      var curLabelCell = spanCell(cur)
      if(curLabelCell == null) {

        val span = localization.featuresForSpan(prev, cur, beg, end)
        if (span eq null) {
          spanCell(cur) = negInfArray
          Double.NegativeInfinity
        } else {
          curLabelCell = java.util.Arrays.copyOf(nanArray, nanArray.length)
          curLabelCell(prev) = weights dot span
          spanCell(cur) = curLabelCell
          curLabelCell(prev)
        }
      } else {
        if (java.lang.Double.isNaN(curLabelCell(prev))) {
          val span = localization.featuresForSpan(prev, cur, beg, end)
          curLabelCell(prev) = weights dot span
        }
        curLabelCell(prev)
      }
    }


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
 * @param pruningModel
 * @param gazetteer
 * @param weights
 * @tparam L
 */
class SegmentationModelFactory[L](wordFeaturizer: Optional[WordFeaturizer[String]] = None,
                                  spanFeaturizer: Optional[SurfaceFeaturizer[String]] = None,
                                  pruningModel: Optional[SemiCRF.ConstraintSemiCRF[L, String]] = None,
                                  gazetteer: Optional[Gazetteer[Any, String]] = None,
                                  weights: Feature=>Double = { (f:Feature) => 0.0}) extends LazyLogging {

  import epic.sequences.SegmentationModelFactory._

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    logger.info("Maximum lengths for segments: " + maxLengthMap)

    val counts: Counter2[Option[L], String, Double] = Counter2.count(train.map(_.asFlatTaggedSequence).map{seg => seg.label zip seg.words}.flatten).mapValues(_.toDouble)
    val lexicon = new SimpleLexicon(new OptionIndex(labelIndex), counts, openTagThreshold = 10, closedWordThreshold = 20)

    val allowedSpanClassifier: LabeledSpanConstraints.Factory[L, String] = pruningModel.getOrElse(new LabeledSpanConstraints.LayeredTagConstraintsFactory(lexicon, maxLengthArray))

    lazy val featurizerPair = goodNERFeaturizers(counts)
    var wfeat = wordFeaturizer.getOrElse(featurizerPair._1)
    var sfeat = spanFeaturizer.getOrElse(featurizerPair._2)
    wfeat = gazetteer.foldLeft(wfeat)(_ + _)
    sfeat = gazetteer.foldLeft(sfeat)(_ + _)

    val wf = IndexedWordFeaturizer.fromData(wfeat, train.map(_.words))
    val sf = IndexedSurfaceFeaturizer.fromData(sfeat, train.map(_.words), allowedSpanClassifier)

    for(f <- pruningModel) {
      assert(f.labelIndex == new OptionIndex(labelIndex), f.labelIndex + " " + labelIndex)
    }
    val indexed = IndexedStandardFeaturizer.make(wf, sf, new OptionIndex(labelIndex), allowedSpanClassifier)(train)
    val model = new SemiCRFModel(indexed, allowedSpanClassifier, weights)

    model
  }



}

object SegmentationModelFactory {

  def goodNERFeaturizers[L](counts: Counter2[L, String, Double]) = {
    val dsl = new WordFeaturizer.DSL[L](counts) with SurfaceFeaturizer.DSL with BrownClusters.DSL
    import dsl._

    val featurizer = (
      unigrams(word + brown + shape, 2)
        + bigrams(shape, 1)
//        + bigrams(brownClusters(7), 1)
//        + shape(-1) * shape * shape(1)
        + prefixes(7)
        + suffixes(7)
//        + unigrams(props, 2)
//        + bigrams(props, 1)
      + unigrams(props, 1)
//      + context(brownClusters(7), 4)
//      + nextWordToLeft(word)
//      + nextWordToRight(word)
      )
    val spanFeatures = length //+ spanShape // //+ length * brownClusters(7)(begin) + length * brownClusters(7)(end)  //+ sent //+ (sent + spanShape) * length
    featurizer -> spanFeatures
  }


  case class Label1Feature[L](label: L, kind: Any) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature
  case object OutsideFeature extends Feature


  object FeatureKinds extends Enumeration {
    val Begin, Interior, Span, Label = Value
  }



  @SerialVersionUID(2L)
  class IndexedStandardFeaturizer[L, W] private (wordFeaturizer: IndexedWordFeaturizer[W],
                                                 surfaceFeaturizer: IndexedSurfaceFeaturizer[W],
                                                 wordFeatureIndex: CrossProductIndex[Feature, Feature],
                                                 spanFeatureIndex: CrossProductIndex[Feature, Feature],
                                                 bioeFeatures: Array[Array[Array[Int]]], // label -> kind -> indexes into surfaceFeaturizer.labelFeatureIndex
                                                 transitionFeatures: Array[Array[Array[Int]]], // prev -> cur -> indexes into surfaceFeaturizer.labelFeatureIndex
                                                 val labelIndex: OptionIndex[L],
                                                 val constraintFactory: LabeledSpanConstraints.Factory[L, W]) extends SemiCRFModel.BIEOFeaturizer[L,W] with Serializable with SafeLogging {

    val featureIndex = SegmentedIndex(wordFeatureIndex, spanFeatureIndex)
    private val wordOffset = featureIndex.componentOffset(0)
    private val spanOffset = featureIndex.componentOffset(1)

    def anchor(w: IndexedSeq[W]): SemiCRFModel.BIEOFeatureAnchoring[L, W] = new SemiCRFModel.BIEOFeatureAnchoring[L, W] {
      import epic.sequences.SegmentationModelFactory.FeatureKinds._
      val constraints = constraintFactory.constraints(w)

      def words: IndexedSeq[W] = w

      val loc = surfaceFeaturizer.anchor(w)
      val wloc = wordFeaturizer.anchor(w)
      def length = w.length

      def featureIndex = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, l: Int, w: Int): FeatureVector = {
        val features = wordFeatureIndex.crossProduct(bioeFeatures(l)(Begin.id) ++ transitionFeatures(prev)(l) ++ bioeFeatures(l)(Label.id) , wloc.featuresForWord(w), wordOffset)
        new FeatureVector(features)
      }
      def featuresForInterior(cur: Int, pos: Int): FeatureVector = {
        val features = wordFeatureIndex.crossProduct(bioeFeatures(cur)(Interior.id) ++ bioeFeatures(cur)(Label.id), wloc.featuresForWord(pos), wordOffset)
        new FeatureVector(features)
      }

      def featuresForSpan(prev: Int, cur: Int, begin: Int, end: Int): FeatureVector = {
        if (!constraints.isAllowedLabeledSpan(begin, end, cur)) {
          null
        } else {
          var features = spanFeatureIndex.crossProduct(bioeFeatures(cur)(Span.id), loc.featuresForSpan(begin, end), spanOffset)

          if(end - begin == 1) {
            features ++= wordFeatureIndex.crossProduct(bioeFeatures(cur)(Span.id), wloc.featuresForWord(begin), wordOffset)
          }

          //val features2 = spanFeatureIndex.crossProduct(transitionFeatures(prev)(cur), loc.featuresForSpan(begin, end), spanOffset)
          new FeatureVector(features)
        }
      }
    }
  }

  object IndexedStandardFeaturizer {
    def make[L, W](wordFeaturizer: IndexedWordFeaturizer[W],
                spanFeaturizer: IndexedSurfaceFeaturizer[W],
                labelIndex: OptionIndex[L],
                constraintFactory: LabeledSpanConstraints.Factory[L, W],
                hashFeatures: HashFeature.Scale = HashFeature.Absolute(0))
               (data: IndexedSeq[Segmentation[L, W]]):IndexedStandardFeaturizer[L, W] = {
      val labelPartIndex = Index[Feature]()
      val outsideFeature = labelPartIndex.index(OutsideFeature)
      val bioeFeatures = Array.tabulate(labelIndex.size, FeatureKinds.maxId)((i,j) => if(i == labelIndex.size - 1) Array.empty[Int] else Array(labelPartIndex.index(Label1Feature(labelIndex.get(i).get, FeatureKinds(j)))))
      val transitionFeatures = Array.tabulate(labelIndex.size, labelIndex.size) { (i, j) =>
        val li = labelIndex.get(i).fold(OutsideFeature:Any)(identity)
        val lj = labelIndex.get(j).fold(OutsideFeature:Any)(identity)

        if(lj == OutsideFeature)
          Array(labelPartIndex.index(TransitionFeature(li, lj)), outsideFeature)
        else
          Array(labelPartIndex.index(TransitionFeature(li, lj)))
      }

      val spanBuilder = new CrossProductIndex.Builder(labelPartIndex, spanFeaturizer.featureIndex, hashFeatures, includeLabelOnlyFeatures = true)
      val wordBuilder = new CrossProductIndex.Builder(labelPartIndex, wordFeaturizer.featureIndex, hashFeatures, includeLabelOnlyFeatures = true)

      for (d <- data){
        val feats = spanFeaturizer.anchor(d.words)
        val wordFeats = wordFeaturizer.anchor(d.words)
        var last = labelIndex(None)
        for ((optL, span) <- d.segmentsWithOutside) optL match {
          case Some(l) =>
            val li = labelIndex(optL)
            // featuresForBegin
            wordBuilder.add(bioeFeatures(li)(FeatureKinds.Begin.id), wordFeats.featuresForWord(span.begin))
            wordBuilder.add(bioeFeatures(li)(FeatureKinds.Label.id), wordFeats.featuresForWord(span.begin))
            wordBuilder.add(transitionFeatures(last)(li), wordFeats.featuresForWord(span.begin))
            // interior
            for(i <- (span.begin+1) until span.end) {
              wordBuilder.add(bioeFeatures(li)(FeatureKinds.Interior.id), wordFeats.featuresForWord(i))
              wordBuilder.add(bioeFeatures(li)(FeatureKinds.Label.id), wordFeats.featuresForWord(i))
            }
            // span
            spanBuilder.add(bioeFeatures(li)(FeatureKinds.Span.id), feats.featuresForSpan(span.begin, span.end))
            if(span.length == 1) {
              wordBuilder.add(bioeFeatures(li)(FeatureKinds.Span.id), wordFeats.featuresForWord(span.begin))
            }
            last = li
          case None =>
            wordBuilder.add(outsideFeature, wordFeats.featuresForWord(span.begin))
            last = labelIndex(None)
        }
      }

      val spanFeatures = spanBuilder.result()
      val wordFeatures = wordBuilder.result()

      new IndexedStandardFeaturizer(wordFeaturizer, spanFeaturizer, wordFeatures, spanFeatures, bioeFeatures, transitionFeatures, labelIndex, constraintFactory)

    }

  }


}
