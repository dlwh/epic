package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg.{Counter, SparseVector, DenseVector}
import epic.sequences.SemiCRF.{TransitionVisitor}
import collection.mutable.ArrayBuffer
import epic.parser.features.WordShapeFeaturizer
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import breeze.collection.mutable.TriangularArray
import epic.trees.Span

/**
 *
 * @author dlwh
 */
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         maxSegmentLength: Int=>Int) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model {
  def extractCRF(weights: DenseVector[Double]) = {
    val grammar = inferenceFromWeights(weights)
    new SemiCRF(grammar)
  }

  type Inference = SemiCRFInference[L, W]

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SemiCRFInference(weights, featureIndex, featurizer, maxSegmentLength)

}

object SemiCRFModel {
  trait BIEOFeaturizer[L, W] extends SemiCRF.IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]): BIEOAnchoredFeaturizer[L, W]
  }

  trait BIEOAnchoredFeaturizer[L, W] extends SemiCRF.AnchoredFeaturizer[L, W] {

    def featuresForBegin(prev: Int, cur: Int, pos: Int):SparseVector[Double]
    def featuresForEnd(cur: Int, pos: Int):SparseVector[Double]
    def featuresForInterior(cur: Int, pos: Int):SparseVector[Double]
    def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int):SparseVector[Double]

    def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int): SparseVector[Double] = {
      val acc = new ArrayBuffer[SparseVector[Double]]()
      var nnz =  0
      val _begin = featuresForBegin(prev, cur, start)
      acc += _begin
      nnz += _begin.activeSize
      val _end = featuresForEnd(cur, end)
      acc += _end
      nnz += _end.activeSize
      var p = start+1
      while(p < end) {
        val w = featuresForInterior(cur, p)
        acc += w
        nnz += w.activeSize
        p += 1
      }

      val forSpan = featuresForSpan(prev, cur, start, end)
      acc += forSpan
      nnz += forSpan.activeSize

      val result = SparseVector.zeros[Double](featureIndex.size)
      var i = 0
      while(i < acc.size) {
        result += acc(i)
        i += 1
      }
      result
    }
  }
}

class SemiCRFInference[L, W](weights: DenseVector[Double],
                             featureIndex: Index[Feature],
                             featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                             maxLength: Int=>Int) extends MarginalInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] with SemiCRF.Grammar[L, W] {
  type Marginal = SemiCRF.Marginal[L, W]
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def emptyCounts = StandardExpectedCounts.zero(this.featureIndex)

  def anchor(w: IndexedSeq[W]) = new Anchoring(w, new IdentityAnchoring(w))


  def labelIndex = featurizer.labelIndex
  def startSymbol = featurizer.startSymbol

  def marginal(v: Segmentation[L, W], aug: SemiCRF.Anchoring[L, W]): (Marginal, Double) = {
    val m = SemiCRF.Marginal(new Anchoring(v.words, aug))
    m -> m.logPartition
  }


  override def goldCounts(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]): ExpectedCounts = {
    val m = SemiCRF.Marginal.goldMarginal[L, W](new Anchoring(v.words, augment), v.segments)
    this.countsFromMarginal(v, m, augment)
  }


  def countsFromMarginal(v: Segmentation[L, W], marg: Marginal, aug: SemiCRF.Anchoring[L, W]): ExpectedCounts = {
    val counts = emptyCounts
    counts.loss = marg.logPartition
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def daxpy(d: Double, vector: SparseVector[Double], counts: DenseVector[Double]) {
        var i = 0
        val index = vector.index
        val data = vector.data
        while(i < vector.iterableSize) {
//          if(vector.isActive(i))
            counts(index(i)) += d * data(i)
          i += 1
        }

      }

      def apply(prev: Int, cur: Int, start: Int, end: Int, count: Double) {
        import localization._
        daxpy(count, featuresForBegin(prev, cur, start), counts.counts)
        daxpy(count, featuresForEnd(cur, end), counts.counts)
        var p = start+1
        while(p < end) {
          daxpy(count, featuresForInterior(cur, p), counts.counts)
          p += 1
        }

        daxpy(count, featuresForSpan(prev, cur, start, end), counts.counts)

      }
    }
    marg.visit(visitor)
    counts

  }


  def baseAugment(v: Segmentation[L, W]): SemiCRF.Anchoring[L, W] = new IdentityAnchoring(v.words)

  class IdentityAnchoring(val w: IndexedSeq[W]) extends SemiCRF.Anchoring[L, W] {
    def maxSegmentLength(l: Int): Int = maxLength(l)

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol: L = featurizer.startSymbol
  }

  class Anchoring(val w: IndexedSeq[W], augment: SemiCRF.Anchoring[L, W]) extends SemiCRF.Anchoring[L, W] {
    val localization = featurizer.anchor(w)
    def maxSegmentLength(l: Int): Int = SemiCRFInference.this.maxLength(l)

    val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, length){ (p,c,w) =>
      weights dot localization.featuresForBegin(p,c,w)
    }
    val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      weights dot localization.featuresForEnd(l, w+1)
    }
    val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      weights dot localization.featuresForInterior(l, w)
    }


    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      var score = augment.scoreTransition(prev, cur, beg, end)
      score += beginCache(prev)(cur)(beg)
      score += endCache(cur)(end-1)
      var pos = beg + 1
      while(pos < end) {
        score += wordCache(cur)(pos)
        pos += 1
      }

      score + (weights dot localization.featuresForSpan(prev, cur, beg, end))
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }


}


object SegmentationModelFactory {
  case class SFeature(w: String, kind: Symbol) extends Feature
  case class BeginFeature[L](w: Feature, cur: L) extends Feature
  case class EndFeature[L](w: Feature, cur: L) extends Feature
  case class SpanFeature[L](distance: Feature, cur: L) extends Feature
  case class UnigramFeature[L](w: Feature, cur: L) extends Feature
  case class CFeature(component: Int, f: Feature) extends Feature
  case class DistanceFeature(distanceBin: Int) extends Feature
}

class SegmentationModelFactory[L](val startSymbol: L,
                                  gazetteer: Gazetteer[String, String] = Gazetteer.empty[String, String]) {

  import SegmentationModelFactory._


  /**
   * Computes basic features from word counts
   * @param wordCounts
   */
  class StandardFeaturizer(wordCounts: Counter[String, Double] ) {
    val inner = new WordShapeFeaturizer(wordCounts)

    def localize(words: IndexedSeq[String])= new Localization(words)

    val interner = new Interner[Feature]

    class Localization(words: IndexedSeq[String]) {
      val classes = words.map(EnglishWordClassGenerator)
      val shapes = words.map(WordShapeGenerator)

      val featuresForWord: immutable.IndexedSeq[Array[Feature]] = 0 until words.length map { pos =>
        val feats = new ArrayBuffer[Feature]()
        feats ++= inner.featuresFor(words, pos)
        feats ++= IndexedSeq(
          SFeature(if(pos == 0) "##" else shapes(pos-1), 'PrevWordShape),
          SFeature(if(pos == words.length - 1) "##" else shapes(pos+1), 'NextWordShape),
          SFeature(if(pos == 0) "##" else classes(pos-1), 'PrevWordClass),
          SFeature(if(pos == words.length - 1) "##" else classes(pos+1), 'NextWordClass))
        feats ++= gazetteer.lookupWord(words(pos)).map(SFeature(_, 'WordSeenInSegment))
        feats.map(interner.intern _).toArray
      }

      def featuresForSpan(start: Int, end: Int):Array[Feature] = {
        val arr = Array[Feature](DistanceFeature(binDistance(end - start)))

        gazetteer.lookupSpan(Span(start, end).map(words).toIndexedSeq).map(arr :+ SFeature(_, 'SegmentKnown)).getOrElse(arr)
      }

      private def binDistance(len: Int) = {
        if(len <= 1) 1
        else if(len <= 4) 2
        else if(len <= 8) 3
        else if(len <= 16) 4
        else 5
      }
    }

  }



  class IndexedStandardFeaturizer(f: StandardFeaturizer,
                                  val labelIndex: Index[L],
                                  val basicFeatureIndex: Index[Feature]) extends SemiCRFModel.BIEOFeaturizer[L,String] {

    val startSymbol = SegmentationModelFactory.this.startSymbol

    private val label2Index = new PairIndex(labelIndex, labelIndex)
    private val labeledFeatureIndex = new PairIndex(labelIndex, basicFeatureIndex)
    // feature mappings... sigh
    private implicit val beginIso = Isomorphism[(L, Feature), BeginFeature[L]](
      tu={pair => BeginFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )
    private implicit val endIso = Isomorphism[(L, Feature), EndFeature[L]](
      tu={pair => EndFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val uniIso = Isomorphism[(L, Feature), UnigramFeature[L]](
      tu={pair => UnigramFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val spanIso = Isomorphism[((L, L), Feature), SpanFeature[(L, L)]](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.distance) }
    )

    private val distanceFeatureIndex = Index[Feature](Iterator(DistanceFeature(1), DistanceFeature(2), DistanceFeature(3), DistanceFeature(4), DistanceFeature(5)))
    private val spanFeatureIndex = new PairIndex(label2Index, distanceFeatureIndex)

    val compositeIndex = new CompositeIndex[Feature](new IsomorphismIndex(labeledFeatureIndex)(beginIso),
      new IsomorphismIndex(labeledFeatureIndex)(endIso),
      new IsomorphismIndex(labeledFeatureIndex)(uniIso),
      new IsomorphismIndex(spanFeatureIndex)(spanIso)
    )

    val BEGIN_COMP = 0
    val END_COMP = 1
    val UNI_COMP = 2
    val SPAN_COMP = 3

    private implicit val featureIso = Isomorphism[(Int,Feature), Feature](
      tu={pair => CFeature(pair._1, pair._2)},
      ut={f => f.asInstanceOf[CFeature].component -> f.asInstanceOf[CFeature].f}
    )

    val featureIndex: IsomorphismIndex[(Int, Feature), Feature] = new IsomorphismIndex(compositeIndex)(featureIso)
    println(featureIndex.size)

    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOAnchoredFeaturizer[L, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[L, String] {
      val loc = f.localize(w)

      val basicFeatureCache = Array.tabulate(w.length){ pos =>
        val feats =  loc.featuresForWord(pos)
        feats.map(basicFeatureIndex).filter(_ >= 0)
      }

      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){ (p,c,w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(BEGIN_COMP, labeledFeatureIndex.mapIndex(c, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }
      val endCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(END_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }
      val wordCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new SparseVector.Builder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(UNI_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.result()
      }

      def featureIndex: Index[Feature] = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, cur: Int, pos: Int): SparseVector[Double] = {
        beginCache(prev)(cur)(pos)
      }

      def featuresForEnd(cur: Int, pos: Int): SparseVector[Double] = {
        endCache(cur)(pos-1)
      }

      def featuresForInterior(cur: Int, pos: Int): SparseVector[Double] = {
        wordCache(cur)(pos)
      }

      private val spanCache = TriangularArray.tabulate(w.length+1){ (beg,end) =>
        loc.featuresForSpan(beg, end).map(distanceFeatureIndex).filter(_ >= 0)
      }
      private val spanFeatures = Array.tabulate(labelIndex.size, labelIndex.size){ (prev, cur) =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          val feats = spanCache(beg, end)
          val builder = new SparseVector.Builder[Double](featureIndex.size)
          builder.sizeHint(feats.length)
          var i = 0
          while(i < feats.length) {
            val index = compositeIndex.mapIndex(SPAN_COMP, spanFeatureIndex.mapIndex(label2Index.mapIndex(prev, cur), feats(i)))
            if(index != -1) {
              builder.add(index, 1.0)
            }
            i += 1
          }
          builder.result()
        }
      }


      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): SparseVector[Double] = {
        spanFeatures(prev)(cur)(beg,end)
      }



    }
  }

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol) ++ train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))

    val wordCounts:Counter[String, Double] = Counter.count(train.flatMap(_.words):_*).mapValues(_.toDouble)
    val f = new StandardFeaturizer(wordCounts)
    val basicFeatureIndex = Index[Feature]()

    val maxMaxLength = (0 until labelIndex.size).map(maxLengthArray).max
    var i = 0
    for(s <- train) {
      if(i % 250 == 0)
        println(i + "/" + train.length)
      val loc = f.localize(s.words)

      for(b <- 0 until s.length) {
        loc.featuresForWord(b) foreach {basicFeatureIndex.index _}
        for(e <- (b+1) until math.min(s.length,b+maxMaxLength)) {
          loc.featuresForSpan(b, e) foreach {basicFeatureIndex.index _}
        }

      }
      i += 1
    }
    println(train.length + "/" + train.length)
    val indexed = new IndexedStandardFeaturizer(f, labelIndex, basicFeatureIndex)
    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray)

    model
  }

}
