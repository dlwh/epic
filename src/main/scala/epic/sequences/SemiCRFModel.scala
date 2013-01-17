package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg.{VectorBuilder, Counter, SparseVector, DenseVector}
import epic.sequences.SemiCRF.TransitionVisitor
import collection.mutable.ArrayBuffer
import epic.parser.features.{PairFeature, SpanShapeGenerator, WordShapeFeaturizer}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import breeze.collection.mutable.TriangularArray
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges

/**
 *
 * @author dlwh
 */
class SemiCRFModel[L, W](val featureIndex: Index[Feature],
                         val featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                         maxSegmentLength: Int=>Int) extends Model[Segmentation[L, W]] with StandardExpectedCounts.Model {
  def labelIndex: Index[L] = featurizer.labelIndex

  def extractCRF(weights: DenseVector[Double]) = {
    val grammar = inferenceFromWeights(weights)
    new SemiCRF(grammar)
  }

  type Inference = SemiCRFInference[L, W]
  type Marginal = SemiCRF.Marginal[L, W]

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

@SerialVersionUID(1)
class SemiCRFInference[L, W](weights: DenseVector[Double],
                             featureIndex: Index[Feature],
                             featurizer: SemiCRFModel.BIEOFeaturizer[L, W],
                             val maxLength: Int=>Int) extends AugmentableInference[Segmentation[L, W], SemiCRF.Anchoring[L, W]] with SemiCRF.Grammar[L, W] with Serializable {
  def viterbi(sentence: IndexedSeq[W], anchoring: SemiCRF.Anchoring[L, W]) = {
    SemiCRF.viterbi(new Anchoring(sentence, anchoring))
  }

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

  def goldMarginal(v: Segmentation[L, W], augment: SemiCRF.Anchoring[L, W]) = {
    val m = SemiCRF.Marginal.goldMarginal[L, W](new Anchoring(v.words, augment), v.segments)
    m -> m.logPartition
  }

  def countsFromMarginal(v: Segmentation[L, W], marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[L, W] {

      def daxpy(d: Double, vector: SparseVector[Double], counts: DenseVector[Double]) {
        var i = 0
        val index = vector.index
        val data = vector.data
        while(i < vector.iterableSize) {
//          if(vector.isActive(i))
            counts(index(i)) += d * data(i) * scale
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
      if(f eq null) Double.NegativeInfinity
      else weights dot f
    }
    val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      val f = localization.featuresForEnd(l, w + 1)
      if(f eq null) Double.NegativeInfinity
      else weights dot f
    }
    val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
      val f = localization.featuresForInterior(l, w)
      if(f eq null) Double.NegativeInfinity
      else weights dot f
    }


    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      var score = augment.scoreTransition(prev, cur, beg, end)
      if(score != Double.NegativeInfinity) {
        val span = localization.featuresForSpan(prev, cur, beg, end)
        if(span eq null)  {
          score = Double.NegativeInfinity
        } else {
          score += (weights dot span)

          score += beginCache(prev)(cur)(beg)
          score += endCache(cur)(end-1)
          var pos = beg + 1
          while(pos < end) {
            score += wordCache(cur)(pos)
            pos += 1
          }
        }
      }
      score
    }

    def labelIndex: Index[L] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }


}


object SegmentationModelFactory {
  case class SFeature(w: Any, kind: Symbol) extends Feature
  case class BeginFeature[L](w: Feature, cur: L) extends Feature
  case class EndFeature[L](w: Feature, cur: L) extends Feature
  case class TrigramFeature(a: Any, b: Any, c: Any) extends Feature
  case class SpanFeature[L](distance: Feature, cur: L) extends Feature
  case class UnigramFeature[L](w: Feature, cur: L) extends Feature
  case class CFeature(component: Int, f: Feature) extends Feature
  case class DistanceFeature(distanceBin: Int) extends Feature
  case object TransitionFeature extends Feature
  case object SpanStartsSentence extends Feature
  case object SpansWholeSentence extends Feature
}

class SegmentationModelFactory[L](val startSymbol: L,
                                  pruningModel: Option[SemiCRF.ConstraintGrammar[L, String]] = None,
                                  gazetteer: Gazetteer[Any, String] = Gazetteer.empty[String, String]) {

  import SegmentationModelFactory._

  def makeModel(train: IndexedSeq[Segmentation[L, String]]): SemiCRFModel[L, String] = {
    val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
    val labelIndex: Index[L] = Index[L](Iterator(startSymbol) ++ train.iterator.flatMap(_.label.map(_._1)))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))

    val wordCounts:Counter[String, Double] = Counter.count(train.flatMap(_.words):_*).mapValues(_.toDouble)
    val f = new StandardFeaturizer(wordCounts)
    val basicFeatureIndex = Index[Feature]()
    val spanFeatureIndex = Index[Feature]()
    val transFeatureIndex = Index[Feature]()

    val maxMaxLength = (0 until labelIndex.size).map(maxLengthArray).max
    var i = 0
    for(s <- train) {
      val loc = f.localize(s.words)

      for(b <- 0 until s.length) {
        loc.featuresForWord(b) foreach {basicFeatureIndex.index _}
        for(e <- (b+1) until math.min(s.length,b+maxMaxLength)) {
          loc.featuresForSpan(b, e) foreach {spanFeatureIndex.index _}
          loc.featuresForTransition(b, e) foreach {transFeatureIndex.index _}
        }

      }
      i += 1
    }

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = new IndexedStandardFeaturizer(f, labelIndex, basicFeatureIndex, spanFeatureIndex, transFeatureIndex, pruningModel)
    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray)

    model
  }


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

      val basicFeatures = (0 until words.length) map { i =>
        val w = words(i)
        if(wordCounts(w) > 10) IndexedSeq(w)
        else if (wordCounts(w) > 5) IndexedSeq(w, classes(i), shapes(i))
        else IndexedSeq(classes(i), shapes(i))
      } map {_.map(_.intern)}

      def basicFeature(pos: Int) = {
         if(pos < 0 || pos >= words.length) IndexedSeq("#")
        else basicFeatures(pos)
      }

      val featuresForWord: immutable.IndexedSeq[Array[Feature]] = 0 until words.length map { pos =>
        val feats = new ArrayBuffer[Feature]()
        val basic = basicFeature(pos).map(SFeature(_, 'Cur))
        val basicLeft = basicFeature(pos - 1).map(SFeature(_, 'Prev))
        val basicRight = basicFeature(pos + 1).map(SFeature(_, 'Next))
        feats ++= basicLeft
        feats ++= basicRight
        feats ++= inner.featuresFor(words, pos)
        for (a <- basicLeft; b <- basic) feats += PairFeature(a,b)
        for (a <- basic; b <- basicRight) feats += PairFeature(a,b)
//        for (a <- basicLeft; b <- basicRight) feats += PairFeature(a,b)
        feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
        if(pos > 0 && pos < words.length - 1) {
          feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
          feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
        }
        feats ++= gazetteer.lookupWord(words(pos)).map(SFeature(_, 'WordSeenInSegment))
        feats.map(interner.intern _).toArray
      }

      def featuresForTransition(beg: Int, end: Int) = {
        val feats = ArrayBuffer[Feature](DistanceFeature(binDistance(end - beg)), TransitionFeature)
        feats.toArray
      }

      def featuresForSpan(start: Int, end: Int):Array[Feature] = {
        val feats = ArrayBuffer[Feature]()
        feats ++= gazetteer.lookupSpan(Span(start, end).map(words).toIndexedSeq).map(SFeature(_, 'SegmentKnown))
        if(start < end - 1) {
          feats += WordEdges('Inside, basicFeature(start)(0), basicFeature(end-1)(0))
          feats += WordEdges('Outside, basicFeature(start-1)(0), basicFeature(end)(0))
          feats += WordEdges('Begin, basicFeature(start-1)(0), basicFeature(start)(0))
          feats += WordEdges('End, basicFeature(end-1)(0), basicFeature(end)(0))
          if(start == 0)
            feats += SpanStartsSentence
          if(start == 0 && end == words.length)
            feats += SpansWholeSentence

        }

        feats += SFeature(SpanShapeGenerator.apply(words, Span(start,end)), 'SpanShape)
        feats.toArray
      }

      private def binDistance(dist2:Int) = {
        val dist = dist2.abs - 1
        if (dist >= 20) 8
        else if (dist > 10) 7
        else if (dist > 5) 6
        else dist
      }
    }

  }

  @SerialVersionUID(1L)
  class IndexedStandardFeaturizer(f: StandardFeaturizer,
                                  val labelIndex: Index[L],
                                  val basicFeatureIndex: Index[Feature],
                                  val basicSpanFeatureIndex: Index[Feature],
                                  val basicTransFeatureIndex: Index[Feature],
                                  val pruningModel: Option[SemiCRF.ConstraintGrammar[L, String]] = None) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable {

    val startSymbol = SegmentationModelFactory.this.startSymbol

    // feature mappings... sigh
    // basically we want to build a big index for all features
    // (beginFeatures ++ endFeatures ++ unigramFeatures ++ spanFeatures ++ transitionFeatures)
    private val label2Index = new PairIndex(labelIndex, labelIndex)
    private val labeledFeatureIndex = new PairIndex(labelIndex, basicFeatureIndex)
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

    private implicit val spanIso = Isomorphism[(L,  Feature), SpanFeature[L]](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.distance) }
    )


    private implicit val transIso = Isomorphism[((L,L),  Feature), SpanFeature[(L,L)]](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.distance) }
    )

    private val spanFeatureIndex = new PairIndex(labelIndex, basicSpanFeatureIndex)
    private val transFeatureIndex = new PairIndex(label2Index, basicTransFeatureIndex)

    val compositeIndex = new CompositeIndex[Feature](new IsomorphismIndex(labeledFeatureIndex)(beginIso),
//      new IsomorphismIndex(labeledFeatureIndex)(endIso),
      new IsomorphismIndex(labeledFeatureIndex)(uniIso),
      new IsomorphismIndex(spanFeatureIndex)(spanIso),
      new IsomorphismIndex(transFeatureIndex)(transIso)
    )

    val BEGIN_COMP = 0
//    val END_COMP = 1
    val UNI_COMP = 1
    val SPAN_COMP = 2
    val TRANS_COMP = 3

    private implicit val featureIso = Isomorphism[(Int,Feature), Feature](
      tu={pair => CFeature(pair._1, pair._2)},
      ut={f => f.asInstanceOf[CFeature].component -> f.asInstanceOf[CFeature].f}
    )

    val featureIndex: IsomorphismIndex[(Int, Feature), Feature] = new IsomorphismIndex(compositeIndex)(featureIso)
    println("Number of features: " + featureIndex.size)

    def anchor(w: IndexedSeq[String]): SemiCRFModel.BIEOAnchoredFeaturizer[L, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[L, String] {
      val constraints = pruningModel.map(_.constraints(w))
      val loc = f.localize(w)

      val basicFeatureCache = Array.tabulate(w.length){ pos =>
        val feats =  loc.featuresForWord(pos)
        feats.map(basicFeatureIndex).filter(_ >= 0)
      }


      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){ (p,c,w) =>
        val ok = constraints.forall(_.allowedStarts(w)(c))
        if (!ok) {
          null
        }  else {
          val builder = new VectorBuilder[Double](featureIndex.size)
          val feats = basicFeatureCache(w)
          builder.reserve(feats.length)
          var i = 0
          while(i < feats.length) {
            val index = compositeIndex.mapIndex(BEGIN_COMP, labeledFeatureIndex.mapIndex(c, feats(i)))
            if(index != -1) {
              builder.add(index, 1.0)
            }

            i += 1
          }
          builder.toSparseVector
        }
      }
      val endCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new VectorBuilder[Double](featureIndex.size)
        /*
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
        */
        builder.toSparseVector
      }
      val wordCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = new VectorBuilder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.reserve(feats.length)
        var i = 0
        while(i < feats.length) {
          val index = compositeIndex.mapIndex(UNI_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if(index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        builder.toSparseVector
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
        if(beg < end)
          loc.featuresForSpan(beg, end).map(basicSpanFeatureIndex).filter(_ >= 0)
        else null
      }
      private val spanFeatures = Array.tabulate(labelIndex.size, labelIndex.size){ (prev, cur) =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          val ok = constraints.forall(_.allowedLabels(beg, end).contains(cur))
          if(!ok) {
            null
          } else {
            val builder = new VectorBuilder[Double](featureIndex.size)
            if(beg < end) {
              val feats = spanCache(beg, end)
              val tfeats = loc.featuresForTransition(beg, end).map(basicTransFeatureIndex)
              builder.reserve(feats.length + tfeats.length)
              var i = 0
              while(i < feats.length) {
                val index = compositeIndex.mapIndex(SPAN_COMP, spanFeatureIndex.mapIndex(cur, feats(i)))
                if(index != -1) {
                  builder.add(index, 1.0)
                }
                i += 1
              }
              i = 0
              while(i < tfeats.length) {
                val index = compositeIndex.mapIndex(TRANS_COMP, transFeatureIndex.mapIndex(label2Index.mapIndex(prev, cur), tfeats(i)))
                if(index != -1) {
                  builder.add(index, 1.0)
                }
                i += 1
              }
            }
            builder.toSparseVector
          }
        }
      }


      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): SparseVector[Double] = {
        spanFeatures(prev)(cur)(beg,end)
      }

    }
  }


}
