package epic.sequences

import epic.framework._
import breeze.util._
import breeze.linalg.{VectorBuilder, Counter, DenseVector, axpy}
import epic.sequences.SemiCRF.TransitionVisitor
import collection.mutable.ArrayBuffer
import epic.parser.features.{PairFeature, SpanShapeGenerator}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import breeze.collection.mutable.TriangularArray
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges
import collection.mutable
import breeze.features.FeatureVector
import epic.features.WordShapeFeaturizer

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


    val wordCounts:Counter[String, Double] = Counter.count(train.flatMap(_.words):_*).mapValues(_.toDouble)
    val nonStarters = wordCounts.activeIterator.filter(_._2 > 5).map(_._1).toSet -- starters
    println(nonStarters)
    val nonInteriors = wordCounts.activeIterator.filter(_._2 > 8).map(_._1).toSet -- interiors
    println(nonInteriors)
    val f = new StandardFeaturizer[L](gazetteer, wordCounts)
    val basicFeatureIndex = Index[Feature]()
    val spanFeatureIndex = Index[Feature]()
    val transFeatureIndex = Index[Feature]()

    val maxMaxLength = (0 until labelIndex.size).map(maxLengthArray).max
    var i = 0
    for(s <- train) {
      val loc = f.localize(s.words)
      val possibleInteriors = s.words.map(!nonInteriors(_))

      for(b <- 0 until s.length) {
        loc.featuresForWord(b) foreach {basicFeatureIndex.index _}
        for(e <- (b+1) to math.min(s.length,b+maxMaxLength)) {
          if (b < e - 1 || (!nonStarters(s.words(b)) && (b until e).forall(possibleInteriors))) {
            loc.featuresForSpan(b, e) foreach {spanFeatureIndex.index _}
            loc.featuresForTransition(b, e) foreach {transFeatureIndex.index _}
          }
        }

      }
      i += 1
    }

    for(f <- pruningModel) {
      assert(f.labelIndex == labelIndex, f.labelIndex + " " + labelIndex)
    }
    val indexed = new IndexedStandardFeaturizer[L](f, startSymbol, nonStarters, nonInteriors, labelIndex, basicFeatureIndex, spanFeatureIndex, transFeatureIndex, maxLengthArray(_), pruningModel)
    val model = new SemiCRFModel(indexed.featureIndex, indexed, maxLengthArray, weights(_))

    model
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

  /**
   * Computes basic features from word counts
   * @param wordCounts
   */
  @SerialVersionUID(1L)
  class StandardFeaturizer[L](gazetteer: Gazetteer[Any, String], wordCounts: Counter[String, Double] ) extends Serializable {
    val inner = new WordShapeFeaturizer(wordCounts)

    def localize(words: IndexedSeq[String])= new Localization(words)

    val interner = new Interner[Feature]

    val noShapeThreshold = 100
    class Localization(words: IndexedSeq[String]) {
      private val classes = words.map(w => if (wordCounts(w) > noShapeThreshold) w else EnglishWordClassGenerator(w))
      private val shapes = words.map(w => if (wordCounts(w) > noShapeThreshold) w else WordShapeGenerator(w))

      val basicFeatures = (0 until words.length) map { i =>
        val w = words(i)
        if (wordCounts(w) > 10) IndexedSeq(w)
        else if (wordCounts(w) > 5) IndexedSeq(w, classes(i), shapes(i))
        else IndexedSeq(classes(i), shapes(i))
      } map {_.map(_.intern)}

      def basicFeature(pos: Int) = {
        if (pos < 0 || pos >= words.length) IndexedSeq("#")
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
        if (pos > 0 && pos < words.length - 1) {
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
        if (start < end - 1) {
          feats += WordEdges('Inside, basicFeature(start)(0), basicFeature(end-1)(0))
          feats += WordEdges('Outside, basicFeature(start-1)(0), basicFeature(end)(0))
          feats += WordEdges('Begin, basicFeature(start-1)(0), basicFeature(start)(0))
          feats += WordEdges('End, basicFeature(end-1)(0), basicFeature(end)(0))
          feats += SFeature(SpanShapeGenerator.apply(words, Span(start,end)), 'SpanShape)
        }
        if (start == 0)
          feats += SpanStartsSentence
        if (start == 0 && end == words.length)
          feats += SpansWholeSentence

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
  class IndexedStandardFeaturizer[L](
                                  f: StandardFeaturizer[L],
                                  val startSymbol: L,
                                  val nonStarters: Set[String],
                                  val nonInteriors: Set[String],
                                  val labelIndex: Index[L],
                                  val basicFeatureIndex: Index[Feature],
                                  val basicSpanFeatureIndex: Index[Feature],
                                  val basicTransFeatureIndex: Index[Feature],
                                  val maxLength: Int=>Int,
                                  val pruningModel: Option[SemiCRF.ConstraintGrammar[L, String]] = None) extends SemiCRFModel.BIEOFeaturizer[L,String] with Serializable {
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

      val starters = Array.tabulate(w.length)(i => !nonStarters(w(i)))
      val interiors = Array.tabulate(w.length)(i => !nonInteriors(w(i)))

      def canStartRealSpan(beg: Int): Boolean = starters(beg)
      def canBeInterior(i: Int): Boolean = interiors(i)


      private def okSpan(beg: Int, end: Int) = (beg + 1 == end) || {
        var ok = canStartRealSpan(beg)
        var pos = beg + 1
        while(pos < end && ok) {
          ok = canBeInterior(pos)
          pos += 1
        }
        ok
      }

      val basicFeatureCache = Array.tabulate(w.length){ pos =>
        val feats =  loc.featuresForWord(pos)
        feats.map(basicFeatureIndex).filter(_ >= 0)
      }


      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){ (p,c,w) =>
        val ok = constraints.forall(_.allowedStarts(w)(c))
        if (!ok) {
          null
        }  else {
          val builder = mutable.ArrayBuilder.make[Int]
          val feats = basicFeatureCache(w)
          builder.sizeHint(feats.size)
          var i = 0
          while (i < feats.length) {
            val index = compositeIndex.mapIndex(BEGIN_COMP, labeledFeatureIndex.mapIndex(c, feats(i)))
            if (index != -1) {
              builder += index
            }

            i += 1
          }
          new FeatureVector(builder.result())
        }
      }
      val endCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        /*
        val builder = new VectorBuilder[Double](featureIndex.size)
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while (i < feats.length) {
          val index = compositeIndex.mapIndex(END_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if (index != -1) {
            builder.add(index, 1.0)
          }

          i += 1
        }
        */
        new FeatureVector(Array.empty[Int])
      }
      val wordCache = Array.tabulate(labelIndex.size, w.length){ (l, w) =>
        val builder = mutable.ArrayBuilder.make[Int]
        val feats = basicFeatureCache(w)
        builder.sizeHint(feats.length)
        var i = 0
        while (i < feats.length) {
          val index = compositeIndex.mapIndex(UNI_COMP, labeledFeatureIndex.mapIndex(l, feats(i)))
          if (index != -1) {
            builder += index
          }

          i += 1
        }
        new FeatureVector(builder.result())
      }

      def featureIndex: Index[Feature] = IndexedStandardFeaturizer.this.featureIndex

      def featuresForBegin(prev: Int, cur: Int, pos: Int): FeatureVector = {
        beginCache(prev)(cur)(pos)
      }

      def featuresForEnd(cur: Int, pos: Int): FeatureVector = {
        endCache(cur)(pos-1)
      }

      def featuresForInterior(cur: Int, pos: Int): FeatureVector = {
        wordCache(cur)(pos)
      }

      private val spanCache = TriangularArray.tabulate(w.length+1){ (beg,end) =>
        if (beg < end)
          loc.featuresForSpan(beg, end).map(basicSpanFeatureIndex).filter(_ >= 0)
        else null
      }

      private lazy val justSpanFeatures = Array.tabulate(labelIndex.size){ cur =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          if (beg == end || maxLength(cur) < (end-beg) || !okSpan(beg,end)  || !(constraints == None || constraints.get.allowedLabels(beg, end).contains(cur))) {
            null
          } else {
            val builder = mutable.ArrayBuilder.make[Int]
            val feats = spanCache(beg, end)
            builder.sizeHint(feats.size)
            var i = 0
            while (i < feats.length) {
              val index = compositeIndex.mapIndex(SPAN_COMP, spanFeatureIndex.mapIndex(cur, feats(i)))
              if (index != -1) {
                builder += index
              }
              i += 1
            }
            new FeatureVector(builder.result)
          }
        }
      }

      private lazy val spanFeatures = Array.tabulate(labelIndex.size, labelIndex.size){ (prev, cur) =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          if (justSpanFeatures(cur)(beg,end) == null) {
            null
          } else {
            val builder = mutable.ArrayBuilder.make[Int]
            val tfeats = spanCache(beg, end)
            builder.sizeHint(tfeats.size)
            var i = 0
            while (i < tfeats.length) {
              val index = compositeIndex.mapIndex(TRANS_COMP, transFeatureIndex.mapIndex(label2Index.mapIndex(prev, cur), tfeats(i)))
              if (index != -1) {
                builder += index
              }
              i += 1
            }
            new FeatureVector(builder.result ++ justSpanFeatures(cur)(beg,end).data)
          }
        }
      }


      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): FeatureVector = {
        spanFeatures(prev)(cur)(beg,end)
      }



    }
  }


}
