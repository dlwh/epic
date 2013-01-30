package epic.srl

import epic.sequences.{Gazetteer, Segmentation, SemiCRFModel, SemiCRF}
import breeze.util._
import epic.framework.{AugmentableInference, StandardExpectedCounts, Model, Feature}
import breeze.linalg.{SparseVector, VectorBuilder, Counter, DenseVector}
import collection.mutable.ArrayBuffer
import epic.parser.features.{SpanShapeGenerator, PairFeature, WordShapeFeaturizer}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges
import epic.parser.features.PairFeature
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges
import breeze.collection.mutable.TriangularArray
import breeze.util
import epic.sequences.SemiCRF.{TransitionVisitor, Anchoring}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiSRLModel(val featureIndex: Index[Feature],
                   val featurizer: SemiSRLModelFactory.IndexedStandardFeaturizer,
                   maxSegmentLength: Int=>Int,
                   initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends Model[SRLInstance] with StandardExpectedCounts.Model with Serializable {

//  def extractCRF(weights: DenseVector[Double]) = {
//    val grammar = inferenceFromWeights(weights)
//    new SemiCRF(grammar)
//  }

  type Marginal = SemiCRF.Marginal[String, String]
  type Inference = SemiSRLInference

  def initialValueForFeature(f: Feature): Double = initialWeights(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SemiSRLInference(weights, featureIndex, featurizer, maxSegmentLength)

}

object SemiSRLModel {
}

class SemiSRLInference(weights: DenseVector[Double],
                       featureIndex: Index[Feature],
                       featurizer: SemiSRLModelFactory.IndexedStandardFeaturizer,
                       val maxLength: Int=>Int) extends AugmentableInference[SRLInstance, SemiCRF.Anchoring[String, String]] {
  type Marginal = SemiCRF.Marginal[String, String]
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def viterbi(words: IndexedSeq[String], lemma: String, pos: Int) = {
    SemiCRF.viterbi(new Anchoring(words, lemma, pos, new IdentityAnchoring(words)))
  }

  def emptyCounts = StandardExpectedCounts.zero(featureIndex)

  def baseAugment(v: SRLInstance): SemiCRF.Anchoring[String, String] = new IdentityAnchoring(v.words)

  class IdentityAnchoring(val words: IndexedSeq[String]) extends SemiCRF.Anchoring[String, String] {
    def maxSegmentLength(l: Int): Int = maxLength(l)

    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def labelIndex: Index[String] = featurizer.labelIndex

    def startSymbol: String = featurizer.startSymbol
  }


  def marginal(v: SRLInstance, aug: SemiCRF.Anchoring[String, String]): Marginal = {
    SemiCRF.Marginal(new Anchoring(v.words, v.frame.lemma, v.frame.pos, aug))
  }

  def goldMarginal(v: SRLInstance, augment: SemiCRF.Anchoring[String, String]): SemiCRF.Marginal[String, String] = {
    SemiCRF.Marginal.goldMarginal[String, String](new Anchoring(v.words, v.frame.lemma, v.frame.pos, augment), v.asSegments(outsideLabel=featurizer.outsideSymbol))
  }

  def countsFromMarginal(v: SRLInstance, marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
    counts.loss += marg.logPartition * scale
    val localization = marg.anchoring.asInstanceOf[Anchoring].localization
    val visitor = new TransitionVisitor[String, String] {

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

  class Anchoring(val words: IndexedSeq[String], lemma: String, pos: Int, augment: SemiCRF.Anchoring[String, String]) extends SemiCRF.Anchoring[String, String] {
    val localization = featurizer.anchor(words, lemma, pos)
    def maxSegmentLength(l: Int): Int = SemiSRLInference.this.maxLength(l)

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

    def labelIndex: Index[String] = featurizer.labelIndex

    def startSymbol = featurizer.startSymbol
  }
}

class SemiSRLModelFactory(val startSymbol: String, val outsideSymbol: String = "O",
                          pruningModel: Option[SemiCRF.ConstraintGrammar[String, String]] = None,
                          weights: Feature=>Double = { (f:Feature) => 0.0}) {

  import SemiSRLModelFactory._

  def makeModel(train: IndexedSeq[SRLInstance]) = {
    val maxLengthMap = train.flatMap(_.args.iterator).groupBy(_.arg).mapValues(arr => arr.map(_.span.length).max)
    val labelIndex: Index[String] = Index[String](Iterator(startSymbol, outsideSymbol) ++ train.iterator.flatMap(_.label.map(_.arg)))
    val lemmaIndex = Index(train.iterator.map(_.lemma))
    val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
    maxLengthArray(1) = 1
    println(maxLengthMap)

    val wordCounts:Counter[String, Double] = Counter.count(train.flatMap(_.words):_*).mapValues(_.toDouble)
    val f = new StandardFeaturizer(wordCounts, lemmaIndex)
    val basicFeatureIndex = Index[Feature]()
    val spanFeatureIndex = Index[Feature]()
    val transFeatureIndex = Index[Feature]()

    val maxMaxLength = (0 until labelIndex.size).map(maxLengthArray).max
    var i = 0
    for(s <- train) {
      val loc = f.localize(s.words, s.lemma, s.pos)

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
    val indexed = new IndexedStandardFeaturizer(f, startSymbol, outsideSymbol, labelIndex, basicFeatureIndex, spanFeatureIndex, transFeatureIndex, maxLengthArray(_), pruningModel)
    val model = new SemiSRLModel(indexed.featureIndex, indexed, maxLengthArray, weights(_))

    model
  }

}

object SemiSRLModelFactory {
  case class SFeature(w: Any, kind: Symbol) extends Feature
  case class BeginFeature(w: Feature, cur: String) extends Feature
  case class EndFeature(w: Feature, cur: String) extends Feature
  case class TrigramFeature(a: Any, b: Any, c: Any) extends Feature
  case class SpanFeature(distance: Feature, cur: Any) extends Feature
  case class UnigramFeature(w: Feature, cur: String) extends Feature
  case class CFeature(component: Int, f: Feature) extends Feature
  case class DistanceFeature(distanceBin: Int) extends Feature
  case object TransitionFeature extends Feature
  case object SpanStartsSentence extends Feature
  case object SpansWholeSentence extends Feature
  case object ContainsPred extends Feature
  case class DistanceToPredFeature(dist: Int, dir: Symbol, voice: Symbol) extends Feature

  /**
   * Computes basic features from word counts
   * @param wordCounts
   */
  @SerialVersionUID(1L)
  class StandardFeaturizer(wordCounts: Counter[String, Double],
                           lemmas: Index[String]) extends Serializable {
    val inner = new WordShapeFeaturizer(wordCounts)

    def localize(words: IndexedSeq[String], lemma: String, pos: Int)= new Localization(words, lemma, pos)

    val interner = new Interner[Feature]
    val passivizingWords = Set("been", "is", "was", "were", "are", "being", "am")

    class Localization(words: IndexedSeq[String], lemma: String, pos: Int) {
      val classes = words.map(EnglishWordClassGenerator)
      val shapes = words.map(WordShapeGenerator)

      val predLooksPassive = words.slice(math.max(0, pos-3),pos).exists(passivizingWords) || (pos < words.length - 1 && words(pos+1) == "by")

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
        feats += TrigramFeature(basicLeft(0), basic(0), basicRight(0))
        if(pos > 0 && pos < words.length - 1) {
          feats += TrigramFeature(shapes(pos-1), shapes(pos), shapes(pos+1))
          feats += TrigramFeature(classes(pos-1), classes(pos), classes(pos+1))
        }
        feats.map(interner.intern _).toArray
      }

      def featuresForTransition(beg: Int, end: Int) = {
        val feats = ArrayBuffer[Feature](DistanceFeature(binDistance(end - beg)), TransitionFeature)
        feats.toArray
      }

      def featuresForSpan(start: Int, end: Int):Array[Feature] = {
        val feats = ArrayBuffer[Feature]()
        if (start < end - 1) {
          feats += WordEdges('Inside, basicFeature(start)(0), basicFeature(end-1)(0))
          feats += WordEdges('Outside, basicFeature(start-1)(0), basicFeature(end)(0))
          feats += WordEdges('Begin, basicFeature(start-1)(0), basicFeature(start)(0))
          feats += WordEdges('End, basicFeature(end-1)(0), basicFeature(end)(0))
          feats += SFeature(SpanShapeGenerator.apply(words, Span(start,end)), 'SpanShape)
        }
        if(start == 0)
          feats += SpanStartsSentence
        if(start == 0 && end == words.length)
          feats += SpansWholeSentence

        if(pos < start) {
          feats += DistanceToPredFeature(binDistance(start - pos), 'Right, if(predLooksPassive) 'Passive else 'Active)
        }  else if(pos >= end) {
          feats += DistanceToPredFeature(binDistance(end - pos + 1), 'Left, if(predLooksPassive) 'Passive else 'Active)
        } else {
          feats += ContainsPred
        }


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
                                  val startSymbol: String,
                                  val outsideSymbol: String,
                                  val labelIndex: Index[String],
                                  val basicFeatureIndex: Index[Feature],
                                  val basicSpanFeatureIndex: Index[Feature],
                                  val basicTransFeatureIndex: Index[Feature],
                                  val maxLength: Int=>Int,
                                  val pruningModel: Option[SemiCRF.ConstraintGrammar[String, String]] = None) extends Serializable {
    // feature mappings... sigh
    // basically we want to build a big index for all features
    // (beginFeatures ++ endFeatures ++ unigramFeatures ++ spanFeatures ++ transitionFeatures)
    private val label2Index = new PairIndex(labelIndex, labelIndex)
    private val labeledFeatureIndex = new PairIndex(labelIndex, basicFeatureIndex)
    private implicit val beginIso = Isomorphism[(String, Feature), BeginFeature](
      tu={pair => BeginFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )
    private implicit val endIso = Isomorphism[(String, Feature), EndFeature](
      tu={pair => EndFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val uniIso = Isomorphism[(String, Feature), UnigramFeature](
      tu={pair => UnigramFeature(pair._2, pair._1)},
      ut={f => (f.cur, f.w) }
    )

    private implicit val spanIso = Isomorphism[(String,  Feature), SpanFeature](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur.asInstanceOf[String], f.distance) }
    )


    private implicit val transIso = Isomorphism[((String,String),  Feature), SpanFeature](
      tu={pair => SpanFeature(pair._2, pair._1)},
      ut={f => (f.cur.asInstanceOf[(String, String)], f.distance) }
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

    private implicit val featureIso = util.Isomorphism[(Int,Feature), Feature](
      tu={pair => CFeature(pair._1, pair._2)},
      ut={f => f.asInstanceOf[CFeature].component -> f.asInstanceOf[CFeature].f}
    )

    val featureIndex: IsomorphismIndex[(Int, Feature), Feature] = new IsomorphismIndex(compositeIndex)(featureIso)
    println("Number of features: " + featureIndex.size)

    def anchor(w: IndexedSeq[String], lemma: String, pos: Int): SemiCRFModel.BIEOAnchoredFeaturizer[String, String] = new SemiCRFModel.BIEOAnchoredFeaturizer[String, String] {
      val constraints = pruningModel.map(_.constraints(w))
      val loc = f.localize(w, lemma, pos)

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

      private val justSpanFeatures = Array.tabulate(labelIndex.size){ cur =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          if(maxLength(cur) < (end-beg) ||  !(constraints == None || constraints.get.allowedLabels(beg, end).contains(cur))) {
            null
          } else {
            if(beg < end) {
              val builder = new VectorBuilder[Double](featureIndex.size)
              val feats = spanCache(beg, end)
              builder.reserve(feats.length)
              var i = 0
              while(i < feats.length) {
                val index = compositeIndex.mapIndex(SPAN_COMP, spanFeatureIndex.mapIndex(cur, feats(i)))
                if(index != -1) {
                  builder.add(index, 1.0)
                }
                i += 1
              }
              builder.toSparseVector
            } else {
              null
            }

          }
        }
      }

      private val spanFeatures = Array.tabulate(labelIndex.size, labelIndex.size){ (prev, cur) =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
          if(maxLength(cur) < (end-beg) || !(constraints == None || constraints.get.allowedLabels(beg, end).contains(cur))) {
            null
          } else {
            if(beg < end) {
              val builder = new VectorBuilder[Double](featureIndex.size)
              val tfeats = loc.featuresForTransition(beg, end).map(basicTransFeatureIndex)
              builder.reserve(tfeats.length)
              var i = 0
              while(i < tfeats.length) {
                val index = compositeIndex.mapIndex(TRANS_COMP, transFeatureIndex.mapIndex(label2Index.mapIndex(prev, cur), tfeats(i)))
                if(index != -1) {
                  builder.add(index, 1.0)
                }
                i += 1
              }
              val v = builder.toSparseVector
              v += justSpanFeatures(cur)(beg, end)
              v
            } else {
              null
            }
          }
        }
      }


      def featuresForSpan(prev: Int, cur: Int, beg: Int, end: Int): SparseVector[Double] = {
        spanFeatures(prev)(cur)(beg,end)
      }

    }
  }


}
