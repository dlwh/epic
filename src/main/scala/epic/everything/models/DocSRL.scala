
/*package epic.everything.models

import breeze.util._
import epic.framework.{AugmentableInference, StandardExpectedCounts, Feature}
import epic.everything.ProcessedDocument
import breeze.collection.mutable.TriangularArray
import breeze.linalg.{VectorBuilder, Counter, SparseVector, DenseVector}
import collection.immutable.BitSet
import epic.parser.features.{SpanShapeGenerator, PairFeature, WordShapeFeaturizer}
import breeze.text.analyze.{WordShapeGenerator, EnglishWordClassGenerator}
import collection.immutable
import collection.mutable.ArrayBuffer
import epic.parser.features.PairFeature
import epic.trees.Span
import epic.parser.features.StandardSpanFeatures.WordEdges
import epic.ontonotes.Document

/**
 * 
 *
 * @author dlwh
 */
object DocSRL {

  class Model(factory: DocumentBeliefs.Factory,
              val labelIndex: Index[String],
              val featureIndex: Index[Feature],
              val featurizer: IndexedFeaturizer,
              initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends epic.framework.Model[ProcessedDocument] with StandardExpectedCounts.Model with Serializable {

    //  def extractCRF(weights: DenseVector[Double]) = {
    //    val grammar = inferenceFromWeights(weights)
    //    new SemiCRF(grammar)
    //  }

    type Marginal = DocSRL.Marginal
    type Inference = DocSRL.Inference

    def initialValueForFeature(f: Feature): Double = initialWeights(f)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new DocSRL.Inference(factory, labelIndex, weights, featureIndex, featurizer)

  }

  case class Marginal(frames: IndexedSeq[IndexedSeq[FrameMarginal]]) extends epic.framework.Marginal {
    def logPartition: Double = frames.map(_.map(_.logPartition).sum).sum
  }
  case class FrameMarginal(logPartition: Double, arr: TriangularArray[Beliefs[Option[String]]], anchoring: Anchoring) extends epic.framework.Marginal

  class Inference(beliefsFactory: DocumentBeliefs.Factory,
                  labelIndex: Index[String],
                  weights: DenseVector[Double],
                  featureIndex: Index[Feature],
                  featurizer: IndexedFeaturizer) extends AugmentableInference[ProcessedDocument, DocumentBeliefs] {
    type Marginal = DocSRL.Marginal
    type ExpectedCounts = StandardExpectedCounts[Feature]

    def emptyCounts = StandardExpectedCounts.zero(featureIndex)

    def numLabels = labelIndex.size

    def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
      beliefsFactory(doc)
    }


    def marginal(v: ProcessedDocument, aug: DocumentBeliefs): Marginal = {
      val pieces =  for (s <- v.sentences) yield {
        for ((f, fi) <- s.frames.zipWithIndex) yield {
          val anchoring = new Anchoring(featurizer.anchor(s.validConstituents,  s.words, f.lemma, f.pos), weights, aug.beliefsForSentence(s.index), fi)
          var partition = 0.0
          val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
            if (s.isPossibleConstituent(begin, end)) {
              val b = DenseVector.tabulate(numLabels)(l => math.exp(anchoring.score(begin, end, l)))
              partition += math.log(breeze.linalg.sum(b))
              aug.beliefsForSentence(s.index).spanBeliefs(begin, end).frames(fi).copy(beliefs=b)
            } else {
              null
            }

          }
         new FrameMarginal(partition, arr, anchoring)

        }
      }

      new Marginal(pieces)
    }

    private val labelZeros = {
      val z = DenseVector.zeros[Double](labelIndex.size + 1)
      z(labelIndex.size) = 1.0
      z
    }

    def goldMarginal(v: ProcessedDocument, augment: DocumentBeliefs): Marginal = {
      val pieces =  for (s <- v.sentences) yield {
        for ((f, fi) <- s.frames.zipWithIndex) yield {
          val anchoring = new Anchoring(featurizer.anchor(s.validConstituents,  s.words, f.lemma, f.pos), weights, augment.beliefsForSentence(s.index), fi)
          var partition = 0.0
          val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
            if (s.isPossibleConstituent(begin, end)) {
              augment.beliefsForSentence(s.index).spanBeliefs(begin, end).frames(fi).copy(beliefs=labelZeros)
            } else {
              null
            }
          }
          for (arg <- f.args) {
            val argy = DenseVector.zeros[Double](labelIndex.size + 1)
            val label: Int = labelIndex(arg.arg)
            argy(label) = 1.0
            partition += anchoring.score(arg.span.start, arg.span.end, label)
            arr(arg.span.start, arg.span.end) = arr(arg.span.start, arg.span.end).copy(beliefs=argy)
          }

          new FrameMarginal(partition, arr, anchoring)
        }
      }

      new Marginal(pieces)
    }

    def countsFromMarginal(v: ProcessedDocument, marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
      counts.loss += marg.logPartition * scale
      for ( (s, frms) <- v.sentences zip marg.frames; f <- frms) {
        val localization = f.anchoring.featurizer
        for ( begin <- 0 until s.length; end <- (begin+1) to s.length if s.isPossibleConstituent(begin, end); l <- 0 to labelIndex.size) {
          val score = f.arr(begin, end).beliefs(l)
          if (score != 0.0)
            breeze.linalg.axpy(score * scale, localization.featuresFor(begin, end, l), counts.counts)
        }

      }
      counts

    }

  }

  trait IndexedFeaturizer {
    def anchor(validSpans: BitSet, words: IndexedSeq[String], lemma: String, pos: Int):FeatureAnchoring
  }

  trait FeatureAnchoring {
    def featuresFor(begin: Int, end: Int, label: Int):SparseVector[Double]
  }


  class Anchoring(val featurizer: FeatureAnchoring, weights: DenseVector[Double],  sentenceBeliefs: SentenceBeliefs, frameIndex: Int) {
    def score(begin: Int, end: Int, label: Int):Double = {
      val init = sentenceBeliefs.spanBeliefs(begin, end).frames(frameIndex).beliefs(label)
      if (init <= 0.0) Double.NegativeInfinity
      else math.log(init) + (featurizer.featuresFor(begin, end, label) dot weights)
    }
  }




  class ModelFactory(weights: Feature=>Double = { (f:Feature) => 0.0}) {

  def makeModel(train: IndexedSeq[Document]) = {
    val sents = train.flatMap(_.sentences)
    val frames = sents.flatMap(_.srl)
    val labelIndex: Index[String] = Index[String](frames.flatMap(_.args.map(_.arg)))
    val lemmaIndex = Index(frames.iterator.map(_.lemma))

    val wordCounts:Counter[String, Double] = Counter.count(sents.flatMap(_.words):_*).mapValues(_.toDouble)
    val f = new StandardFeaturizer(wordCounts, lemmaIndex)
    val basicFeatureIndex = Index[Feature]()
    val spanFeatureIndex = Index[Feature]()
    val transFeatureIndex = Index[Feature]()

    var i = 0
    for(s <- sents; frame <- s.srl) {
      val loc = f.localize(s.words, frame.lemma, frame.pos)

      for(b <- 0 until s.length) {
        loc.featuresForWord(b) foreach {basicFeatureIndex.index _}
        for(e <- (b+1) to s.length) {
          loc.featuresForSpan(b, e) foreach {spanFeatureIndex.index _}
        }

      }
      i += 1
    }

    val indexed = new IndexedStandardFeaturizer(f, labelIndex, basicFeatureIndex, spanFeatureIndex)
    val model = new Model(indexed.featureIndex, labelIndex, indexed, weights(_))

    model
  }

}

  object Features {
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
  }
  import Features._

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
                                  val labelIndex: Index[String],
                                  val basicFeatureIndex: Index[Feature],
                                  val basicSpanFeatureIndex: Index[Feature]) extends IndexedFeaturizer with Serializable {
    // feature mappings... sigh
    // basically we want to build a big index for all features
    // (beginFeatures ++ endFeatures ++ unigramFeatures ++ spanFeatures ++ transitionFeatures)
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

    val compositeIndex = new CompositeIndex[Feature](new IsomorphismIndex(labeledFeatureIndex)(beginIso),
      //      new IsomorphismIndex(labeledFeatureIndex)(endIso),
      new IsomorphismIndex(labeledFeatureIndex)(uniIso),
      new IsomorphismIndex(spanFeatureIndex)(spanIso)
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

    def anchor(w: IndexedSeq[String], lemma: String, pos: Int): FeatureAnchoring = new FeatureAnchoring {
      val loc = f.localize(w, lemma, pos)

      val basicFeatureCache = Array.tabulate(w.length){ pos =>
        val feats =  loc.featuresForWord(pos)
        feats.map(basicFeatureIndex).filter(_ >= 0)
      }


      val beginCache = Array.tabulate(labelIndex.size, labelIndex.size, w.length){ (p,c,w) =>
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

      private val spanFeatures = Array.tabulate(labelIndex.size){ cur =>
        TriangularArray.tabulate(w.length+1) { (beg, end) =>
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

      def featuresForSpan(beg: Int, end: Int, cur: Int): SparseVector[Double] = {
        spanFeatures(cur)(beg,end)
      }

    }
  }


}
*/
