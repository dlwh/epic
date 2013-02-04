package epic.everything

import breeze.util.Index
import epic.framework._
import breeze.linalg._
import breeze.collection.mutable.TriangularArray
import collection.immutable.BitSet
import epic.ontonotes.Argument
import epic.trees.Span
import epic.sequences.{SemiCRFInference, SemiCRF, Segmentation, SegmentationEval}
import collection.mutable.ArrayBuffer
import collection.mutable
import epic.sequences.SemiCRF.TransitionVisitor
import scala.Some
import epic.ontonotes.Argument
import epic.sequences.Segmentation
import epic.everything.ChainNER.Label1Feature
import epic.trees.Span

/**
 *
 * @author dlwh
 */
object SRL {
  class Model(factory: SentenceBeliefs.Factory,
              val labelIndex: Index[Option[String]],
              outsideLabel: String,
              val featurizer: IndexedFeaturizer,
              initialWeights: Feature=>Option[Double] = {(_: Feature) => None}) extends EvaluableModel[FeaturizedSentence] with StandardExpectedCounts.Model with Serializable {
    assert(labelIndex(Some(outsideLabel)) != -1)

    def featureIndex = featurizer.featureIndex

    type Marginal = SRL.Marginal
    type Inference = SRL.Inference

    def initialValueForFeature(f: Feature): Double = initialWeights(f).getOrElse(0.0)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SRL.Inference(factory, labelIndex, outsideLabel, weights, featurizer)

    type EvaluationResult = SegmentationEval.Stats

    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence): EvaluationResult = {
      val pieces = guess.frames.zip(gold.frames) map { case (guessf, goldf) =>
        SegmentationEval.evaluateExample(Set(outsideLabel,"V"), asSegments(guess.words, guessf.args), asSegments(gold.words, goldf.args))
      }

      pieces.foldLeft(new SegmentationEval.Stats())(_ + _)

    }

    private def asSegments(words: IndexedSeq[String], frame: IndexedSeq[Argument]): Segmentation[String, String] = {
      val sorted = frame.sortBy(_.span.start)
      var out = new ArrayBuffer[(String, Span)]()
      var last = 0
      for( arg <- sorted ) {
        assert(last <= arg.span.start)
        while(arg.span.start != last) {
          out += (outsideLabel -> Span(last,last+1))
          last += 1
        }
        out += (arg.arg -> Span(arg.span.start, arg.span.end))
        last = arg.span.end
      }
      while(words.length != last) {
        out += (outsideLabel -> Span(last,last+1))
        last += 1
      }

      Segmentation(out, words)
    }
  }

  case class Marginal(frames: IndexedSeq[SemiCRF.Marginal[Option[String], String]]) extends epic.framework.Marginal {
    def logPartition: Double = frames.map(_.logPartition).sum
  }

  class Inference(beliefsFactory: SentenceBeliefs.Factory,
                  labelIndex: Index[Option[String]],
                  outsideLabel: String,
                  weights: DenseVector[Double],
                  featurizer: IndexedFeaturizer) extends ProjectableInference[FeaturizedSentence, SentenceBeliefs] with AnnotatingInference[FeaturizedSentence] {
    type Marginal = SRL.Marginal
    type ExpectedCounts = StandardExpectedCounts[Feature]

    val notSRL = labelIndex(None)


    def emptyCounts = StandardExpectedCounts.zero(featurizer.featureIndex)

    def numLabels = labelIndex.size

    def baseAugment(doc: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(doc)
    }


    def annotate(s: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val pieces =  for ((margf, fi) <- m.frames.zipWithIndex) yield {
        val segments: Segmentation[Option[String], String] = SemiCRF.posteriorDecode(margf)
        s.frames(fi).copy(args=segments.label.collect { case (Some(l),span) => Argument(l,span)})
      }

      s.copy(frames=pieces)
    }

    def marginal(s: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val pieces =  for ((f, fi) <- s.frames.zipWithIndex) yield {
        SemiCRF.Marginal(new Anchoring(featurizer.anchor(s, f.lemma, f.pos), s.words, labelIndex, outsideLabel, weights, aug, fi))
      }
      new Marginal(pieces)
    }



    def goldMarginal(s: FeaturizedSentence, augment: SentenceBeliefs): Marginal = {
      val pieces = for ((f, fi) <- s.frames.zipWithIndex) yield {
        val seg = f.stripEmbedded.asSegments(s.words)
        val anchoring = new Anchoring(featurizer.anchor(s, f.lemma, f.pos), s.words, labelIndex, outsideLabel, weights, augment, fi)
        SemiCRF.Marginal.goldMarginal(anchoring, seg)
      }

      new Marginal(pieces)
    }

    def countsFromMarginal(s: FeaturizedSentence, marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
      counts.loss += marg.logPartition * scale
      for ( f <- marg.frames) {
        val localization = f.anchoring.asInstanceOf[Anchoring].featurizer
        f visit new TransitionVisitor[Option[String], String] {
          def apply(prev: Int, cur: Int, beg: Int, end: Int, count: Double) {
            if (count != 0.0) {
              try {
                for (f <- localization.featuresFor(beg, end, cur))
                  counts.counts(f) += count * scale
              } catch {
                case ex =>
                  throw new RuntimeException(s.words + " " + count + " " + beg + " " + end + " " + labelIndex.get(cur) + " " + s.frames(f.anchoring.asInstanceOf[Anchoring].frameIndex) + " " + s.isPossibleMaximalSpan(beg,end) + " " + s.constituentSparsity.activeLabelsTop(beg,end).map(f.anchoring.asInstanceOf[Anchoring].sentenceBeliefs.spanBeliefs(0,s.length).label.property.index.get(_)))
              }
            }
          }
        }
      }
      counts
    }

    def project(sent: FeaturizedSentence, marg: Marginal, sentenceBeliefs: SentenceBeliefs): SentenceBeliefs = {
      assert(!marg.logPartition.isInfinite, "Infinite partition! " + sent)
      val newSpans = TriangularArray.tabulate(sent.length+1){ (b,e) =>
        if (b < e) {
          val spanBeliefs = sentenceBeliefs.spanBeliefs(b, e)
          if (spanBeliefs eq null) {
            null
          } else {
            val newFrames: IndexedSeq[Beliefs[Option[String]]] = for(i <- 0 until marg.frames.size) yield {
              val beliefs = DenseVector.tabulate(labelIndex.size) {
                marg.frames(i).spanMarginal(_, b, e)
              }
              assert(beliefs(notSRL) == 0.0, beliefs)
              if (spanBeliefs.frames(i)(notSRL) == 0.0 || beliefs(notSRL) < 0.0) {
                beliefs(notSRL) = 0.0
              } else {
                beliefs(notSRL) = 1 - breeze.linalg.sum(beliefs)
              }
              val normalizer: Double = breeze.linalg.sum(beliefs)
              beliefs /= normalizer
              Beliefs(spanBeliefs.frames(i).property, beliefs)
            }
            spanBeliefs.copy(frames = newFrames)
          }
        } else {
          null
        }
      }
      sentenceBeliefs.copy(spans=newSpans)
    }

  }

  trait IndexedFeaturizer {
    def featureIndex: Index[Feature]
    def anchor(fs: FeaturizedSentence, lemma: String, pos: Int):FeatureAnchoring
  }

  trait FeatureAnchoring {
    def featuresFor(begin: Int, end: Int, label: Int):Array[Int]
  }

  class Anchoring(val featurizer: FeatureAnchoring,
                  val words: IndexedSeq[String],
                  val labelIndex: Index[Option[String]],
                  val outsideLabel: String,
                  weights: DenseVector[Double],  val sentenceBeliefs: SentenceBeliefs, val frameIndex: Int) extends SemiCRF.Anchoring[Option[String], String] {

    def startSymbol: Option[String] = Some(outsideLabel)

    val iNone = labelIndex(None)
    val iOutside = labelIndex(Some(outsideLabel))

    def maxSegmentLength(label: Int): Int = if(label == iNone) 0 else if(label == iOutside) 1 else 50


    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      val init = beliefPiece(prev, cur, beg, end)
      if (init != Double.NegativeInfinity)
        init + score(beg, end, cur)
      else Double.NegativeInfinity
    }

    // (same trick as in parser:)
    // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
    // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
    // but the dynamic program does not visit all spans for all parses, only those
    // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
    // and then we divide out p(not span) for spans in the tree.

    val normalizingPiece = sentenceBeliefs.spans.data.filter(_ ne null).map { b =>
      val notNerScore = b.frames(frameIndex).beliefs(iNone)

      if (notNerScore < 1E-6) 0.0 else math.log(notNerScore)
    }.sum

    private def beliefPiece(prev: Int, cur: Int, beg: Int, end: Int): Double = {
      val score = if (cur == iNone) Double.NegativeInfinity
      else if (sentenceBeliefs.spanBeliefs(beg, end).eq(null) || sentenceBeliefs.spanBeliefs(beg, end).frames(frameIndex)(cur) == 0.0) Double.NegativeInfinity
      else if (sentenceBeliefs.spanBeliefs(beg, end).frames(frameIndex)(iNone) < 1E-6) {
        math.log(sentenceBeliefs.spanBeliefs(beg,end).frames(frameIndex)(cur))
      } else {
        math.log(sentenceBeliefs.spanBeliefs(beg,end).frames(frameIndex)(cur) / sentenceBeliefs.spanBeliefs(beg,end).frames(frameIndex)(iNone))
      }

      if (beg == 0) score + normalizingPiece else score
    }


    def score(begin: Int, end: Int, label: Int):Double = {
      val init = sentenceBeliefs.spanBeliefs(begin, end).frames(frameIndex)(label)
      if (init == Double.NegativeInfinity) Double.NegativeInfinity
      else {
        val feats: Array[Int] = featurizer.featuresFor(begin, end, label)
        if (feats eq null) {
          Double.NegativeInfinity
        } else {
          init + dot(feats, weights)
        }
      }
    }

    private def dot(features: Array[Int], weights: DenseVector[Double]) = {
      var i =0
      var score = 0.0
      if (features eq null) {
        Double.NegativeInfinity
      } else {
        while(i < features.length) {
          score += weights(features(i))
          i += 1
        }
        score
      }
    }
  }

  class ModelFactory(factory: SentenceBeliefs.Factory,
                  processor: FeaturizedDocument.Factory,
                  weights: Feature=>Option[Double] = { (f:Feature) => None}) {

    def makeModel(sents: IndexedSeq[FeaturizedSentence]) = {
      val frames = sents.flatMap(_.frames)
      val lemmaIndex = Index(frames.iterator.map(_.lemma))


      val featurizer = new StandardFeaturizer(factory.srlProp.index, lemmaIndex, processor.wordFeatureIndex, processor.spanFeatureIndex)
      val model = new Model(factory, factory.srlProp.index, factory.srlOutsideLabel, featurizer, weights(_))

      model
    }
  }

  object Features {
    case class DistanceToPredFeature(dir: Symbol, label: Any, voice: Symbol, dist: Int) extends Feature
    case object LemmaContainedFeature extends Feature
  }
  import Features._

  case class StandardFeaturizer(labelIndex: Index[Option[String]],
                                lemmaIndex: Index[String],
                                baseWordFeatureIndex: Index[Feature],
                                baseSpanFeatureIndex: Index[Feature]) extends IndexedFeaturizer { outer =>

    val kinds = Array('Begin, 'Interior, 'End)
    val propKinds = Array('PassiveProp, 'ActiveProp)
    val leftRight = Array('Left, 'Right)

    println(lemmaIndex.size + " " + labelIndex.size)

    private def binDistance(dist2:Int) = {
      val dist = dist2.abs - 1
      if (dist >= 20) 4
      else if (dist >= 10) 3
      else if (dist >= 5) 2
      else if (dist >= 2) 1
      else 0
    }

    private def numDistBins = 5


    val (featureIndex: Index[Feature], wordFeatures, spanFeatures, distanceToLemmaFeatures, lemmaContainedFeature) = {
      val featureIndex = Index[Feature]()
//      val labelFeatures = Array.tabulate(labelIndex.size, lemmaIndex.size + 1, kinds.length, baseWordFeatureIndex.size) { (l, lem, k, f) =>
//        if(lem == lemmaIndex.size)
//          featureIndex.index(Label1Feature(labelIndex.get(l), baseWordFeatureIndex.get(f), kinds(k)))
//        else
//          featureIndex.index(Label1Feature( (labelIndex.get(l) + lemmaIndex.get(lem)).intern, baseWordFeatureIndex.get(f), kinds(k)))
//      }
      val labelFeatures = null

      val spanFeatures = Array.tabulate(labelIndex.size, lemmaIndex.size + 1, baseSpanFeatureIndex.size) { (l, lem, f) =>
        if(lem == lemmaIndex.size)
          featureIndex.index(Label1Feature(labelIndex.get(l), baseSpanFeatureIndex.get(f), 'Span))
        else -1
//        else
//          featureIndex.index(Label1Feature((labelIndex.get(l) + " " + lemmaIndex.get(lem)).intern, baseSpanFeatureIndex.get(f), 'Span))
      }

      val distanceToLemmaFeatures = Array.tabulate(labelIndex.size, lemmaIndex.size + 1, propKinds.length, 2, numDistBins) { (l, lem, kind, dir, dist) =>
        if(lem == lemmaIndex.size)
          featureIndex.index(DistanceToPredFeature(leftRight(dir), labelIndex.get(l), propKinds(kind), dist))
        else
          featureIndex.index(DistanceToPredFeature(leftRight(dir), (labelIndex.get(l) + " " + lemmaIndex.get(lem)).intern, propKinds(kind), dist))
      }

      val lemmaContainedFeature = featureIndex.index(LemmaContainedFeature)

      (featureIndex, labelFeatures, spanFeatures, distanceToLemmaFeatures, lemmaContainedFeature)
    }

    println("SRL features: " + featureIndex.size)


    def anchor(fs: FeaturizedSentence, lemma: String, pos: Int): FeatureAnchoring = {
      new Anchoring(fs, lemma, pos)

    }

    class Anchoring(fs: FeaturizedSentence, lemma: String, pos: Int) extends FeatureAnchoring {
      val voiceIndex = if(pos > 0 &&
        Set("was", "were", "being", "been").contains(fs.words(pos-1))
        || (pos > 1 && Set("was", "were", "being", "been").contains(fs.words(pos-2)))) 0 else 1

      val lemmaInd = lemmaIndex(lemma)

      def featuresFor(begin: Int, end: Int, label: Int): Array[Int] = {
        this.spanFeatures(label)(begin, end)
      }

      val beginCache = Array.tabulate(labelIndex.size, fs.words.length){ (label,w) =>
//        val feats = fs.wordFeatures(w)
        val builder = Array.newBuilder[Int]
//        builder.sizeHint(if(lemmaInd == -1) feats.length else 2 * feats.length)
//        appendFeatures(builder, feats, wordFeatures(label)(lemmaIndex.size)(0))
//        if(lemmaInd >= 0)
//          appendFeatures(builder, feats, wordFeatures(label)(lemmaInd)(0))
        builder.result()
      }


      private def appendFeatures(builder: mutable.ArrayBuilder[Int], rawFeatures: Array[Int], labeledFeatures: Array[Int]) {
        var i = 0
        while (i < rawFeatures.length) {
          builder += labeledFeatures(rawFeatures(i))
          i += 1
        }
      }

      val endCache = Array.tabulate(labelIndex.size, fs.words.length){ (label,w) =>
//        val feats = fs.wordFeatures(w)
        val builder = Array.newBuilder[Int]
//        builder.sizeHint(if(lemmaInd == -1) feats.length else 2 * feats.length)
//        appendFeatures(builder, feats, wordFeatures(label)(lemmaIndex.size)(2))
//        if(lemmaInd >= 0)
//          appendFeatures(builder, feats, wordFeatures(label)(lemmaInd)(2))
        builder.result()
      }

      val interiorCache = Array.tabulate(labelIndex.size, fs.words.length){ (label,w) =>
//        val feats = fs.wordFeatures(w)
        val builder = Array.newBuilder[Int]
//        builder.sizeHint(if(lemmaInd == -1) feats.length else 2 * feats.length)
//        appendFeatures(builder, feats, wordFeatures(label)(lemmaIndex.size)(1))
//        if(lemmaInd >= 0)
//          appendFeatures(builder, feats, wordFeatures(label)(lemmaInd)(1))
        builder.result()
      }


      def featuresForBegin(cur: Int, pos: Int): Array[Int] = {
        beginCache(cur)(pos)
      }

      def featuresForEnd(cur: Int, pos: Int): Array[Int] = {
        endCache(cur)(pos-1)
      }

      def featuresForInterior(cur: Int, pos: Int): Array[Int] = {
        interiorCache(cur)(pos)
      }


      private val spanFeatures: Array[TriangularArray[Array[Int]]] = Array.tabulate(labelIndex.size){ label =>
        TriangularArray.tabulate(fs.words.length+1) { (beg, end) =>
          if(!fs.isPossibleMaximalSpan(beg, end) || beg == end || (pos < end && pos >= beg && (end-beg) > 1)) {
            null
          } else {
            val acc = new ArrayBuffer[Array[Int]]()
            val _begin = featuresForBegin(label, beg)
            acc += _begin
            val _end = featuresForEnd(label, end)
            acc += _end

            var p = beg+1
            while(p < end) {
              val w = featuresForInterior(label, p)
              acc += w
              p += 1
            }

            val builder = Array.newBuilder[Int]
            builder.sizeHint(acc.map(_.size).sum)
            var i = 0
            while(i < acc.size) {
              builder ++= acc(i)
              i += 1
            }
            val forSpan = fs.featuresForSpan(beg, end)
            appendFeatures(builder, forSpan, outer.spanFeatures(label)(lemmaIndex.size))
//            if(lemmaInd >= 0)
//              appendFeatures(builder, forSpan, outer.spanFeatures(label)(lemmaInd))

           val dir = if(pos < beg) 0 else 1

            builder += distanceToLemmaFeatures(label)(lemmaIndex.size)(voiceIndex)(dir)(binDistance(beg - pos))
            if(lemmaInd >= 0)
              builder += distanceToLemmaFeatures(label)(lemmaInd)(voiceIndex)(dir)(binDistance(beg - pos))
            builder.result()
          }
        }
      }





    }
  }

}
