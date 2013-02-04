package epic.everything

import breeze.util.Index
import epic.framework._
import breeze.linalg.{Counter, SparseVector, DenseVector}
import breeze.collection.mutable.TriangularArray
import collection.immutable.BitSet
import epic.ontonotes.Argument
import epic.trees.Span
import epic.sequences.{Segmentation, SegmentationEval}
import collection.mutable.ArrayBuffer
import epic.everything.ChainNER.Label1Feature
import collection.mutable

/**
 *
 * @author dlwh
 */
object SRL {
  class Model(factory: SentenceBeliefs.Factory,
              val labelIndex: Index[String],
              val featurizer: IndexedFeaturizer,
              initialWeights: Feature=>Option[Double] = {(_: Feature) => None}) extends EvaluableModel[FeaturizedSentence] with StandardExpectedCounts.Model with Serializable {

    def featureIndex = featurizer.featureIndex

    type Marginal = SRL.Marginal
    type Inference = SRL.Inference

    def initialValueForFeature(f: Feature): Double = initialWeights(f).getOrElse(0.0)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SRL.Inference(factory, labelIndex, weights, featurizer)

    type EvaluationResult = SegmentationEval.Stats

    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence): EvaluationResult = {
      val pieces = guess.frames.zip(gold.frames) map { case (guessf, goldf) =>
        SegmentationEval.evaluateExample(Set("O","V"), asSegments(guess.words, guessf.args), asSegments(gold.words, goldf.args))
      }

      pieces.foldLeft(new SegmentationEval.Stats())(_ + _)

    }

    private def asSegments(words: IndexedSeq[String], frame: IndexedSeq[Argument], outsideLabel: String="O"): Segmentation[String, String] = {
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

  case class Marginal(frames: IndexedSeq[FrameMarginal]) extends epic.framework.Marginal {
    def logPartition: Double = frames.map(_.logPartition).sum
  }
  case class FrameMarginal(logPartition: Double, arr: TriangularArray[Beliefs[Option[String]]], anchoring: Anchoring) extends epic.framework.Marginal

  class Inference(beliefsFactory: SentenceBeliefs.Factory,
                  labelIndex: Index[String],
                  weights: DenseVector[Double],
                  featurizer: IndexedFeaturizer) extends ProjectableInference[FeaturizedSentence, SentenceBeliefs] with AnnotatingInference[FeaturizedSentence] {
    type Marginal = SRL.Marginal
    type ExpectedCounts = StandardExpectedCounts[Feature]

    def emptyCounts = StandardExpectedCounts.zero(featurizer.featureIndex)

    def numLabels = labelIndex.size

    def baseAugment(doc: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(doc)
    }


    def annotate(s: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val pieces =  for ((margf, fi) <- m.frames.zipWithIndex) yield {
        val result = collection.mutable.ArrayBuffer.empty[Argument]
        for(begin <- 0 until s.length; end <- (begin+1) to s.length) {
          if (s.isPossibleConstituent(begin, end)) {
            val maxLabel = margf.arr(begin, end).beliefs.argmax
            if(maxLabel < labelIndex.size) {
              result += Argument(labelIndex.get(maxLabel), Span(begin, end))
            }
          }
        }
        s.frames(fi).copy(args=result)
      }

      s.copy(frames=pieces)
    }

    def marginal(s: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val pieces =  for ((f, fi) <- s.frames.zipWithIndex) yield {
        val anchoring = new Anchoring(featurizer.anchor(s.validConstituents,  s, f.lemma, f.pos), labelIndex.size, weights, aug, fi)
        var partition = 0.0
        val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
          if (s.isPossibleConstituent(begin, end)) {
            val b = DenseVector.tabulate(numLabels+1)(l => math.exp(anchoring.score(begin, end, l)))
            val normalizer = breeze.linalg.sum(b)
            partition += math.log(normalizer)
            assert(normalizer >= 0.0)
            b /= normalizer
            aug.spanBeliefs(begin, end).frames(fi).copy(beliefs=b)
          } else {
            labelZeros
          }

        }
        new FrameMarginal(partition, arr, anchoring)

      }

      new Marginal(pieces)
    }

    private val labelZeros = {
      val z = DenseVector.zeros[Double](labelIndex.size + 1)
      z(labelIndex.size) = 1.0
      Beliefs(beliefsFactory.srlProp, z)
    }

    def goldMarginal(s: FeaturizedSentence, augment: SentenceBeliefs): Marginal = {
      val pieces = for ((f, fi) <- s.frames.zipWithIndex) yield {
        val anchoring = new Anchoring(featurizer.anchor(s.constituentSparsity.activeTriangularIndices, s, f.lemma, f.pos), labelIndex.size, weights, augment, fi)
        var partition = 0.0
        val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
          labelZeros
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

      new Marginal(pieces)
    }

    def countsFromMarginal(s: FeaturizedSentence, marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
      counts.loss += marg.logPartition * scale
      for ( f <- marg.frames) {
        val localization = f.anchoring.featurizer
        for ( begin <- 0 until s.length; end <- (begin+1) to s.length if s.isPossibleConstituent(begin, end); l <- 0 until labelIndex.size) {
          val score = f.arr(begin, end).beliefs(l)
          if (score != 0.0) {
            for (f <- localization.featuresFor(begin, end, l))
              counts.counts(f) += score * scale
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
            val newFrames: IndexedSeq[Beliefs[Option[String]]] = marg.frames.map(_.arr(b, e))
            val copy = spanBeliefs.copy(frames = newFrames)
            copy
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
    def anchor(validSpans: BitSet, fs: FeaturizedSentence, lemma: String, pos: Int):FeatureAnchoring
  }

  trait FeatureAnchoring {
    def featuresFor(begin: Int, end: Int, label: Int):Array[Int]
  }


  class Anchoring(val featurizer: FeatureAnchoring, notSRLLabel: Int, weights: DenseVector[Double],  sentenceBeliefs: SentenceBeliefs, frameIndex: Int) {
    def score(begin: Int, end: Int, label: Int):Double = {
      val init = sentenceBeliefs.spanBeliefs(begin, end).frames(frameIndex).beliefs(label)
      if (init <= 0.0) Double.NegativeInfinity
      else {
        val feats: Array[Int] = featurizer.featuresFor(begin, end, label)
        if (feats == null && label == notSRLLabel) {
          0.0
        } else {
          math.log(init) + dot(feats, weights)
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
      val labelIndex = factory.srlLabelIndex


      val featurizer = new StandardFeaturizer(factory.srlProp.index, lemmaIndex, processor.wordFeatureIndex, processor.spanFeatureIndex)
      val model = new Model(factory, labelIndex, featurizer, weights(_))

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


    def anchor(validSpans: BitSet, fs: FeaturizedSentence, lemma: String, pos: Int): FeatureAnchoring = {
      new Anchoring(validSpans, fs, lemma, pos)

    }

    class Anchoring(validSpans: BitSet, fs: FeaturizedSentence, lemma: String, pos: Int) extends FeatureAnchoring {
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
          if(!validSpans(TriangularArray.index(beg, end)) || beg == end || (pos < end && pos >= beg)) {
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
