package epic.everything

import breeze.util.Index
import epic.framework._
import breeze.linalg.{Counter, SparseVector, DenseVector}
import breeze.collection.mutable.TriangularArray
import collection.immutable.BitSet

/**
 *
 * @author dlwh
 */
object SRL {
  /*
  class Model(factory: SentenceBeliefs.Factory,
              val labelIndex: Index[String],
              val featurizer: IndexedFeaturizer,
              initialWeights: Feature=>Double = {(_: Feature) => 0.0}) extends EvaluableModel[FeaturizedSentence] with StandardExpectedCounts.Model with Serializable {

    //  def extractCRF(weights: DenseVector[Double]) = {
    //    val grammar = inferenceFromWeights(weights)
    //    new SemiCRF(grammar)
    //  }

    def featureIndex = featurizer.featureIndex

    type Marginal = SRL.Marginal
    type Inference = SRL.Inference

    def initialValueForFeature(f: Feature): Double = initialWeights(f)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new SRL.Inference(factory, labelIndex, weights, featurizer)
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

    def marginal(s: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val pieces =  for ((f, fi) <- s.frames.zipWithIndex) yield {
        val anchoring = new Anchoring(featurizer.anchor(s.validConstituents,  s.words, f.lemma, f.pos), weights, aug, fi)
        var partition = 0.0
        val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
          if (s.isPossibleConstituent(begin, end)) {
            val b = DenseVector.tabulate(numLabels)(l => math.exp(anchoring.score(begin, end, l)))
            partition += math.log(breeze.linalg.sum(b))
            aug.spanBeliefs(begin, end).frames(fi).copy(beliefs=b)
          } else {
            null
          }

        }
        new FrameMarginal(partition, arr, anchoring)

      }

      new Marginal(pieces)
    }

    private val labelZeros = {
      val z = DenseVector.zeros[Double](labelIndex.size + 1)
      z(labelIndex.size) = 1.0
      z
    }

    def goldMarginal(s: FeaturizedSentence, augment: SentenceBeliefs): Marginal = {
      val pieces = for ((f, fi) <- s.frames.zipWithIndex) yield {
        val anchoring = new Anchoring(featurizer.anchor(s.constituentSparsity.activeTriangularIndices,  s.words, f.lemma, f.pos), weights, augment, fi)
        var partition = 0.0
        val arr = TriangularArray.tabulate(s.length+1) {(begin, end) =>
          if (s.isPossibleConstituent(begin, end)) {
            augment.spanBeliefs(begin, end).frames(fi).copy(beliefs=labelZeros)
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

      new Marginal(pieces)
    }

    def countsFromMarginal(s: FeaturizedSentence, marg: Marginal, counts: ExpectedCounts, scale: Double): ExpectedCounts = {
      counts.loss += marg.logPartition * scale
      for ( f <- marg.frames) {
        val localization = f.anchoring.featurizer
        for ( begin <- 0 until s.length; end <- (begin+1) to s.length if s.isPossibleConstituent(begin, end); l <- 0 to labelIndex.size) {
          val score = f.arr(begin, end).beliefs(l)
          if (score != 0.0)
            breeze.linalg.axpy(score * scale, localization.featuresFor(begin, end, l), counts.counts)
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
        } else null
      }
      sentenceBeliefs.copy(spans=newSpans)
    }

  }

  trait IndexedFeaturizer {
    def featureIndex: Index[Feature]
    def anchor(validSpans: BitSet, words: IndexedSeq[String], lemma: String, pos: Int):FeatureAnchoring
  }

  trait FeatureAnchoring {
    def featuresFor(begin: Int, end: Int, label: Int):Array[Int]
  }


  class Anchoring(val featurizer: FeatureAnchoring, weights: DenseVector[Double],  sentenceBeliefs: SentenceBeliefs, frameIndex: Int) {
    def score(begin: Int, end: Int, label: Int):Double = {
      val init = sentenceBeliefs.spanBeliefs(begin, end).frames(frameIndex).beliefs(label)
      if (init <= 0.0) Double.NegativeInfinity
      else math.log(init) + dot(featurizer.featuresFor(begin, end, label), weights)
    }

    private def dot(features: Array[Int], weights: DenseVector[Double]) = {
      var i =0
      var score = 0.0
      while(i < features.length) {
        score += weights(features(i))
        i += 1
      }
      score
    }
  }

  class ModelFactory(factory: SentenceBeliefs.Factory,
                  processor: FeaturizedDocument.Factory,
                  weights: Feature=>Double = { (f:Feature) => 0.0}) {

    val distanceBins: Int = 10

    def makeModel(sents: IndexedSeq[FeaturizedSentence]) = {
      val frames = sents.flatMap(_.frames)
      val lemmaIndex = Index(frames.iterator.map(_.lemma))
      val labelIndex = factory.srlLabelIndex


      val featurizer = new IndexedFeaturizer(labelIndex, lemmaIndex, processor.wordFeatureIndex, processor.spanFeatureIndex)
      val model = new Model(factory, labelIndex, indexed, weights(_))

      model
    }
  }

  case class IndexedFeaturizer(labelIndex: Index[String], lemmaIndex: Index[String], baseWordFeatureIndex: Index[Feature], baseSpanFeatureIndex: Index[Feature]) {

    val kinds = Array('Begin, 'Interior, 'End)

    val (featureIndex: Index[Feature], wordFeatures, spanFeatures, transitionFeatures) = {
      val featureIndex = Index[Feature]()
      val labelFeatures = Array.tabulate(labelIndex.size, kinds.length, baseWordFeatureIndex.size) { (l, k, f) =>
        featureIndex.index(Label1Feature(labelIndex.get(l), baseWordFeatureIndex.get(f), kinds(k)))
      }

      val spanFeatures = Array.tabulate(labelIndex.size, baseSpanFeatureIndex.size) { (l, f) =>
        featureIndex.index(Label1Feature(labelIndex.get(l), baseSpanFeatureIndex.get(f), 'Span))
      }

      val transitionFeatures = Array.tabulate(labelIndex.size, labelIndex.size) { (l1, l2) =>
        featureIndex.index(TransitionFeature(labelIndex.get(l1), labelIndex.get(l2)))
      }

      (featureIndex, labelFeatures, spanFeatures, transitionFeatures)
    }


    private val notNER = labelIndex(NERType.OutsideSentence)
    assert(notNER != -1)

    def makeAnchoring(fs: FeaturizedSentence, weights: DenseVector[Double], beliefs: SentenceBeliefs) = new Anchoring(fs, weights, beliefs)

    class Anchoring(fs: FeaturizedSentence, weights: DenseVector[Double], beliefs: SentenceBeliefs) extends SRL.Anchoring {
      def labelIndex: Index[NERType.Value] = IndexedFeaturizer.this.labelIndex

      def startSymbol = NERType.OutsideSentence

      def words: IndexedSeq[String] = fs.words

      def maxSegmentLength(label: Int): Int = maxLength(label)

      def dot(weights: DenseVector[Double], features: Array[Int], featureMap: Array[Int]) = {
        var i = 0
        var score = 0.0
        while(i < features.length) {
          score += weights(featureMap(features(i)))
          i += 1
        }
        score
      }

      val beginCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
        val f = fs.featuresForWord(w)
        if (f eq null) Double.NegativeInfinity
        else dot(weights, f, wordFeatures(l)(0))
      }
      val wordCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
        val f = fs.featuresForWord(w)
        if (f eq null) Double.NegativeInfinity
        else dot(weights, f, wordFeatures(l)(1))
      }
      val endCache = Array.tabulate(labelIndex.size, length+1){ (l, w) =>
        val f = fs.featuresForWord(w)
        if (f eq null) Double.NegativeInfinity
        else dot(weights, f, wordFeatures(l)(2))
      }

      val spanCache = TriangularArray.tabulate(length+1){ (b, e) =>
        val f = fs.featuresForSpan(b,e)
        Array.tabulate(labelIndex.size){ l =>
          if (f eq null) Double.NegativeInfinity
          else dot(weights, f, spanFeatures(l))
        }
      }

      // (same trick as in parser:)
      // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
      // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
      // but the dynamic program does not visit all spans for all parses, only those
      // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
      // and then we divide out p(not span) for spans in the tree.

      val normalizingPiece = beliefs.spans.data.filter(_ ne null).map { b =>
        val notNerScore = b.ner(notNER)

        if (notNerScore < 1E-4) 0.0 else math.log(notNerScore)
      }.sum

      private def beliefPiece(prev: Int, cur: Int, beg: Int, end: Int): Double = {
        val score = if (cur == notNER) Double.NegativeInfinity
        else if (beliefs.spanBeliefs(beg, end).eq(null) || beliefs.spanBeliefs(beg, end).ner(cur) == 0.0) Double.NegativeInfinity
        else if (beliefs.spanBeliefs(beg, end).ner(notNER) < 1E-4) {
          math.log(beliefs.spanBeliefs(beg,end).ner(cur))
        } else {
          math.log(beliefs.spanBeliefs(beg,end).ner(cur) / beliefs.spanBeliefs(beg,end).ner(notNER))
        }

        if (beg == 0) score + normalizingPiece else score
      }


      def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
        var score = beliefPiece(prev, cur, beg, end) + spanCache(beg, end)(cur)
        if (score == Double.NegativeInfinity) {
          score
        } else {
          score += beginCache(cur)(beg)
          score += endCache(cur)(end-1)
          var pos = beg
          while(pos < end) {
            score += wordCache(cur)(pos)
            pos += 1
          }
          score += weights(transitionFeatures(prev)(cur))
          score
        }
      }

      def updateCounts(m: SemiCRF.Marginal[NERType.Value, String], counts: DenseVector[Double], scale: Double) {
        m visit new TransitionVisitor[NERType.Value, String] {

          def daxpy(d: Double, vector: Array[Int], featureMap: Array[Int], counts: DenseVector[Double]) {
            var i = 0
            while(i < vector.length) {
              counts(vector(i)) += d * scale
              i += 1
            }

          }

          def apply(prev: Int, cur: Int, start: Int, end: Int, count: Double) {
            daxpy(count, fs.featuresForWord(start), wordFeatures(cur)(0), counts)
            daxpy(count, fs.featuresForWord(end-1), wordFeatures(cur)(2), counts)
            var p = start+1
            while(p < end) {
              daxpy(count, fs.featuresForWord(p), wordFeatures(cur)(1), counts)
              p += 1
            }

            daxpy(count, fs.featuresForSpan(start, end), spanFeatures(cur), counts)
            counts(transitionFeatures(prev)(cur)) += count * scale
          }
        }

      }
    }
  }

*/
}
