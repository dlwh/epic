package epic.everything

import epic.framework._
import epic.ontonotes.NERType
import epic.sequences._
import breeze.linalg._
import breeze.util._
import breeze.collection.mutable.TriangularArray
import epic.sequences.SemiCRF.{TransitionVisitor, Anchoring}

/**
 *
 * @author dlwh
 */
object ChainNER {
  class Model(factory: SentenceBeliefs.Factory,
              val featurizer: IndexedFeaturizer,
              weights: Feature=>Option[Double] = { (f:Feature) => None}) extends EvaluableModel[FeaturizedSentence] {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = SemiCRF.Marginal[NERType.Value, String]
    type Inference = ChainNER.Inference
    type EvaluationResult = SegmentationEval.Stats
    def featureIndex = featurizer.featureIndex

    def initialValueForFeature(f: Feature): Double = weights(f).getOrElse(0.0)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
      new ChainNER.Inference(factory, weights, featurizer)
    }

    def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.toObjective
    }


    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence, logResults: Boolean): EvaluationResult = {
      val stats = SegmentationEval.evaluateExample(Set(NERType.NotEntity, NERType.OutsideSentence), gold = gold.ner, guess = guess.ner)
      if (logResults) println("Guess:\n" + guess.ner.render(badLabel=NERType.NotEntity) + "\n Gold:\n" + gold.ner.render(badLabel=NERType.NotEntity)+ "\n" + stats)
      stats
    }
  }

  case class Inference(beliefsFactory: SentenceBeliefs.Factory,
                       weights: DenseVector[Double],
                       feat: IndexedFeaturizer) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {

    val labels: Index[NERType.Value] = feat.labelIndex

    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = SemiCRF.Marginal[NERType.Value, String]
    private val notNER = labels(NERType.OutsideSentence)
    assert(notNER != -1)

    def emptyCounts = StandardExpectedCounts.zero(feat.featureIndex)

    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val decodedNer = SemiCRF.posteriorDecode(m)
      sent.copy(nerOpt=Some(decodedNer))
    }

    // inference methods
    def goldMarginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      SemiCRF.Marginal.goldMarginal(feat.makeAnchoring(sent, weights, aug), sent.ner.segments)
    }

    def marginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      SemiCRF.Marginal(feat.makeAnchoring(sent, weights, aug))
    }


    def countsFromMarginal(sent: FeaturizedSentence, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      accum.loss += scale * marg.logPartition
      marg.anchoring.asInstanceOf[feat.Anchoring].updateCounts(marg, accum.counts, scale)
      accum
    }


    def baseAugment(sent: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(sent)
    }

    def project(sent: FeaturizedSentence, marg: Marginal, sentenceBeliefs: SentenceBeliefs): SentenceBeliefs = {
      assert(!marg.logPartition.isInfinite, "Infinite partition! " + sent)
      val newSpans = TriangularArray.tabulate(sent.length+1){ (b,e) =>
        if (b < e) {
          val spanBeliefs = sentenceBeliefs.spanBeliefs(b, e)
          if (spanBeliefs eq null) {
            null
          } else {
            val copy = spanBeliefs.copy(ner = spanBeliefs.ner.updated(DenseVector.tabulate(labels.size){marg.spanMarginal(_, b, e)}))
            assert(copy.ner.beliefs(notNER) == 0.0, copy.ner.beliefs)
            // dealing with some stupid rounding issues...
            if (spanBeliefs.ner.beliefs(notNER) == 0.0) {
              copy.ner.beliefs(notNER) = 0.0
            } else {
              copy.ner.beliefs(notNER) = 1 - sum(copy.ner.beliefs)
//              if (copy.ner.beliefs(notNER) < 1E-6) {
//                assert(copy.ner.beliefs(notNER) > -1E-3, "copy.ner.beliefs(notNER) should have been bigger than " + copy.ner.beliefs(notNER) + " " + copy.ner.beliefs + " " + spanBeliefs.ner)
//                copy.ner.beliefs(notNER) = 0.0
//              }
            }
            val normalizer: Double = sum(copy.ner.beliefs)
            // make sure it's close to normalized already...
//            assert( (normalizer - 1.0).abs < 1E-3, s"NER not normalized! new: ${copy.ner} old: ${spanBeliefs.ner}")
            copy.ner.beliefs /= normalizer
            copy
          }
        } else null
      }
      sentenceBeliefs.copy(spans=newSpans)
    }



  }

  class ModelFactory(beliefsFactory: SentenceBeliefs.Factory,
                     processor: FeaturizedDocument.Factory,
                     gazetteer: Gazetteer[Any, String] = Gazetteer.empty[String, String],
                     weights: Feature=>Option[Double] = { (f:Feature) => None}) {
    def makeModel(sentences: IndexedSeq[FeaturizedSentence]):Model = {
      val train = sentences.map(_.ner)
      val maxLengthMap = train.flatMap(_.segments.iterator).groupBy(_._1).mapValues(arr => arr.map(_._2.length).max)
      val labelIndex = beliefsFactory.nerLabelIndex
      val maxLengthArray = Encoder.fromIndex(labelIndex).tabulateArray(maxLengthMap.getOrElse(_, 0))
      println(maxLengthMap)

      val gazetteerFeatures = Index[Feature]()

      val maxMaxLength = (0 until labelIndex.size).map(maxLengthArray).max
//      var i = 0
//      for(s <- sentences) {
//
//        TODO: gazetteer
//        for(b <- 0 until s.length; e <- (b+1) until math.min(s.length,b+maxMaxLength) if s.isPossibleSpan(b, e)) {
//
//
//        }
//        i += 1
//      }


      val featurizer = new IndexedFeaturizer(maxLengthArray, labelIndex, processor.wordFeatureIndex, processor.spanFeatureIndex)



      new Model(beliefsFactory, featurizer, weights)
    }

  }

  case class Label1Feature[L](label: L, f: Feature, kind: Symbol) extends Feature
  case class TransitionFeature[L](label: L, label2: L) extends Feature

  case class IndexedFeaturizer(maxLength: Array[Int], labelIndex: Index[NERType.Value], baseWordFeatureIndex: Index[Feature], baseSpanFeatureIndex: Index[Feature]) {

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

    class Anchoring(fs: FeaturizedSentence, weights: DenseVector[Double], messages: SentenceBeliefs) extends SemiCRF.Anchoring[NERType.Value, String] {
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
      val endCache = Array.tabulate(labelIndex.size, length){ (l, w) =>
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

      val normalizingPiece = messages.spans.data.filter(_ ne null).map { b =>
        val notNerScore = b.ner(notNER)

        if (notNerScore <= 0.0) 0.0 else math.log(notNerScore)
      }.sum

      private def messagePiece(prev: Int, cur: Int, beg: Int, end: Int): Double = {
        val score = if (cur == notNER) Double.NegativeInfinity
        else if (messages.spanBeliefs(beg, end).eq(null) || messages.spanBeliefs(beg, end).ner(cur) == 0.0) Double.NegativeInfinity
        else if (messages.spanBeliefs(beg, end).ner(notNER) <= 0.0) {
          math.log(messages.spanBeliefs(beg,end).ner(cur))
        } else {
          math.log(messages.spanBeliefs(beg,end).ner(cur) / messages.spanBeliefs(beg,end).ner(notNER))
        }

        if (beg == 0) score + normalizingPiece else score
      }


      def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
        var score = messagePiece(prev, cur, beg, end) + spanCache(beg, end)(cur)
        if (score == Double.NegativeInfinity) {
          score
        } else {
          score += beginCache(cur)(beg)
          score += endCache(cur)(end-1)
          var pos = beg+1
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
              counts(featureMap(vector(i))) += d * scale
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

}
