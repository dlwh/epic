package epic.redux

import epic.framework._
import epic.ontonotes.NERType
import epic.sequences.{SemiCRF, SemiCRFInference, SegmentationEval, SemiCRFModel}
import epic.everything.models.SentenceBeliefs
import breeze.linalg._
import breeze.util.Index
import breeze.collection.mutable.TriangularArray
import epic.sequences.SemiCRF.Anchoring

/**
 *
 * @author dlwh
 */
object ChainNER {
  class Model(factory: SentenceBeliefs.Factory,
              val inner: SemiCRFModel[NERType.Value, String]) extends EvaluableModel[FeaturizedSentence] {
    type ExpectedCounts = inner.ExpectedCounts
    type Marginal = inner.Marginal
    type Inference = ChainNER.Inference
    type EvaluationResult = SegmentationEval.Stats

    def featureIndex: Index[Feature] = inner.featureIndex

    def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ChainNER.Inference(factory, inner.inferenceFromWeights(weights), inner.labelIndex)

    def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.toObjective
    }


    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence): EvaluationResult = {
      SegmentationEval.evaluateExample(Set(NERType.NotEntity, NERType.OutsideSentence), gold = gold.ner, guess = guess.ner)
    }
  }

  case class Inference(beliefsFactory: SentenceBeliefs.Factory,
                       inner: SemiCRFInference[NERType.Value, String],
                       labels: Index[NERType.Value]) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {


    type ExpectedCounts = inner.ExpectedCounts
    type Marginal = inner.Marginal
    private val notNER = labels(NERType.OutsideSentence)
    assert(notNER != -1)

    def emptyCounts = inner.emptyCounts

    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val decodedNer = inner.posteriorDecode(m)
      sent.copy(nerOpt=Some(decodedNer))
    }



    // inference methods
    def goldMarginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val anchoring = beliefsToAnchoring(sent, aug)
      inner.goldMarginal(sent.ner, anchoring)
    }


    def countsFromMarginal(sent: FeaturizedSentence, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      inner.countsFromMarginal(sent.ner, marg, accum, scale)
    }


    def baseAugment(sent: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(sent)
    }

    def projectGold(sent: FeaturizedSentence, m: Marginal, oldAugment: SentenceBeliefs): SentenceBeliefs = {
      project(sent, m, oldAugment)
    }


    def project(sent: FeaturizedSentence, marg: Marginal, sentenceBeliefs: SentenceBeliefs): SentenceBeliefs = {
      assert(!marg.logPartition.isInfinite, "Infinite partition! " + sent)
      val newSpans = TriangularArray.tabulate(sent.length+1){ (b,e) =>
        if(b < e) {
          val spanBeliefs = sentenceBeliefs.spanBeliefs(b, e)
          if(spanBeliefs eq null) {
            null
          } else {
            val copy = spanBeliefs.copy(ner = spanBeliefs.ner.updated(DenseVector.tabulate(labels.size){marg.spanMarginal(_, b, e)}))
            assert(copy.ner.beliefs(notNER) == 0.0, copy.ner.beliefs)
            // dealing with some stupid rounding issues...
            if (spanBeliefs.ner.beliefs(notNER) == 0.0) {
              copy.ner.beliefs(notNER) = 0.0
            } else {
              copy.ner.beliefs(notNER) = 1 - sum(copy.ner.beliefs)
              if(copy.ner.beliefs(notNER) < 1E-6) {
                assert(copy.ner.beliefs(notNER) > -1E-6)
                copy.ner.beliefs(notNER) = 0.0
              }
            }
            val normalizer: Double = sum(copy.ner.beliefs)
            // make sure it's close to normalized already...
            assert( (normalizer - 1.0).abs < 1E-3, s"NER not normalized! new: ${copy.ner} old: ${spanBeliefs.ner}")
            copy.ner.beliefs /= normalizer
            copy
          }
        } else null
      }
      sentenceBeliefs.copy(spans=newSpans)
    }

    def marginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val anchoring = beliefsToAnchoring(sent, aug)
      inner.marginal(sent.ner, anchoring )
    }

    def beliefsToAnchoring(sent: FeaturizedSentence, beliefs: SentenceBeliefs):SemiCRF.Anchoring[NERType.Value, String] = {
      // (same trick as in parser:)
      // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
      // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
      // but the dynamic program does not visit all spans for all parses, only those
      // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
      // and then we divide out p(not span) for spans in the tree.
      new Anchoring[NERType.Value, String] {
        private def passAssert(v: Double, pred: Double=>Boolean, stuff: Any*) = if(pred(v)) v else throw new AssertionError(s"Value $v: other stuff: ${stuff.mkString(" ")}")
        def labelIndex: Index[NERType.Value] = labels

        def words: IndexedSeq[String] = sent.words

        def maxSegmentLength(label: Int): Int = inner.maxLength(label)

        val normalizingPiece = beliefs.spans.data.filter(_ ne null).map { b =>
          val notNerScore = b.ner(notNER)

          if(notNerScore < 1E-4) 0.0 else math.log(notNerScore)
        }.sum

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          val score = if(cur == notNER) Double.NegativeInfinity
          else if (beliefs.spanBeliefs(beg, end).eq(null) || beliefs.spanBeliefs(beg, end).ner(cur) == 0.0) Double.NegativeInfinity
          else if (beliefs.spanBeliefs(beg, end).ner(notNER) < 1E-4) {
            math.log(beliefs.spanBeliefs(beg,end).ner(cur))
          } else {
            passAssert(math.log(beliefs.spanBeliefs(beg,end).ner(cur) / beliefs.spanBeliefs(beg,end).ner(notNER)), {!_.isNaN}, beliefs.spanBeliefs(beg,end).ner(cur), beliefs.spanBeliefs(beg,end).ner(notNER))

          }

          if (beg == 0) score + normalizingPiece else score
        }

        def startSymbol = inner.startSymbol
      }
    }
  }

}
