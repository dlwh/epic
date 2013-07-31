package epic.everything

import epic.framework._
import epic.ontonotes.NERType
import epic.sequences._
import breeze.linalg._
import breeze.util._
import breeze.collection.mutable.TriangularArray
import epic.sequences.SemiCRF.{TransitionVisitor, Anchoring}
import epic.constraints.LabeledSpanConstraints
import epic.sequences.SegmentationModelFactory.IndexedStandardFeaturizer

/**
 *
 * @author dlwh
 */
object ChainNER {
  class Model(factory: SentenceBeliefs.Factory,
              val baseModel: SemiCRFModel[NERType.Value, String],
              maxMarginal: Boolean,
              weights: Feature=>Option[Double] = { (f:Feature) => None}) extends EvaluableModel[FeaturizedSentence] {
    type ExpectedCounts = baseModel.ExpectedCounts
    type Marginal = SemiCRF.Marginal[NERType.Value, String]
    type Inference = ChainNER.Inference
    type EvaluationResult = SegmentationEval.Stats
    def featureIndex = baseModel.featureIndex


    def accumulateCounts(d: FeaturizedSentence, m: Marginal, accum: ExpectedCounts, scale: Double) {
      baseModel.accumulateCounts(d.ner, m, accum, scale)

    }

    def initialValueForFeature(f: Feature): Double = weights(f).getOrElse(0.0)

    def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
      new ChainNER.Inference(factory, weights, baseModel.inferenceFromWeights(weights), maxMarginal)
    }

    def emptyCounts: ExpectedCounts = baseModel.emptyCounts

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
                       baseInference: SemiCRFInference[NERType.Value, String],
                       maxMarginal: Boolean) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {

    val labels: Index[NERType.Value] = baseInference.labelIndex

    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = SemiCRF.Marginal[NERType.Value, String]
    private val notNER = labels(NERType.OutsideSentence)
    assert(notNER != -1)


    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val decodedNer = SemiCRF.posteriorDecode(m)
      sent.copy(nerOpt=Some(decodedNer))
    }

    // inference methods
    def goldMarginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      baseInference.goldMarginal(sent.ner, new BeliefsComponentAnchoring(sent.words, sent.nerConstraints, labels, aug))
    }

    def marginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      if(maxMarginal) {
        SemiCRF.Marginal.maxDerivationMarginal(baseInference.anchor(sent.words, new BeliefsComponentAnchoring(sent.words, sent.nerConstraints, labels, aug)))
      } else {
        baseInference.marginal(sent.ner)
      }
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
            copy.ner.beliefs(notNER) = 0.0
//            assert(copy.ner.beliefs(notNER) == 0.0, copy.ner.beliefs)
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
                     weights: Feature=>Option[Double] = { (f:Feature) => None},
                     maxMarginals: Boolean = false) {
    def makeModel(sentences: IndexedSeq[FeaturizedSentence]):Model = {
      val train = sentences.map(_.ner)
      val labelIndex = beliefsFactory.nerLabelIndex

      val featurizer = IndexedStandardFeaturizer.make(processor.featurizer,
        NERType.OutsideSentence,
        labelIndex,
        processor.nerConstrainer)(train)

      val model = new SemiCRFModel(featurizer, processor.nerConstrainer, weights(_).getOrElse(0.0))

      new Model(beliefsFactory, model, maxMarginals, weights)
    }

  }

  class BeliefsComponentAnchoring(val words: IndexedSeq[String],
                                  val constraints: LabeledSpanConstraints[NERType.Value],
                                  val labelIndex: Index[NERType.Value],
                                  messages: SentenceBeliefs) extends SemiCRF.Anchoring[NERType.Value, String] {
    def startSymbol: NERType.Value = NERType.OutsideSentence
    private val notNER = labelIndex(NERType.OutsideSentence)


    private val normalizingPiece = messages.spans.data.filter(_ ne null).map { b =>
      val notNerScore = b.ner(notNER)

      if (notNerScore <= 0.0) 0.0 else math.log(notNerScore)
    }.sum

    def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int): Double = {
      // (same trick as in parser:)
      // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
      // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
      // but the dynamic program does not visit all spans for all parses, only those
      // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
      // and then we divide out p(not span) for spans in the tree.
      val score = if (cur == notNER) Double.NegativeInfinity
      else if (messages.spanBeliefs(begin, end).eq(null) || messages.spanBeliefs(begin, end).ner(cur) == 0.0) Double.NegativeInfinity
      else if (messages.spanBeliefs(begin, end).ner(notNER) <= 0.0) {
        math.log(messages.spanBeliefs(begin,end).ner(cur))
      } else {
        math.log(messages.spanBeliefs(begin,end).ner(cur) / messages.spanBeliefs(begin,end).ner(notNER))
      }

      if (begin == 0) score + normalizingPiece else score
    }
  }


}
