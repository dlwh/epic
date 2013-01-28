package epic.everything.models

import epic.everything.{ProcessedDocument}
import epic.framework.{Marginal, ProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index}
import epic.sequences.{SegmentationEval, SemiCRF, SemiCRFInference, SemiCRFModel}
import epic.sequences.SemiCRF.Anchoring
import breeze.collection.mutable.TriangularArray
import breeze.linalg._
import epic.ontonotes.NERType

/**
 *
 * @author dlwh
 */
class ChainNERModel(beliefsFactory: DocumentBeliefs.Factory,
                    val inner: SemiCRFModel[NERType.Value, String]) extends DocumentAnnotatingModel {
  def featureIndex: Index[Feature] = inner.featureIndex
  type Inference = ChainNERInference
  type ExpectedCounts = inner.ExpectedCounts
  type Marginal = ChainNERMarginal[inner.Marginal]

  def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ChainNERInference(beliefsFactory, inner.inferenceFromWeights(weights), inner.labelIndex)

  def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    ecounts.toObjective
  }

  type EvaluationResult = SegmentationEval.Stats

  def evaluate(guess: ProcessedDocument, gold: ProcessedDocument): EvaluationResult = {
    val individuals = for( (gld, gss) <- guess.sentences.map(_.ner).zip(gold.sentences.map(_.ner))) yield {
      SegmentationEval.evaluateExample(Set(NERType.NotEntity, NERType.OutsideSentence), gold = gld, guess = gss)
    }

    individuals.reduceLeft(_ + _)
  }
}

case class ChainNERMarginal[Inner<:Marginal](sentences: IndexedSeq[Inner]) extends epic.framework.Marginal {
  val logPartition = sentences.map(_.logPartition).sum
}

case class ChainNERInference(beliefsFactory: DocumentBeliefs.Factory,
                             inner: SemiCRFInference[NERType.Value, String],
                             labels: Index[NERType.Value]) extends DocumentAnnotatingInference with ProjectableInference[ProcessedDocument, DocumentBeliefs] {


  type ExpectedCounts = inner.ExpectedCounts
  type Marginal = ChainNERMarginal[inner.Marginal]
  val notNER = labels(NERType.OutsideSentence)
  assert(notNER != -1)

  def emptyCounts = inner.emptyCounts

  def annotate(doc: ProcessedDocument, m: Marginal): ProcessedDocument = {
    val newSentences = for ( (s, sentMarg) <- doc.sentences zip m.sentences) yield {
      val decodedNer = inner.posteriorDecode(sentMarg)
      s.copy(ner=decodedNer)
    }

    doc.copy(newSentences)
  }



  // inference methods
  def goldMarginal(doc: ProcessedDocument, aug: DocumentBeliefs): Marginal = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.goldMarginal(s.ner, anchoring )
    }

    ChainNERMarginal(marginals)
  }


  def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    val counts = emptyCounts
    for(i <- 0 until doc.sentences.length) {
      counts += inner.countsFromMarginal(doc.sentences(i).ner, marg.sentences(i), accum, scale)
    }

    counts
  }


  def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
    beliefsFactory(doc)
  }

  def projectGold(doc: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(doc, m, oldAugment)
  }


  def project(doc: ProcessedDocument, m: Marginal, oldBeliefs: DocumentBeliefs): DocumentBeliefs = {
    val newSentences = Array.tabulate(doc.sentences.length) { s =>
      val marg = m.sentences(s)
      assert(!marg.logPartition.isInfinite, doc.sentences(s))
      val sentenceBeliefs = oldBeliefs.beliefsForSentence(s)
      val newSpans = TriangularArray.tabulate(doc.sentences(s).length+1){ (b,e) =>
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
            assert( (normalizer - 1.0).abs < 1E-3, copy.ner + " " + spanBeliefs.ner)
            copy.ner.beliefs /= normalizer
            copy
          }
        } else null
      }
      sentenceBeliefs.copy(spans=newSpans)
    }

    DocumentBeliefs(newSentences)
  }

  def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): Marginal = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.marginal(s.ner, anchoring )
    }

    ChainNERMarginal(marginals)
  }

  def beliefsToAnchoring(doc: ProcessedDocument, beliefs: DocumentBeliefs):IndexedSeq[SemiCRF.Anchoring[NERType.Value, String]] = {
    beliefs.sentences.zip(doc.sentences).map { case(b, s) =>
      // (same trick as in parser:)
      // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
      // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
      // but the dynamic program does not visit all spans for all parses, only those
      // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
      // and then we divide out p(not span) for spans in the tree.
      new Anchoring[NERType.Value, String] {
        private def passAssert(v: Double, pred: Double=>Boolean, stuff: Any*) = if(pred(v)) v else throw new AssertionError("Value " + v + ": other stuff:" + stuff.mkString(" "))
        def labelIndex: Index[NERType.Value] = labels

        def words: IndexedSeq[String] = s.words

        def maxSegmentLength(label: Int): Int = inner.maxLength(label)

        val normalizingPiece = b.spans.data.filter(_ ne null).map { b =>
          val notNerScore = b.ner(notNER)

          if(notNerScore < 1E-4) 0.0 else math.log(notNerScore)
        }.sum

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          val score = if(cur == notNER) Double.NegativeInfinity
          else if(b.spanBeliefs(beg, end).eq(null) || b.spanBeliefs(beg, end).ner(cur) == 0.0) Double.NegativeInfinity
          else if(b.spanBeliefs(beg, end).ner(notNER) < 1E-4) {
            math.log(b.spanBeliefs(beg,end).ner(cur))
          } else {
             passAssert(math.log(b.spanBeliefs(beg,end).ner(cur) / b.spanBeliefs(beg,end).ner(notNER)), {!_.isNaN}, b.spanBeliefs(beg,end).ner(cur), b.spanBeliefs(beg,end).ner(notNER))

          }

          if (beg == 0) score + normalizingPiece else score
        }

        def startSymbol = inner.startSymbol
      }
    }
  }
}

