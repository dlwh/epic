package epic.everything.models

import epic.everything.{DSpan, ProcessedDocument, NERType, DocumentAnnotator}
import epic.framework.{Marginal, ProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index}
import epic.sequences.{SemiCRF, SemiCRFInference, SemiCRFModel}
import epic.sequences.SemiCRF.Anchoring
import breeze.collection.mutable.TriangularArray
import breeze.linalg._

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

  // annotation methods
  def apply(doc: ProcessedDocument, beliefs: DocumentBeliefs): ProcessedDocument = {
    val anchorings = beliefsToAnchoring(doc, beliefs)
    val newSentences = for( (s,i) <- doc.sentences.zipWithIndex) yield {
      val segmentation = inner.viterbi(s.words, anchorings(i))
      s.copy(ner=segmentation)
    }

    doc.copy(sentences=newSentences)
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
      assert(!counts.counts.valuesIterator.exists(_.isNaN), doc.sentences(i).words + " " + i)
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
            copy.ner.beliefs(notNER) = 1 - sum(copy.ner.beliefs)
            if(copy.ner.beliefs(notNER) < 1E-6) {
              assert(copy.ner.beliefs(notNER) > -1E-6)
              copy.ner.beliefs(notNER) = 0.0
            }
            assert( (sum(copy.ner.beliefs) - 1.0).abs < 1E-4, copy.ner + " " + spanBeliefs.ner)
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
      new Anchoring[NERType.Value, String] {
        private def passAssert(v: Double, pred: Double=>Boolean, stuff: Any*) = if(pred(v)) v else throw new AssertionError("Value " + v + ": other stuff:" + stuff.mkString(" "))
        def labelIndex: Index[NERType.Value] = labels

        def words: IndexedSeq[String] = s.words

        def maxSegmentLength(label: Int): Int = inner.maxLength(label)

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          if(cur == notNER) Double.NegativeInfinity
          else if(b.spanBeliefs(beg, end).eq(null) || b.spanBeliefs(beg, end).ner(cur) == 0.0) Double.NegativeInfinity
          else if(b.spanBeliefs(beg, end).ner(notNER) == 0.0) {
            math.log(b.spanBeliefs(beg,end).ner(cur))
          } else {
             passAssert(math.log(b.spanBeliefs(beg,end).ner(cur) / b.spanBeliefs(beg,end).ner(notNER)), {!_.isNaN}, b.spanBeliefs(beg,end).ner(cur), b.spanBeliefs(beg,end).ner(notNER))

          }
        }

        def startSymbol = inner.startSymbol
      }
    }
  }
}

