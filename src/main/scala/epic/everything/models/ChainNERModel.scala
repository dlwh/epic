package epic.everything.models

import breeze.linalg.DenseVector
import epic.everything.{DSpan, ProcessedDocument, NERType, DocumentAnnotator}
import epic.framework.{FullProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index}
import epic.sequences.{SemiCRF, SemiCRFInference, SemiCRFModel}
import epic.sequences.SemiCRF.Anchoring
import breeze.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
class ChainNERModel(val inner: SemiCRFModel[NERType.Value, String]) extends DocumentAnnotatingModel {
  def featureIndex: Index[Feature] = inner.featureIndex
  type ExpectedCounts = inner.ExpectedCounts

  def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ChainNERInference(inner.inferenceFromWeights(weights), inner.labelIndex)

  def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    ecounts.toObjective
  }

  type Inference = ChainNERInference
}

case class ChainNERInference(inner: SemiCRFInference[NERType.Value, String],
                             labels: Index[NERType.Value]) extends DocumentAnnotatingInference with FullProjectableInference[ProcessedDocument, DocumentBeliefs] {
  type ExpectedCounts = inner.ExpectedCounts
  case class Marginal(sentences: IndexedSeq[inner.Marginal], partition: Double)

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
  def goldMarginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.goldMarginal(s.ner, anchoring )
    }

    val partition = marginals.map(_.logPartition).sum
    Marginal(marginals, partition) -> partition
  }


  def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, aug: DocumentBeliefs): ExpectedCounts = {
    val counts = emptyCounts
    val anchorings = beliefsToAnchoring(doc, aug)
    for(i <- 0 until doc.sentences.length) {
      counts += inner.countsFromMarginal(doc.sentences(i).ner, marg.sentences(i), anchorings(i))
    }

    counts
  }



  def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
    sys.error("TODO")
  }


  def projectGold(doc: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(doc, m, oldAugment)
  }


  def project(doc: ProcessedDocument, m: Marginal, oldBeliefs: DocumentBeliefs): DocumentBeliefs = {
    val newSentences = Array.tabulate(doc.sentences.length) { s =>
      val marg = m.sentences(s)
      val sentenceBeliefs = oldBeliefs.beliefsForSentence(s)
      val newSpans = TriangularArray.tabulate(doc.sentences(s).length){ (b,e) =>
        val spanBeliefs = sentenceBeliefs.spanBeliefs(b, e)
        spanBeliefs.copy(ner = spanBeliefs.ner.copy(beliefs=DenseVector.tabulate(labels.size){marg.spanMarginal(_, b, e)}))
      }
      sentenceBeliefs.copy(newSpans)
    }

    DocumentBeliefs(newSentences)
  }

  def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.marginal(s.ner, anchoring )
    }

    val partition = marginals.map(_._2).sum
    Marginal(marginals.map(_._1), partition) -> partition
  }

  def beliefsToAnchoring(doc: ProcessedDocument, beliefs: DocumentBeliefs):IndexedSeq[SemiCRF.Anchoring[NERType.Value, String]] = {
    beliefs.sentenceBeliefs.zip(doc.sentences).map { case(b, s) =>
      new Anchoring[NERType.Value, String] {
        def labelIndex: Index[NERType.Value] = labels

        def w: IndexedSeq[String] = s.words

        def maxSegmentLength(label: Int): Int = inner.maxLength(label)

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          math.log(b.spanBeliefs(cur,beg).ner(cur)) - math.log1p(-b.spanBeliefs(cur,beg).ner(cur))
        }

        def startSymbol = inner.startSymbol
      }
    }
  }
}

