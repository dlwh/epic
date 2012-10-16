package epic.everything.models

import breeze.linalg.DenseVector
import epic.everything.{DSpan, Document, NERType, DocumentAnnotator}
import epic.framework.{FullProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index}
import epic.sequences.{SemiCRF, SemiCRFInference, SemiCRFModel}
import epic.sequences.SemiCRF.Anchoring
import breeze.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
class ChainNERModel(val inner: SemiCRFModel[NERType.Value, String],
                    val lens: DocumentBeliefs.Lens) extends DocumentAnnotatingModel {
  def featureIndex: Index[Feature] = inner.featureIndex
  type ExpectedCounts = inner.ExpectedCounts

  def initialValueForFeature(f: Feature): Double = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = new ChainNERInference(inner.inferenceFromWeights(weights), inner.labelIndex, lens)

  def emptyCounts: ExpectedCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    ecounts.toObjective
  }

  type Inference = ChainNERInference
}

case class ChainNERInference(inner: SemiCRFInference[NERType.Value, String],
                             labels: Index[NERType.Value],
                             lens: DocumentBeliefs.Lens) extends DocumentAnnotatingInference with FullProjectableInference[Document, DocumentBeliefs] {
  type ExpectedCounts = inner.ExpectedCounts
  case class Marginal(sentences: IndexedSeq[inner.Marginal], partition: Double)

  def emptyCounts = inner.emptyCounts

  // annotation methods
  def apply(doc: Document, beliefs: DocumentBeliefs): Document = {
    val anchorings = beliefsToAnchoring(doc, beliefs)
    val newSentences = for( (s,i) <- doc.sentences.zipWithIndex) yield {
      val segmentation = inner.viterbi(s.words, anchorings(i))
      val ner =for( (l,span) <- segmentation.segments) yield DSpan(doc.id, i, span.start, span.end) -> l

      s.annotate(ner=ner.toMap)
    }

    doc.copy(sentences=newSentences)
  }

  // inference methods
  def goldMarginal(doc: Document, aug: DocumentBeliefs): (Marginal, Double) = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.goldMarginal(s.nerSegmentation, anchoring )
    }

    val partition = marginals.map(_.logPartition).sum
    Marginal(marginals, partition) -> partition
  }


  def countsFromMarginal(doc: Document, marg: Marginal, aug: DocumentBeliefs): ExpectedCounts = {
    val counts = emptyCounts
    val anchorings = beliefsToAnchoring(doc, aug)
    for(i <- 0 until doc.sentences.length) {
      counts += inner.countsFromMarginal(doc.sentences(i).nerSegmentation, marg.sentences(i), anchorings(i))
    }

    counts
  }



  def baseAugment(doc: Document): DocumentBeliefs = {
    lens.initialFullBeliefs(doc)
  }


  def projectGold(doc: Document, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(doc, m, oldAugment)
  }


  def project(doc: Document, m: Marginal, oldBeliefs: DocumentBeliefs): DocumentBeliefs = {
    val newSentences = for(i <- 0 until doc.sentences.length) yield {
      val marg = m.sentences(i)
      val newSpans = TriangularArray.tabulate(doc.sentences(i).length){ (b,e) =>
        new PropertyBeliefs(Array(Array.tabulate(labels.size){marg.spanMarginal(_, b, e)}))
      }
      new SentenceBeliefs(newSpans,Array())
    }

    lens.recombine(oldBeliefs, lens.slice(oldBeliefs).copy(sentenceBeliefs = newSentences.toArray))
  }

  def marginal(doc: Document, aug: DocumentBeliefs): (Marginal, Double) = {
    val anchorings = beliefsToAnchoring(doc, aug)
    val marginals = for((s,anchoring) <- doc.sentences zip anchorings) yield {
      inner.marginal(s.nerSegmentation, anchoring )
    }

    val partition = marginals.map(_._2).sum
    Marginal(marginals.map(_._1), partition) -> partition
  }

  def beliefsToAnchoring(doc: Document, beliefs: DocumentBeliefs):IndexedSeq[SemiCRF.Anchoring[NERType.Value, String]] = {
    lens.slice(beliefs).sentenceBeliefs.zip(doc.sentences).map { case(b, s) =>
      new Anchoring[NERType.Value, String] {
        def labelIndex: Index[NERType.Value] = labels

        def w: IndexedSeq[String] = s.words

        def maxSegmentLength(label: Int): Int = inner.maxLength(label)

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          math.log(b.spanBeliefs(cur,beg)(0)(cur)) - math.log1p(-b.spanBeliefs(cur,beg)(0)(cur))
        }

        def startSymbol = inner.startSymbol
      }
    }
  }
}

