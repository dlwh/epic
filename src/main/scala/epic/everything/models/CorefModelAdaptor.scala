package epic.everything.models

import epic.coref.{PropCorefInference, PropCorefModel}
import breeze.util.Index
import epic.framework.{FullProjectableInference, Feature}
import breeze.linalg.DenseVector
import epic.sequences.{SemiCRF, SemiCRFInference}
import epic.everything.{DSpan, ProcessedDocument, NERType}
import breeze.collection.mutable.TriangularArray
import epic.sequences.SemiCRF.Anchoring

/**
 * 
 * @author dlwh
 */
class CorefModelAdaptor(val base: PropCorefModel,
                        lens: DocumentBeliefs.Lens) extends DocumentAnnotatingModel {
  type ExpectedCounts = base.ExpectedCounts
  type Inference = CorefInferenceAdaptor

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    base.expectedCountsToObjective(ecounts)
  }

  def featureIndex: Index[Feature] = base.featureIndex

  def initialValueForFeature(f: Feature): Double = base.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]): Inference = {
    new CorefInferenceAdaptor(base.inferenceFromWeights(weights), lens)
  }
}

case class CorefInferenceAdaptor(inner: PropCorefInference,
                                 lens: DocumentBeliefs.Lens) extends DocumentAnnotatingInference with FullProjectableInference[ProcessedDocument, DocumentBeliefs] {
  type ExpectedCounts = inner.ExpectedCounts
  type Marginal = inner.Marginal

  def emptyCounts = inner.emptyCounts

  // annotation methods
  def apply(doc: ProcessedDocument, beliefs: DocumentBeliefs): ProcessedDocument = {
    error("TODO")
  }

  // inference methods
  def goldMarginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
    inner.goldMarginal(doc.coref, lens.slice(aug))
  }


  def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, aug: DocumentBeliefs): ExpectedCounts = {
    inner.countsFromMarginal(doc.coref, marg, lens.slice(aug))
  }



  def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
    lens.initialFullBeliefs(doc)
  }


  def projectGold(doc: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(doc, m, lens.slice(oldAugment))
  }


  def project(doc: ProcessedDocument, m: Marginal, oldBeliefs: DocumentBeliefs): DocumentBeliefs = {
    val projected = inner.project(doc.coref, m, lens.slice(oldBeliefs))
    lens.recombine(oldBeliefs, projected)
  }

  def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
    inner.marginal(doc.coref, aug)
  }

}
