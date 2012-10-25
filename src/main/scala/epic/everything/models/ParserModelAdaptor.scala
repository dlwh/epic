package epic.everything.models

import epic.parser.models.{ParserInference, ParserModel}
import epic.trees.{TreeInstance, Tree, AnnotatedLabel}
import breeze.util.Index
import epic.framework.{FullProjectableInference, FullEPInference, Feature}
import breeze.linalg.DenseVector
import epic.everything.{ProcessedDocument, DocumentAnnotator}
import epic.parser.CoreAnchoring

/**
 *
 * @author dlwh
 ** //
class ParserModelAdaptor(val inner: ParserModel[AnnotatedLabel, String], lens: DocumentBeliefs.Lens) extends DocumentAnnotatingModel {
  type ExpectedCounts = inner.ExpectedCounts

  def featureIndex: Index[Feature] = inner.featureIndex

  def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)



  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    inner.expectedCountsToObjective(ecounts)
  }

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    new ParserInferenceAdaptor(inner.inferenceFromWeights(weights), lens)
  }

  type Inference = ParserInferenceAdaptor
}

class ParserInferenceAdaptor(val inner: ParserInference[AnnotatedLabel, String],
                            lens: DocumentBeliefs.Lens) extends DocumentAnnotatingInference with FullProjectableInference[ProcessedDocument, DocumentBeliefs] {
  case class Marginal(sentences: IndexedSeq[inner.Marginal], partition: Double)
  type ExpectedCounts = inner.ExpectedCounts
  def emptyCounts = inner.emptyCounts

  def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
    lens.initialFullBeliefs(doc)
  }


  def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
    val trees = doc.sentences.map(_.tree)
    val words = doc.sentences.map(_.words)

  }

  def projectGold(v: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(v, m, oldAugment)
  }

  def countsFromMarginal(v: ProcessedDocument, marg: ParserInferenceAdaptor#Marginal, aug: DocumentBeliefs): ExpectedCounts = {
    val counts = emptyCounts
    val instances: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = error("TODO")
    val adaptScorer: IndexedSeq[CoreAnchoring[AnnotatedLabel, String]] = error("TODO")
    for( i <- (0 until marg.sentences.length)) counts += inner.countsFromMarginal(instances(i), marg.sentences(i), adaptScorer(i))

    counts
  }


}
 //*/

