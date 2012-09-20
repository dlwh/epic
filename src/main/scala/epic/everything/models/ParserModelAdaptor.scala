package epic.everything.models

import epic.parser.models.{ParserInference, ParserModel}
import epic.trees.{Tree, AnnotatedLabel}
import breeze.util.Index
import epic.framework.Feature
import breeze.linalg.DenseVector
import epic.everything.{Document, DocumentAnnotator}

/**
 *
 * @author dlwh
class ParserModelAdaptor(val inner: ParserModel[AnnotatedLabel, String]) extends DocumentAnnotatingModel {
  type ExpectedCounts = inner.ExpectedCounts

  def featureIndex: Index[Feature] = inner.featureIndex

  def initialValueForFeature(f: Feature): Double = inner.initialValueForFeature(f)

  def emptyCounts = inner.emptyCounts

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    inner.expectedCountsToObjective(ecounts)
  }

  def annotator(weights: DenseVector[Double]): DocumentAnnotator = {
    val parser = inner.extractParser(weights)

    new DocumentAnnotator {
      def apply(v1: Document): Document = {
        val newSentences = v1.sentences.map(s => s.annotate(tree=parser(s.words)))
        v1.copy(sentences=newSentences)
      }
    }
  }

  type Inference = ParserInferenceAdaptor
}

class ParserInferenceAdaptor(val inner: ParserInference[AnnotatedLabel, String]) extends DocumentAnnotatingInference {
  def baseAugment(v: Document): DocumentBeliefs = TODO


  type ExpectedCounts = inner.ExpectedCounts

  def guessCounts(value: Document, augment: DocumentBeliefs): ExpectedCounts = {

  }

  def goldCounts(value: Document, augment: DocumentBeliefs): ExpectedCounts = null

} */

