package epic.everything.models

import epic.framework.{AugmentableInference, Model}
import epic.everything._
import breeze.linalg.DenseVector

/**
 *
 * @author dlwh
 */
trait DocumentAnnotatingModel extends Model[ProcessedDocument] { self =>
  type Inference <: DocumentAnnotatingInference {type ExpectedCounts = self.ExpectedCounts }
}

trait DocumentAnnotatingInference extends AugmentableInference[ProcessedDocument, DocumentBeliefs] with DocumentAnnotator {
  def apply(doc: ProcessedDocument): ProcessedDocument = apply(doc, baseAugment(doc))
}
