package epic.everything.models

import epic.framework.{AugmentableInference, Model}
import epic.everything._

/**
 *
 * @author dlwh
 */
trait DocumentAnnotatingModel extends Model[ProcessedDocument] { self =>
  type Inference <: DocumentAnnotatingInference {type ExpectedCounts = self.ExpectedCounts; type Marginal = self.Marginal }
}

trait DocumentAnnotatingInference extends AugmentableInference[ProcessedDocument, DocumentBeliefs] with DocumentAnnotator {
  def apply(doc: ProcessedDocument): ProcessedDocument = apply(doc, baseAugment(doc))
}
