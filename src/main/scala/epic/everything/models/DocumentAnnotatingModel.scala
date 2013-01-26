package epic.everything.models

import epic.framework.{AugmentableInference, Model}
import epic.everything._

/**
 *
 * @author dlwh
 */
trait DocumentAnnotatingModel extends Model[ProcessedDocument] { self =>
  type Inference <: DocumentAnnotatingInference {type ExpectedCounts = self.ExpectedCounts; type Marginal = self.Marginal }

  type EvaluationResult <: epic.framework.EvaluationResult[self.EvaluationResult]
  def evaluate(guess: ProcessedDocument, gold: ProcessedDocument):EvaluationResult
}

trait DocumentAnnotatingInference extends AugmentableInference[ProcessedDocument, DocumentBeliefs] {
  def annotate(doc: ProcessedDocument, m: Marginal): ProcessedDocument
}
