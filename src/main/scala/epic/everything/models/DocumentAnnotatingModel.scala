package epic.everything.models

import epic.framework.{AugmentableInference, Model}
import epic.everything.{DocumentAnnotator, Document}
import breeze.linalg.DenseVector

/**
 *
 * @author dlwh
 */
trait DocumentAnnotatingModel extends Model[Document] { self =>
  type Inference <: DocumentAnnotatingInference {type ExpectedCounts = self.ExpectedCounts }

  def annotator(weights: DenseVector[Double]): DocumentAnnotator
}

trait DocumentAnnotatingInference extends AugmentableInference[Document, DocumentBeliefs]
