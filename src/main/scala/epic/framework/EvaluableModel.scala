package epic.framework

import breeze.linalg.DenseVector

/**
 * A model that has some kind of evaluation function.
 * Used with an [[epic.framework.AnnotatingInference]], you
 * can make predictions for a test set and then get the performance.
 * @author dlwh
 */
trait EvaluableModel[Datum] extends Model[Datum] { self =>
  type Inference <: AnnotatingInference[Datum] {type ExpectedCounts = self.ExpectedCounts; type Marginal = self.Marginal }
  type EvaluationResult <: epic.framework.EvaluationResult[self.EvaluationResult]
  def evaluate(guess: Datum, gold: Datum):EvaluationResult

  def evaluate(data: IndexedSeq[Datum], weights: DenseVector[Double]):EvaluationResult = {
    val inf = inferenceFromWeights(weights)
    data.par.aggregate(None:Option[EvaluationResult])({(res, datum) =>
      val result = evaluate(inf.annotate(datum, inf.marginal(datum)), datum)
      Some(res.foldLeft(result)(_ + _))
    }, {(a,b) => if(a.isEmpty) b else if(b.isEmpty) a else Some(a.get + b.get)}).get
  }
}
