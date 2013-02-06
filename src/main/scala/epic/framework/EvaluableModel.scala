package epic.framework

import breeze.linalg.DenseVector

/**
 * 
 * @author dlwh
 */
trait EvaluableModel[Datum] extends Model[Datum] { self =>
  type Inference <: AnnotatingInference[Datum] {type ExpectedCounts = self.ExpectedCounts; type Marginal = self.Marginal }
  type EvaluationResult <: epic.framework.EvaluationResult[self.EvaluationResult]
  def evaluate(guess: Datum, gold: Datum, logResults: Boolean):EvaluationResult

  def evaluate(data: IndexedSeq[Datum], weights: DenseVector[Double], logResults: Boolean = true):EvaluationResult = {
    val inf = inferenceFromWeights(weights)
    data.par.aggregate(None:Option[EvaluationResult])({(res, datum) =>
      val result = evaluate(inf.annotate(datum, inf.marginal(datum)), datum, logResults)
      Some(res.foldLeft(result)(_ + _))
    }, {(a,b) => if(a.isEmpty) b else if(b.isEmpty) a else Some(a.get + b.get)}).get
  }
}
