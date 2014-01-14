package epic.dense

import breeze.linalg._
import breeze.util.Index
import epic.framework.Feature


class IdentityTransform[T] extends Transform[T, T] {

  val index = Index[Feature]()


  def extractLayer(weights: DenseVector[Double]) = {
    new Layer()
  }

  class Layer extends _Layer {
    val index = IdentityTransform.this

    def activations(fv: T) = fv

    def tallyDerivative(deriv: DenseVector[Double], scale: DenseVector[Double], t: T) = {
    }

  }

}

