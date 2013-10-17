package epic.dense

import breeze.linalg._
import operators._
import epic.framework.Feature
import breeze.util.Index

/**
 *
 *
 * @author dlwh
 */
trait Transform[In, Out] {
  val index: Index[Feature]

  def extractLayer(dv: DenseVector[Double]):Layer

  type Layer <: _Layer

  trait _Layer {

    def activations(fv: In):Out

    def tallyDerivative(deriv: DenseVector[Double], scale: DenseVector[Double], fv: In)

  }

}
