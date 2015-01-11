package epic.dense

import breeze.linalg._
import breeze.util.Index
import epic.framework.Feature

/**
 *
 *
 * @author dlwh
 */
trait Transform[In, +Out] {
  val index: Index[Feature]


  def extractLayer(dv: DenseVector[Double]):Layer

  type Layer <: _Layer

  trait _Layer {

    def index = Transform.this.index

    def activations(fv: In):Out

    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)

  }

}
