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

  type Layer <: Transform.Layer[In,Out]
}

object Transform {
  
  trait Layer[In, +Out] {

    def index: Index[Feature];

    def activations(fv: In):Out

    def tallyDerivative(deriv: DenseVector[Double], scale: =>Vector[Double], fv: In)

  }

}
