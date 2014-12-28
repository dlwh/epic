//package epic.dense
//
//import breeze.linalg._
//import breeze.util.Index
//import epic.framework.Feature
//
///**
// *
// *
// * @author dlwh
// */
//trait TransformFloat[In, +Out] {
//  val index: Index[Feature]
//
//
//  def extractLayer(dv: DenseVector[Float]):Layer
//
//  type Layer <: _Layer
//
//  trait _Layer {
//
//    def index = TransformFloat.this.index
//
//    def activations(fv: In):Out
//
//    def tallyDerivative(deriv: DenseVector[Float], scale: =>Vector[Float], fv: In)
//
//  }
//
//}
