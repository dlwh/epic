//package epic.dense
//
//import breeze.linalg._
//import breeze.util.Index
//import epic.framework.Feature
//
//
//class IdentityTransformFloat[T] extends TransformFloat[T, T] {
//
//  val index = Index[Feature]()
//
//
//  def extractLayer(weights: DenseVector[Float]) = {
//    new Layer()
//  }
//
//  class Layer extends _Layer {
//
//    def activations(fv: T) = fv
//
//    def tallyDerivative(deriv: DenseVector[Float], scale: =>Vector[Float], t: T) = {}
//  }
//
//}
//
