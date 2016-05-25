package epic.logo

trait Decoder[T, W] {

  def decode(weights : Weights[W], instance : T) : (FeatureVector[W], Double)

}
