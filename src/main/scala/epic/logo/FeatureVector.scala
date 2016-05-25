package epic.logo

import scala.collection.JavaConversions._
import breeze.linalg._
import breeze.math.MutableInnerProductModule

case class FeatureVector[W](var data :W)(implicit space: MutableInnerProductModule[W, Double]) {
  import space._

  def normSquared() = data dot data

  def ^(x : Int) = {
    assert(x == 2)
    normSquared()
  }


  override def toString() = data.toString()

  def dotProduct(w : Weights[W]) = (data dot w.array) * w.scale

  def *(w : Weights[W]) = dotProduct(w)

  def *(fv : FeatureVector[W]) = data dot fv.data

  def *(c : Double) = {
    new FeatureVector(data * c)
  }

  def subtract(that : FeatureVector[W]) = {
    new FeatureVector(data - that.data)
  }

  def add(that : FeatureVector[W]) = {
     new FeatureVector(data + that.data)
  }

  def addWeighted(that : FeatureVector[W], c : Double) = {
    axpy(c, that.data, data)
  }

  def -(that : FeatureVector[W]) = subtract(that)

  def +(that : FeatureVector[W]) = add(that)

  def +=(that : FeatureVector[W]) = addWeighted(that, 1.0)

  def -=(that : FeatureVector[W]) = addWeighted(that, -1.0)

}
