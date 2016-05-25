package epic.logo
import scala.collection.JavaConversions._
import java.util.Arrays
import breeze.linalg._
import breeze.math.MutableInnerProductModule

class Weights[W](var array: W, var scale : Double = 1.0)(implicit space: MutableInnerProductModule[W, Double]) {

  import space._
  var norm = calcNormSquared
  private def calcNormSquared = (array dot array) * scale * scale

  def checkNorm = {
    val calcNorm = calcNormSquared
    assert( NumUtils.approxEquals(this ^ 2, calcNorm, 1e-5))
  }

  def approxEquals(w : Weights[W]) = {
    breeze.linalg.norm(compile - w.compile) < 1e-5
  }

  def compile: W = array * scale

  override def toString() : String = {
    scale + "*" + array
  }

  def *=(d : Double) = {
    scale *= d
    norm *= sq(d)
  }

  def *(fv : FeatureVector[W]) = {
    array dot fv.data
  }

  def +=(fv : FeatureVector[W]) = increment(fv, 1.0)

  private def increment(fv: FeatureVector[W], d: Double) = {
    norm += sq(d / scale) * fv.normSquared()
    norm += 2 * d * (fv * this)
    axpy(d / scale, fv.data, array)
  }

  private final def sq(d : Double) = d * d

  def increment(w : Weights[W], d : Double) = {
    array += w.array * w.scale * d
  }

  def -=(fv : FeatureVector[W]) = {
    increment(fv, -1.0)

  }

  def ^(x : Int) = {
    assert(x == 2)
    norm
  }

  def zeroOut() = {
    array = space.zeroLike.apply(array)
  }
}
