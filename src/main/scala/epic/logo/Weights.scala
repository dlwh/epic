package epic.logo
import scala.collection.JavaConversions._
import java.util.Arrays
import breeze.linalg._
import breeze.math.MutableInnerProductModule

class Weights[W](var underlying: W, var scale : Double = 1.0)(implicit space: MutableInnerProductModule[W, Double]) {

  import space._
  var norm = calcNormSquared
  private def calcNormSquared = (underlying dot underlying) * scale * scale

  def checkNorm = {
    val calcNorm = calcNormSquared
    assert(NumUtils.approxEquals(this.`^2`, calcNorm, 1e-5))
  }

  def approxEquals(w : Weights[W]) = {
    breeze.linalg.norm(compile - w.compile) < 1e-5
  }

  def compile: W = underlying * scale

  override def toString() : String = {
    scale + "*" + underlying
  }

  def *=(d : Double) = {
    scale *= d
    norm *= sq(d)
  }

  def *(fv : W) = {
    scale * (underlying dot fv)
  }

  def +=(fv : W) = increment(fv, 1.0)

  private def increment(fv: W, d: Double) = {
    norm += sq(d / scale) * (fv dot fv)
    norm += 2 * d * (fv dot underlying) / scale
    axpy(d / scale, fv, underlying)
  }

  private final def sq(d : Double) = d * d

  def increment(w : Weights[W], d : Double) = {
    underlying += w.underlying * w.scale * d
  }

  def -=(fv : W) = {
    increment(fv, -1.0)

  }

  def `^2` = norm


  def zeroOut() = {
    underlying = space.zeroLike.apply(underlying)
  }
}
