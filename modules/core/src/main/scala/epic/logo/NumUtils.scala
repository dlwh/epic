package epic.logo

/**
 * @author adpauls
 */
object NumUtils {
  def approxEquals(a: Double, b: Double, tol: Double) = {
    if (Math.abs(a) < tol) Math.abs(b) < tol else Math.abs((a - b) / a) < tol
  }
}
