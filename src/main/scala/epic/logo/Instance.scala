package epic.logo

import scala.collection.mutable.ArrayBuffer

/**
 * This class is mutable for efficiency purposes.
 */
case class Instance[T, W](var x: T,
                          var alphas: ArrayBuffer[Double] = ArrayBuffer[Double](),
                          var slack: Double = 0.0,
                          var constraints: ArrayBuffer[(W, Double)] = ArrayBuffer[(W, Double)]())
