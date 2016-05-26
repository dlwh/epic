package epic.logo

import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer
import scala.runtime.DoubleRef

class Instance[T, W](val x : T) {

  var slack = new DoubleRef(0.0)

  val alphas = new ArrayBuffer[Double]() //(1.0)
  val constraints = new ArrayBuffer[(W, Double)]()
}
