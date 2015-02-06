package epic.dense

import org.scalatest.FunSuite
import breeze.optimize.{GradientTester, DiffFunction}
import breeze.linalg._

/**
  *
  *
  * @author dlwh
  */
class AffineTransformTest extends FunSuite {

   test("chain rule") {
     val index = AffineTransform(12, 10, true)
     val dv = DenseVector.rand(10)
     val objective = new DiffFunction[DenseVector[Double]] {
       def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
         val layer = index.extractLayer(x, true)
         val acts = layer.activations(dv)
         val obj = acts.sum
         val deriv = DenseVector.zeros[Double](x.length)
         layer.tallyDerivative(deriv, DenseVector.ones[Double](acts.length), dv)
         obj -> deriv
       }
     }

     val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size) - 0.5
     val diffs = GradientTester.test[Int, DenseVector[Double]](objective, weights, randFraction = 1.0)
     assert(max(diffs) < 1E-3, s"${diffs.max} was bigger than expected!!")
   }

  test("chain rule 2") {
    val index = new AffineTransform(11, 12, AffineTransform(12, 10, true), true)
    val dv = DenseVector.rand(10) * 100.0
    val target = DenseVector.rand(11) * 100.0
    val objective = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val layer = index.extractLayer(x, true)
        val acts = layer.activations(dv)
        val obj = math.pow(norm(target - acts, 2), 2) / 2
        val initDeriv = acts - target
        val deriv = DenseVector.zeros[Double](x.length)
        layer.tallyDerivative(deriv,  initDeriv, dv)
        obj -> deriv
      }
    }

    val weights: DenseVector[Double] = (DenseVector.rand[Double](index.index.size) - 0.5) * 4.0
    val diffs = GradientTester.test[Int, DenseVector[Double]](objective, weights, randFraction = 1.0)
    assert(max(diffs) < 2E-2, s"${diffs.max} was bigger than expected!!")
  }

 }
