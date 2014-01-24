package epic.dense

import org.scalatest.FunSuite
import breeze.optimize.{GradientTester, DiffFunction}
import breeze.linalg.DenseVector

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
         val layer = index.extractLayer(x)
         val acts = layer.activations(dv)
         val obj = acts.sum
         val deriv = DenseVector.zeros[Double](x.length)
         layer.tallyDerivative(deriv, DenseVector.ones[Double](acts.length), dv)
         obj -> deriv
       }
     }

     val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size) - 0.5
     val diffs = GradientTester.test[Int, DenseVector[Double]](objective, weights, randFraction = 1.0)
     assert(diffs.max < 1E-3, s"${diffs.max} was bigger than expected!!")
   }

 }
