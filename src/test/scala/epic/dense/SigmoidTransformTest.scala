package epic.dense

import org.scalatest.FunSuite
import breeze.optimize.{GradientTester, DiffFunction}
import breeze.linalg.{sum, argmax, max, DenseVector}

/**
 *
 *
 * @author dlwh
 */
class SigmoidTransformTest extends FunSuite {

  test("chain rule") {
    val index = new SigmoidTransform(12, 10, true)
    val dv = DenseVector.rand(10)
    val objective = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val layer = index.extractLayer(x)
        val acts = layer.activations(dv)
        val obj = sum(acts)
        val deriv = DenseVector.zeros[Double](x.length)
        layer.tallyDerivative(deriv, DenseVector.ones[Double](acts.length), dv)
        obj -> deriv
      }
    }

    val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size) - 0.5
    val diffs = GradientTester.test[Int, DenseVector[Double]](objective, weights, randFraction = 1.0)
    assert(max(diffs) < 1E-3, s"${max(diffs)} was bigger than expected!!")
  }

  test("layered chain rule") {
    val index = new SigmoidTransform(new AffineTransform(20, 12, new SigmoidTransform(12, 10, true)))

    val dv = DenseVector.rand(10)
    val objective = new DiffFunction[DenseVector[Double]] {
      def calculate(x: DenseVector[Double]): (Double, DenseVector[Double]) = {
        val layer = index.extractLayer(x)
        val acts = layer.activations(dv)
        val obj = sum(acts)
        val deriv = DenseVector.zeros[Double](x.length)
        layer.tallyDerivative(deriv, DenseVector.ones[Double](acts.length), dv)
        obj -> deriv
      }
    }

    val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size)/2.0 - 0.25
    val diffs = GradientTester.test[Int, DenseVector[Double]](objective, weights, randFraction = 1.0)
    assert(max(diffs) < 1E-2, s"${max(diffs)} was bigger than expected!! ${argmax(diffs)} ${index.index.get(argmax(diffs))}")
  }

}
