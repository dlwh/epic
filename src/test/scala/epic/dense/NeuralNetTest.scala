package epic.dense

import org.scalatest.FunSuite
import breeze.optimize.{GradientTester, DiffFunction}
import breeze.linalg.DenseVector
import epic.framework.ModelObjective
import breeze.util.{DenseIntIndex, Index}
import breeze.features.FeatureVector

/**
 *
 *
 * @author dlwh
 */
class NeuralNetTest extends FunSuite {

  test("chain rule") {
    val index = new SigmoidTransform(2, 10, true)
    val model = new NeuralNet.Model(Index(0 to 1), index)
    val obj = new ModelObjective(model, IndexedSeq(NeuralNet.ClassificationInstance(1, DenseVector.rand(10))))

    val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size) - 0.5
    val diffs = GradientTester.test[Int, DenseVector[Double]](obj, weights, randFraction = 1.0)
    assert(diffs.max < 1E-3, s"${diffs.max} was bigger than expected!!")
  }

  test("devlin chain rule") {
    System.setProperty(org.slf4j.impl.SimpleLogger.DEFAULT_LOG_LEVEL_KEY, "WARN")
    val index = new AffineTransform(2, 90, new SigmoidTransform(new DevlinTransform(new DenseIntIndex(10), 10)))
    val model = new NeuralNet.Model(Index(0 to 1), index)
    val obj = new ModelObjective(model, IndexedSeq(NeuralNet.ClassificationInstance(1, new FeatureVector(Array.range(9,0,-1)))))

    val weights: DenseVector[Double] = DenseVector.rand[Double](index.index.size) - 0.5
    val diffs = GradientTester.test[Int, DenseVector[Double]](obj, weights, randFraction = 1.0)
    assert(diffs.max < 1E-3, s"${diffs.max} was bigger than expected!!")

  }



}
