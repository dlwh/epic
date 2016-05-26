package epic.logo;

import breeze.linalg._
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.framework.Example
import org.scalatest.FunSuite
import breeze.math._

class ClassifierTest extends FunSuite {


  private def example(label: Boolean, features: Map[Int, Double]) = {
    Example(label, Counter(features))
  }

  test("simple example") {
    val trainingData = Array (
      example(true, Map(0 -> 1.0, 1 -> 1.0, 2 -> 1.0)),
      example(false,Map(0 -> 1.0, 1 -> 1.0, 3 -> 1.0)),
      example(true, Map(1 -> 1.0, 4 -> 1.0))
    )

    val testData = Array(
      example(true, Map(1 -> 1.0,2 -> 1.0))
    )

    val c = Trainer.trainL1MaxMarginMulticlassClassifier(
      IndexedSeq(false, true), trainingData,
      labelConjoiner = ((label: Boolean, f: Counter[Int, Double]) =>
        Counter(f.toMap.map { case (k, v) => ((label, k), v) })),
      C = 1000.0,
      oneSlackFormulation = false,
      initialConstraint = Counter[(Boolean, Int), Double]())

    for(x <- trainingData) {
      assert(x.label == c(x.features), x)
    }

    val r = c(testData(0).features)
    assert(r == testData(0).label)
  }

}
