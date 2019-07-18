package epic.features

import breeze.linalg.DenseVector
import breeze.util.Index
import epic.features.HashFeature.Absolute
import epic.framework.Feature
import org.scalatest.FunSuite

/**
 * TODO
 *
 * @author dlwh
 **/
class CrossProductIndexTest extends FunSuite {
  test("buildSparseMatrix, no buckets") {
    val index1 = Index(Iterator('A, 'B, 'C, 'D))
    val index2 = Index(Iterator(1, 2, 3, 4, 5))
    val cpbuilder = new CrossProductIndex.Builder(index1, index2, includeLabelOnlyFeatures = true)

    cpbuilder.add('A, 1)
    cpbuilder.add('A, 4)
    cpbuilder.add('B, 2)
    cpbuilder.add('C, 2)
    cpbuilder.add('C, 3)
    cpbuilder.add('D, 4)

    val res = cpbuilder.result()

    val weights = DenseVector.rand[Double](res.size)

    val (csc, intercept) = res.buildSparseMatrix(weights)

    for(i <- 0 until index1.size; j <- 0 until index2.size) {
      val mapped = res.mapped(i, j)
      if (mapped >= 0)
        assert(csc(i, j) === weights(mapped))
    }

    for(i <- 0 until index1.size) {
      assert(intercept(i) === weights(i))
    }
  }


  test("buildSparseMatrix, many buckets") {
    val index1 = Index(Iterator('A, 'B, 'C, 'D))
    val index2 = Index(Iterator(1, 2, 3, 4, 5))
    val cpbuilder = new CrossProductIndex.Builder(index1, index2, includeLabelOnlyFeatures = true, hashFeatures = Absolute(4))

    cpbuilder.add('A, 1)
    cpbuilder.add('A, 4)
    cpbuilder.add('B, 2)
    cpbuilder.add('C, 2)
    cpbuilder.add('C, 3)
    cpbuilder.add('D, 4)

    val res = cpbuilder.result()

    val weights = DenseVector.rand[Double](res.size)

    val (csc, intercept) = res.buildSparseMatrix(weights)

    for(i <- 0 until index1.size; j <- 0 until index2.size) {
      val mapped = res.mapped(i, j)
      if (mapped >= 0)
        assert(csc(i, j) === weights(mapped))
    }

    for(i <- 0 until index1.size) {
      assert(intercept(i) === weights(i))
    }
  }


  test("cross product filtering, many buckets") {
    val index1 = Index(Iterator('A, 'B, 'C, 'D))
    val index2 = Index(Iterator(1, 2, 3, 4, 5))
    val cpbuilder = new CrossProductIndex.Builder(index1, index2, includeLabelOnlyFeatures = true, hashFeatures = Absolute(4))

    cpbuilder.add('A, 1)
    cpbuilder.add('A, 4)
    cpbuilder.add('B, 2)
    cpbuilder.add('C, 2)
    cpbuilder.add('C, 3)
    cpbuilder.add('D, 4)

    val res = cpbuilder.result()
    
    val newCP = res.prune(i => index1.get(res.labelPart(i)) == 'A)

    assert(newCP.toSet === res.iterator.toSet[Feature].filterNot { case CrossProductFeature('A, _, _) => true; case (_: Feature) => false })
    assert(newCP.size === 12) // 4 label + 4 hash + 4 non-4 features
    assert(newCP(CrossProductFeature('A, 4)) === -1)
    assert(newCP(CrossProductFeature('A, 1)) === -1)
    assert(newCP(CrossProductFeature('D, 5)) === -1)
    assert(newCP.secondIndex(1) === -1)
    assert(newCP.secondIndex(4) !== -1)

  }

}
