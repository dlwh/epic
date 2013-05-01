package epic.sequences

import breeze.linalg.{SparseVector, Vector}
import breeze.collection.mutable.TriangularArray
import breeze.features.FeatureVector

class CompressedFeatureCache(data: Array[Array[TriangularArray[FeatureVector]]]) {
  def apply(value: Int) = data(value)
}

