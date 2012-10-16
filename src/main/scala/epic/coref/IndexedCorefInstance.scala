package epic.coref

import breeze.collection.mutable.TriangularArray
import breeze.linalg.SparseVector
import collection.immutable.BitSet

/**
 *
 * @author dlwh
 */
case class IndexedCorefInstance(unindexed: CorefInstance,
                                goldClusters: IndexedSeq[BitSet],
                                features: TriangularArray[SparseVector[Double]],
                                // instance -> property -> value for property (-1 is unknown)
                                properties: Array[Array[Int]]) {
  def numMentions = unindexed.numMentions
  def featuresFor(a: Int, b: Int) = {
    features(a, b)
  }
}
