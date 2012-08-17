package epic.coref
package pairwise

import breeze.util.Index
import epic.framework.Feature
import breeze.collection.mutable.TriangularArray
import breeze.linalg.SparseVector
import collection.immutable.BitSet

/**
 *
 * @author dlwh
 */

case class PairwiseIndexer(index: Index[Feature],
                           feat: PairwiseFeaturizer,
                           instances: IndexedSeq[IndexedCorefInstance])

object PairwiseIndexer {
  def apply(feat: PairwiseFeaturizer, instances: IndexedSeq[CorefInstance]): PairwiseIndexer = {
    val index = Index[Feature]()

    val indexed = for (inst <- instances) yield {
      val length = inst.numMentions
      val array = new TriangularArray[SparseVector[Double]](length + 1, {
        (a: Int, b: Int) =>
          if (a == 0 && b == 0) null
          else {
            val ctr = if (a == 0) {
              feat.featuresForRoot(inst.mentions(b - 1), inst.words)
            } else {
              feat.featuresFor(inst.mentions(a - 1), inst.mentions(b - 1), inst.words)
            }
            val vec = SparseVector.zeros[Double](Int.MaxValue)
            for ((f, v) <- ctr.iterator) {
              vec(index.index(f)) = v
            }
            vec
          }
      })

      // now for clusters
      // 0 is for root
      val mentionMap = Map.empty ++ (null +: inst.mentions).zipWithIndex
      val clusters = for (cluster: Set[MentionCandidate] <- inst.clusters.toIndexedSeq) yield BitSet.empty ++ cluster.map(mentionMap)

      new IndexedCorefInstance(inst, clusters, array, null)
    }


    // we set sparsevectors to have length maxint, now we set them right.
    val fixed = indexed.map(inst => inst.copy(features = new TriangularArray[SparseVector[Double]](inst.numMentions + 1, {
      (a, b) =>
        if (a == 0 && b == 0) null
        else {
          val tooLong = inst.featuresFor(a, b)
          //      tooLong.compact()
          val justRight = new SparseVector[Double](tooLong.index, tooLong.data, tooLong.used, index.size)
          justRight
        }
    })))

    new PairwiseIndexer(index, feat, fixed)
  }
}
