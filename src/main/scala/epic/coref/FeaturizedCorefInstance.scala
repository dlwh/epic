package epic.coref

import breeze.collection.mutable.TriangularArray
import breeze.linalg.{SparseVector, VectorBuilder}

/**
 * 
 * @author dlwh
 */
case class FeaturizedCorefInstance(unindexed: CorefInstance,
                                   features: TriangularArray[SparseVector[Double]],
                                   properties: Array[Array[Int]]) {
  import unindexed._
  def mentions = unindexed.mentions
  def numMentions = mentions.length

  def clusters = unindexed.clusters

  def clusterFor(m: Int) = reverseClusters(m)
  def featuresFor(anaphor: Int, mention: Int):SparseVector[Double] = features(anaphor, mention)
  def propertyValueFor(mention: Int, prop: Int): Int = properties(mention)(prop)
}

