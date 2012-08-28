package epic.coref

import breeze.util.{MutableIndex, Index}
import epic.framework.Feature
import breeze.collection.mutable.TriangularArray
import breeze.linalg.SparseVector
import collection.immutable.BitSet
import pairwise.PairwiseFeaturizer
import java.util

/**
 * Computes and indexes all features, along with what features
 * are active for all pairs...
 * @author dlwh
 */
case class PropIndexer(index: Index[Feature],
                       feat: PairwiseFeaturizer,
                       extractors: IndexedSeq[PropertyExtractor],
                       instances: IndexedSeq[IndexedCorefInstance],
                       featuresForProperties: Array[IndexedPropertyFeatures]) {
}

case class IndexedPropertyFeatures(prop: Property, agree: Int, mismatch: Int, pairs: Array[Array[Int]]) {
  def arity = prop.arity
}

object PropIndexer {
  def apply(feat: PairwiseFeaturizer, extractors: IndexedSeq[PropertyExtractor], instances: IndexedSeq[CorefInstance]): PropIndexer = {
    val index = Index[Feature]()

    val indexed = for (inst <- instances) yield {
      val length = inst.numMentions
      val pairArray = TriangularArray.tabulate[SparseVector[Double]](length + 1){ indexPair(inst, _, _, feat, index) }
      val properties = null +: inst.mentions.toArray.map(indexProperties(extractors, _, inst))

      // now for clusters
      // 0 is for root
      val mentionMap = Map.empty ++ (null +: inst.mentions).zipWithIndex
      val clusters = for (cluster: Set[MentionCandidate] <- inst.clusters.toIndexedSeq) yield BitSet.empty ++ cluster.map(mentionMap)

      new IndexedCorefInstance(inst, clusters, pairArray, properties)
    }

    // index all property-features
    val featuresForProperties = for (ex <- extractors.toArray) yield {
      val prop = ex.property
      val agreeFeature = PropertyAgreementFeature(prop.name)
      val disagreeFeature = PropertyMismatchFeature(prop.name)
      val (agg, dis) = (index.index(agreeFeature), index.index(disagreeFeature))
      val pairArray = Array.tabulate(prop.arity, prop.arity){ (i,j) =>
        index.index(PropertyFeature(prop.name, prop.choices.get(i), prop.choices.get(j)))
      }

      IndexedPropertyFeatures(prop, agg, dis, pairArray)
    }

    // we set SparseVectors to have length maxint, now we set them right.
    val fixed = indexed.map(inst => inst.copy(features = TriangularArray.tabulate[SparseVector[Double]](inst.numMentions + 1){
      (a, b) =>
        if (a == 0 && b == 0) null
        else {
          val tooLong = inst.featuresFor(a, b)
          //      tooLong.compact()
          val justRight = new SparseVector[Double](tooLong.index, tooLong.data, tooLong.used, index.size)
          justRight
        }
    }))

    new PropIndexer(index, feat, extractors, fixed, featuresForProperties)
  }

  private def indexPair(inst: CorefInstance, a: Int, b: Int, feat: PairwiseFeaturizer, index: MutableIndex[Feature]): SparseVector[Double] = {
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
  }

  private def indexProperties(ex: IndexedSeq[PropertyExtractor], c: MentionCandidate, context: CorefDocument) = {
    ex.map(_.extract(c, context)).toArray

  }
}
