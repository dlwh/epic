package epic.coref

import breeze.util.{MutableIndex, Index}
import epic.framework.Feature
import breeze.collection.mutable.TriangularArray
import breeze.linalg.{VectorBuilder, SparseVector}
import collection.immutable.BitSet
import epic.everything.models.Property
import epic.everything.{Document, DSpan}
import epic.coref.PropCorefModel.PropertyFeatures


class PropCorefModelFactory(featurizer: PairwiseFeaturizer, extractors: IndexedSeq[PropertyExtractor[_]]) {
  def makeModel(instances: IndexedSeq[CorefInstance]): PropCorefModel = {
    val index = Index[Feature]()
    for (inst <- instances) {
      val length = inst.numMentions
      for (i <- 0 until length; j <- 0 to i) {
        indexPair(inst, j, i, index)
      }
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

      PropertyFeatures(prop, agg, dis, pairArray)
    }

    val indexedFeaturizer = new IndexedFeaturizer(index, featuresForProperties)
    new PropCorefModel(indexedFeaturizer,  index)
  }

  case class IndexedFeaturizer(index: Index[Feature], properties: IndexedSeq[PropertyFeatures[_]]) extends PropCorefModel.Featurizer {

    def localize(inst: CorefDocument): Localization = new Localization {
      def featuresFor(anaphor: Int, mention: Int): VectorBuilder[Double] = {
        val ctr = if (anaphor == mention) {
          featurizer.featuresForRoot(inst.mentions(anaphor), inst)
        } else {
          featurizer.featuresFor(inst.mentions(anaphor), inst.mentions(mention), inst)
        }
        val vec = new VectorBuilder[Double](Int.MaxValue)
        for ((f, v) <- ctr.iterator) {
          vec.add(index(f),v)
        }
        vec
      }

      def propertyValueFor(mention: Int, prop: Int): Int = {
        extractors(prop).extract(inst.mentions(mention), inst)
      }
    }
  }

  private def indexPair(inst: CorefDocument, a: Int, b: Int, index: MutableIndex[Feature]) {
    val ctr = if (a == b) {
      featurizer.featuresForRoot(inst.mentions(b), inst)
    } else {
      featurizer.featuresFor(inst.mentions(a), inst.mentions(b), inst)
    }
    for (f <- ctr.keysIterator) {
      index.index(f)
    }
  }

}
