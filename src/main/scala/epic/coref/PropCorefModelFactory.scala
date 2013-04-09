package epic.coref

import breeze.util.{MutableIndex, Index}
import epic.framework.Feature
import breeze.collection.mutable.TriangularArray
import breeze.linalg.VectorBuilder
import epic.ontonotes.Document


trait CorefInstanceFeaturizer {
  def featureIndex: Index[Feature]
  def propertyFeatures: IndexedSeq[PropertyFeatures[_]]
  def featurize(inst: CorefInstance):FeaturizedCorefInstance
  def featurizeDocument(inst: Document): FeaturizedCorefInstance
}

object CorefInstanceFeaturizer {
  def fromTrainingSet(featurizer: PairwiseFeaturizer,
                      extractors: IndexedSeq[PropertyExtractor[_]],
                      processor: CorefInstance.Factory)
                     (instances: IndexedSeq[Document]):(CorefInstanceFeaturizer, IndexedSeq[FeaturizedCorefInstance]) = {
    val index = Index[Feature]()

    def indexPair(inst: CorefDocument, a: Int, b: Int, index: MutableIndex[Feature]) = {
      val ctr = if (a == b) {
        featurizer.featuresForRoot(inst.mentions(b), inst)
      } else {
        featurizer.featuresFor(inst.mentions(a), inst.mentions(b), inst)
      }
      val vb = new VectorBuilder[Double](Int.MaxValue, ctr.size)
      for ((f,v) <- ctr.iterator) {
        vb.add(index.index(f), v)
      }
      vb
    }

    val preinstances = for (doc <- instances) yield {
      val inst = processor(doc)
      val length = inst.numMentions
      val pairwiseFeatures = TriangularArray.tabulate(length){ (anaphor, mention) =>
        indexPair(inst, anaphor, mention, index)
      }
      val properties = Array.tabulate(length){ mention =>
        extractors.map(_.extract(inst.mentions(mention), inst)).toArray
      }

      (inst, pairwiseFeatures, properties)
    }

    val fixedInstances = for( (inst, pf, prop) <- preinstances) yield {
      val fixedFeatures = pf.map{vb => vb.length = index.size; vb.toSparseVector}
      FeaturizedCorefInstance(inst, fixedFeatures, prop)
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

    val feat = new CorefInstanceFeaturizer {
      val featureIndex = index

      def propertyFeatures: IndexedSeq[PropertyFeatures[_]] = featuresForProperties

      def featurize(inst: CorefInstance): FeaturizedCorefInstance = {
        val length = inst.numMentions
        val pairwiseFeatures = TriangularArray.tabulate(length){ (anaphor, mention) =>
          innerPair(inst, anaphor, mention, index)
        }
        val properties = Array.tabulate(length){ mention =>
          extractors.map(_.extract(inst.mentions(mention), inst)).toArray
        }

        FeaturizedCorefInstance(inst, pairwiseFeatures, properties)
      }


      def featurizeDocument(inst: Document): FeaturizedCorefInstance = {
        featurize(processor(inst))
      }

      def innerPair(inst: CorefDocument, a: Int, b: Int, index: Index[Feature]) = {
        val ctr = if (a == b) {
          featurizer.featuresForRoot(inst.mentions(b), inst)
        } else {
          featurizer.featuresFor(inst.mentions(a), inst.mentions(b), inst)
        }
        val vb = new VectorBuilder[Double](Int.MaxValue, ctr.size)
        for ((f,v) <- ctr.iterator) {
          val fg = index(f)
          if(fg != -1)
            vb.add(fg, v)
        }
        vb.toSparseVector
      }
    }



    feat -> fixedInstances

  }


}
