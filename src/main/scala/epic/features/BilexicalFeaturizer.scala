package epic.features

import epic.framework.Feature
import epic.trees.DependencyTree
import breeze.util.OptionIndex
import java.util


/**
 * TODO
 *
 * @author dlwh
 **/
trait BilexicalFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):BilexicalFeatureAnchoring[W]
}

trait BilexicalFeatureAnchoring[W] {
  def featuresForAttachment(head: Int, dep: Int, level: FeaturizationLevel = FeaturizationLevel.FullFeatures):Array[Feature]
}


@SerialVersionUID(1L)
class ProductBilexicalFeaturizer[W](wf: IndexedWordFeaturizer[W],
                                    val featureIndex: FeatureIndex[Feature]) extends IndexedBilexicalFeaturizer[W] with Serializable {

  def featurizer: WordFeaturizer[W] = wf.featurizer

  def anchor(w: IndexedSeq[W]): IndexedBilexicalFeatureAnchoring[W] = new IndexedBilexicalFeatureAnchoring[W] {
    val anc = wf.anchor(w)
    val arr = Array.ofDim[Array[Array[Int]]](anc.words.length, anc.words.length)

    def words: IndexedSeq[W] = anc.words

    def featuresForWord(pos: Int, level: FeaturizationLevel): Array[Int] = anc.featuresForWord(pos, level)

    def featuresForAttachment(head: Int, dep: Int, level: FeaturizationLevel): Array[Int] = {
      var ret = arr(head)(dep)
      if (ret eq null) {
        ret = new Array[Array[Int]](FeaturizationLevel.numLevels)
        arr(head)(dep) = ret
      }

      if (ret(level.level) eq null) {
        val f1 = featureIndex.crossProduct(anc.featuresForWord(head, level), anc.featuresForWord(dep, FeaturizationLevel.MinimalFeatures), false)
        val f2 = featureIndex.crossProduct(anc.featuresForWord(head, FeaturizationLevel.MinimalFeatures), anc.featuresForWord(dep, level), false)
        ret(level.level) = f1 ++ f2
      }

      ret(level.level)
    }
  }
}

case class BilexicalFeature(head: Any, dep: Any) extends Feature

trait IndexedBilexicalFeaturizer[W] extends IndexedWordFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):IndexedBilexicalFeatureAnchoring[W]
  def featurizer: WordFeaturizer[W]
}

trait IndexedBilexicalFeatureAnchoring[W] extends IndexedWordAnchoring[W] {
  def featuresForAttachment(head: Int, dep: Int, level: FeaturizationLevel = FeaturizationLevel.FullFeatures):Array[Int]
}

object IndexedBilexicalFeaturizer {
  def fromData[L, W](wfeat: IndexedWordFeaturizer[W],
                  depTrees: IndexedSeq[DependencyTree[L, W]],
                  hashFeatures: HashFeature.Scale = HashFeature.Relative(1.0)):IndexedBilexicalFeaturizer[W] =  {
    val index = FeatureIndex.build(wfeat.featureIndex, wfeat.featureIndex, hashFeatures, "Bilexical") { adder =>
      for (tree <- depTrees) {
        val wanch = wfeat.anchor(tree.words)
        for( (head, dep) <- tree.arcs if head < tree.words.length) {
          adder(wanch.featuresForWord(head, FeaturizationLevel.MinimalFeatures),
            wanch.featuresForWord(dep, FeaturizationLevel.FullFeatures))
          adder(wanch.featuresForWord(head, FeaturizationLevel.FullFeatures),
            wanch.featuresForWord(dep, FeaturizationLevel.MinimalFeatures))
        }
      }
    }

    new ProductBilexicalFeaturizer[W](wfeat, index)
  }

}