package epic.features

import epic.framework.Feature
import epic.trees.DependencyTree
import breeze.util.{Index, OptionIndex}
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
  def featuresForAttachment(head: Int, dep: Int):Array[Feature]
}


@SerialVersionUID(1L)
class ProductBilexicalFeaturizer[W](headFeaturizer: IndexedWordFeaturizer[W],
                                    depFeaturizer: IndexedWordFeaturizer[W],
                                    val featureIndex: CrossProductIndex[Feature, Feature]) extends IndexedBilexicalFeaturizer[W] with Serializable {

  def anchor(w: IndexedSeq[W]): IndexedBilexicalFeatureAnchoring[W] = new IndexedBilexicalFeatureAnchoring[W] {
    val headAnchoring = headFeaturizer.anchor(w)
    val depAnchoring = depFeaturizer.anchor(w)
    val cache = Array.ofDim[Array[Int]](words.length, words.length)

    def words: IndexedSeq[W] = w

    def featuresForAttachment(head: Int, dep: Int): Array[Int] = {
      var ret = cache(head)(dep)
      if (ret eq null) {
        val f1 = featureIndex.crossProduct(headAnchoring.featuresForWord(head), depAnchoring.featuresForWord(dep), usePlainLabelFeatures = false)
        ret = f1
        cache(head)(dep) = f1
      }

      ret
    }
  }
}

case class BilexicalFeature(head: Any, dep: Any) extends Feature

trait IndexedBilexicalFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):IndexedBilexicalFeatureAnchoring[W]
  def featureIndex: CrossProductIndex[Feature, Feature]
}

trait IndexedBilexicalFeatureAnchoring[W]  {
  def featuresForAttachment(head: Int, dep: Int):Array[Int]
}

object IndexedBilexicalFeaturizer {
  def fromData[L, W](headFeaturizer: IndexedWordFeaturizer[W],
                     depFeaturizer: IndexedWordFeaturizer[W],
                  depTrees: IndexedSeq[DependencyTree[L, W]],
                  hashFeatures: HashFeature.Scale = HashFeature.Relative(1.0)):IndexedBilexicalFeaturizer[W] =  {
    val builder = new CrossProductIndex.Builder(headFeaturizer.featureIndex, depFeaturizer.featureIndex, hashFeatures, "Bilexical")
    for (tree <- depTrees) {
      val hanch = headFeaturizer.anchor(tree.words)
      val danch = headFeaturizer.anchor(tree.words)
      for( (head, dep) <- tree.arcs if head < tree.words.length) {
        builder.add(hanch.featuresForWord(head),
          danch.featuresForWord(dep))
        error("Want both directions?")
        builder.add(danch.featuresForWord(head),
          hanch.featuresForWord(dep))
      }
    }

    val index = builder.result()

    new ProductBilexicalFeaturizer[W](headFeaturizer, depFeaturizer, index)
  }

}