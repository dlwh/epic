package epic.features

import epic.framework.Feature
import epic.trees.DependencyTree
import epic.features.BilexicalFeaturizer._
import epic.features.WordFeaturizer.Modifier
import epic.util.Arrays
import epic.features.BilexicalFeaturizer.HeadFeaturizer
import epic.features.BilexicalFeaturizer.DepFeaturizer
import epic.features.BilexicalFeaturizer.SumBilexicalFeaturizer
import epic.features.BilexicalFeaturizer.HeadDepFeaturizer
import breeze.util.Index


/**
 * TODO
 *
 * @author dlwh
 **/
trait BilexicalFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):BilexicalFeatureAnchoring[W]

  def *(other: BilexicalFeaturizer[W]) = (this,other) match {
    case (x:HeadFeaturizer[W], y: DepFeaturizer[W]) => new HeadDepFeaturizer(x.base,y.base)
    case (y: DepFeaturizer[W], x:HeadFeaturizer[W]) => new HeadDepFeaturizer(x.base,y.base)
    case _ => new ProductBilexicalFeaturizer(this,other)
  }

  def +(other: BilexicalFeaturizer[W]):BilexicalFeaturizer[W] = (this,other) match {
    case (SumBilexicalFeaturizer(as),SumBilexicalFeaturizer(bs)) => SumBilexicalFeaturizer(as ++ bs)
    case (a,SumBilexicalFeaturizer(bs)) => SumBilexicalFeaturizer(a +: bs)
    case (SumBilexicalFeaturizer(as),b) => SumBilexicalFeaturizer(as :+ b)
    case _ => SumBilexicalFeaturizer(IndexedSeq(this, other))
  }
}

object BilexicalFeaturizer {
  trait DSL {
    object head
    object dep

    def bilex[W](f: WordFeaturizer[W]):BilexicalFeaturizer[W] = bilex(f, f)
    def bilex[W](head: WordFeaturizer[W], dep: WordFeaturizer[W]):BilexicalFeaturizer[W] = HeadDepFeaturizer(head, dep)

    lazy val distance = new BilexicalFeaturizer.DistanceFeaturizer[String]()
    def adaptSpanFeaturizer[W](f: SurfaceFeaturizer[W]) = new BilexicalFeaturizer.AdaptedSurfaceFeaturizer[W](f)

    def withDistance[W](f: BilexicalFeaturizer[W], db: DistanceBinner = new DistanceBinner()) = new BinomialFeaturizer(f, new DistanceFeaturizer(db))

    implicit def headWFModifier[W]: WordFeaturizer.Modifier[W, head.type, HeadFeaturizer[W]] = new Modifier[W, head.type, HeadFeaturizer[W]] {
      def apply(f: WordFeaturizer[W], t: head.type): HeadFeaturizer[W] = new HeadFeaturizer(f)
    }

    implicit def depWFModifier[W]: WordFeaturizer.Modifier[W, dep.type, DepFeaturizer[W]] = new Modifier[W, dep.type, DepFeaturizer[W]] {
      def apply(f: WordFeaturizer[W], t: dep.type): DepFeaturizer[W] = new DepFeaturizer(f)
    }
  }

  case class SumBilexicalFeaturizer[W](prods: IndexedSeq[BilexicalFeaturizer[W]]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val anchs = prods.map(_.anchor(w)).toArray
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = anchs.flatMap(_.featuresForAttachment(head, dep))
    }
  }

  case class AdaptedSurfaceFeaturizer[W](base: SurfaceFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val ba = base.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = {
        if (head < dep) ba.featuresForSpan(head, dep)
        else ba.featuresForSpan(dep, head)
      }
    }
  }

  case class HeadFeaturizer[W](base: WordFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val ba = base.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = ba.featuresForWord(head).map(f => HeadFeature(f):Feature)
    }
  }

  case class DepFeaturizer[W](base: WordFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val ba = base.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = ba.featuresForWord(dep).map(f => DepFeature(f):Feature)
    }
  }

  case class BinomialFeaturizer[W](headBase: BilexicalFeaturizer[W], depBase: BilexicalFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val hb = headBase.anchor(w)
      val db = if (headBase eq depBase) hb else depBase.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = {
        val hf = hb.featuresForAttachment(head, dep)
        val df = db.featuresForAttachment(head, dep)
        val cross = Arrays.crossProduct(hf, df)((a, b) => CrossProductFeature(a,b):Feature)
        Arrays.concatenate(hf, df, cross)
      }
    }
  }

  case class HeadDepFeaturizer[W](headBase: WordFeaturizer[W], depBase: WordFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val hb = headBase.anchor(w)
      val db = if (headBase eq depBase) hb else depBase.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = {
        Arrays.crossProduct(hb.featuresForWord(head), db.featuresForWord(dep))((a, b) => HeadDepFeature(a,b):Feature)
      }
    }
  }

  case class ProductBilexicalFeaturizer[W](a: BilexicalFeaturizer[W], b: BilexicalFeaturizer[W]) extends BilexicalFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = new BilexicalFeatureAnchoring[W] {
      val aa = a.anchor(w)
      val ba = b.anchor(w)
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = {
        Arrays.crossProduct(aa.featuresForAttachment(head, dep), ba.featuresForAttachment(head, dep))(CrossProductFeature(_, _, "BilexCross"))
      }
    }
  }

  case class DistanceFeaturizer[W](db: DistanceBinner = DistanceBinner()) extends BilexicalFeaturizer[W] {
    private val theAnchoring = new BilexicalFeatureAnchoring[W] with Serializable {
      def featuresForAttachment(head: Int, dep: Int): Array[Feature] = Array(DistanceFeature(db.binnedDistance(head, dep)))
    }
    def anchor(w: IndexedSeq[W]): BilexicalFeatureAnchoring[W] = theAnchoring
  }
}

trait BilexicalFeatureAnchoring[W] {
  def featuresForAttachment(head: Int, dep: Int):Array[Feature]
}

@SerialVersionUID(1L)
class ProductIndexedBilexicalFeaturizer[W](headFeaturizer: IndexedWordFeaturizer[W],
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
  def featureIndex: Index[Feature]
}

trait IndexedBilexicalFeatureAnchoring[W]  {
  def featuresForAttachment(head: Int, dep: Int):Array[Int]
}

object IndexedBilexicalFeaturizer {
  def fromData[L, W](f: BilexicalFeaturizer[W],
                     depTrees: IndexedSeq[DependencyTree[L, W]],
                     hashFeatures: HashFeature.Scale = HashFeature.Relative(1.0)):IndexedBilexicalFeaturizer[W] =  {
    val index = Index[Feature]()
    for (tree <- depTrees) {
      val anch = f.anchor(tree.words)
      for( (head, dep) <- tree.arcs if head < tree.words.length) {
        anch.featuresForAttachment(head, dep) foreach index.index
      }
    }

    new BasicIndexedBilexicalFeaturizer(f, new HashExtendingIndex(index, HashFeature(_), hashFeatures))
  }

  class BasicIndexedBilexicalFeaturizer[W](f: BilexicalFeaturizer[W], val featureIndex: Index[Feature]) extends IndexedBilexicalFeaturizer[W] with Serializable {

    def anchor(w: IndexedSeq[W]): IndexedBilexicalFeatureAnchoring[W] = {
      val anc = f.anchor(w)

      new IndexedBilexicalFeatureAnchoring[W] {
        def featuresForAttachment(head: Int, dep: Int): Array[Int] = anc.featuresForAttachment(head, dep).map(featureIndex)
      }
    }
  }

  /*
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
  //      builder.add(danch.featuresForWord(head),
  //      hanch.featuresForWord(dep))
      }
    }

    val index = builder.result()

    new ProductIndexedBilexicalFeaturizer[W](headFeaturizer, depFeaturizer, index)
  }
  */

}

trait LexFeature extends Feature

case class HeadFeature[P](r: Feature) extends LexFeature

case class DepFeature[P](r: Feature) extends LexFeature
case class HeadDepFeature[P](head: Feature, dep: Feature) extends LexFeature
case class DistFeature(distance: Int, f: Any) extends LexFeature
case class DistanceFeature(distance: Int, label: String = "Span")extends LexFeature

case class AttachRight(distance: Int) extends LexFeature
case object AttachRight extends LexFeature
case class AttachLeft(distance: Int) extends LexFeature
case object AttachLeft extends LexFeature