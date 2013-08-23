package epic.features

import epic.framework.Feature
import breeze.util.Index
import epic.trees.BinaryTree
import epic.trees.TreeInstance
import epic.features.WordFeaturizer.Modifier
import epic.features.SplitSpanFeaturizer.{ProductSplitSpanFeaturizer, SumSplitSpanFeaturizer}
import epic.util.Arrays
import epic.features.SurfaceFeaturizer.MarkerPos

/**
 * TODO
 *
 * @author dlwh
 **/
trait SplitSpanFeaturizer[W] extends SurfaceFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):SplitSpanFeatureAnchoring[W]

  def *(other: SplitSpanFeaturizer[W]) = (this,other) match {
    case _ => new ProductSplitSpanFeaturizer(this, other)
  }

  override def +(other: SurfaceFeaturizer[W]):SplitSpanFeaturizer[W] = (this,other) match {
    case (SumSplitSpanFeaturizer(as),SumSplitSpanFeaturizer(bs)) => SumSplitSpanFeaturizer(as ++ bs)
    case (a,SumSplitSpanFeaturizer(bs)) => SumSplitSpanFeaturizer(a +: bs)
    case (SumSplitSpanFeaturizer(as),b) => SumSplitSpanFeaturizer(as :+ SplitSpanFeaturizer.liftSurfaceFeaturizerToSplitSpan(b))
    case _ => SumSplitSpanFeaturizer(IndexedSeq(this, other))
  }
}


object SplitSpanFeaturizer {
  trait DSL extends SurfaceFeaturizer.DSL {
    object split extends SplitPointMarker
    implicit def splitWFModifier[W]: WordFeaturizer.Modifier[W, split.type, SplitFeaturizer[W]] = new Modifier[W, split.type, SplitFeaturizer[W]] {
      def apply(f: WordFeaturizer[W], t: split.type): SplitFeaturizer[W] = new SplitFeaturizer(f)
    }

    def distance[W](b: MarkerPos, s: split.type, db: DistanceBinner = DistanceBinner()):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](b, s, db)
    }

    def distance[W](s: split.type, b: MarkerPos):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](s, b, DistanceBinner())
    }

    def distance[W](m: MarkerPos, n: MarkerPos):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](m, n, DistanceBinner())
    }

  }

  implicit def liftSurfaceFeaturizerToSplitSpan[W](surface: SurfaceFeaturizer[W]):SplitSpanFeaturizer[W] = surface match {
    case x: SplitSpanFeaturizer[W] => x
    case _ =>
      new SplitSpanFeaturizer[W] {
        def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
          val anch = surface.anchor(w)
          def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = emptyArray

          def featuresForSpan(begin: Int, end: Int): Array[Feature] = anch.featuresForSpan(begin, end)
        }
      }
  }


  class SplitSpanDistanceFeaturizer[W] private[SplitSpanFeaturizer](a: Any, b: Any, db: DistanceBinner = DistanceBinner()) extends SplitSpanFeaturizer[W] {
    val label = s"$a <-> $b"
    private val theSplitNeedingAnchoring = new SplitSpanFeatureAnchoring[W] with Serializable {
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        val lhs = markerToPos(a, begin, end, split)
        val rhs = markerToPos(b, begin, end, split)
        Array(DistanceFeature(db.binnedDistance(lhs, rhs), label))
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
    }

    private val theNotSplitNeedingAnchoring = new SplitSpanFeatureAnchoring[W] with Serializable {
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        emptyArray
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val lhs = markerToPos(a, begin, end, -1)
        val rhs = markerToPos(b, begin, end, -1)
        Array(DistanceFeature(db.binnedDistance(lhs, rhs), label))
      }
    }

    private def markerToPos(a: Any, begin: Int, end: Int, split: Int): Int = {
      a match {
        case MarkerPos(i, true) => begin + i
        case MarkerPos(i, false) => end + i
        case _: SplitPointMarker => split
        case _ => error("....")
      }
    }

    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = {
      if(a.isInstanceOf[SplitPointMarker] || b.isInstanceOf[SplitPointMarker])
        theSplitNeedingAnchoring
      else
        theNotSplitNeedingAnchoring
    }
  }

  case class SplitFeaturizer[W](f: WordFeaturizer[W]) extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      val wf = f.anchor(w)
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        wf.featuresForWord(split).map(SplitFeature)
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
    }

  }

  case class SumSplitSpanFeaturizer[W](prods: IndexedSeq[SplitSpanFeaturizer[W]]) extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      val anchs = prods.map(_.anchor(w)).toArray
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = anchs.flatMap(_.featuresForSpan(begin, end))

      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        anchs.flatMap(_.featuresForSplit(begin, split, end))
      }
    }
  }

  case class ProductSplitSpanFeaturizer[W](a: SplitSpanFeaturizer[W], b: SplitSpanFeaturizer[W]) extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      val aa = a.anchor(w)
      val ba = b.anchor(w)
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        Arrays.crossProduct(aa.featuresForSpan(begin, end), ba.featuresForSpan(begin, end))(CrossProductFeature(_, _))
      }

      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        val aSplit: Array[Feature] = aa.featuresForSplit(begin, split, end)
        val bSplit: Array[Feature] = ba.featuresForSplit(begin, split, end)
        val aSpan: Array[Feature] = aa.featuresForSpan(begin, end)
        val bSpan: Array[Feature] = ba.featuresForSpan(begin, end)
        Arrays.concatenate[Feature](
          Arrays.crossProduct(aSplit, bSpan)(CrossProductFeature(_, _, "Split")),
          Arrays.crossProduct(aSplit, bSplit)(CrossProductFeature(_, _, "Split")),
          Arrays.crossProduct(aSpan, bSplit)(CrossProductFeature(_, _, "Split"))
        )
      }
    }
  }
  sealed trait SplitPointMarker extends  Serializable { override def toString = "split"}
  private val emptyArray = Array.empty[Feature]
}

trait SplitSpanFeatureAnchoring[W] extends SurfaceFeatureAnchoring[W] {
  def featuresForSplit(begin: Int, split: Int, end: Int):Array[Feature]
}


trait IndexedSplitSpanFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):IndexedSplitSpanFeatureAnchoring[W]
  def featureIndex: Index[Feature]
}

trait IndexedSplitSpanFeatureAnchoring[W]  extends IndexedSurfaceAnchoring[W] {
  def featuresForSplit(begin: Int, split: Int, end: Int):Array[Int]
}

object IndexedSplitSpanFeaturizer {
  def fromData[L, W](f: SplitSpanFeaturizer[W],
                     trees: IndexedSeq[TreeInstance[L, W]],
                     hashFeatures: HashFeature.Scale = HashFeature.Relative(1.0)):IndexedSplitSpanFeaturizer[W] =  {
    val index = Index[Feature]()
    for (ti <- trees) {
      val wspec = f.anchor(ti.words)
      ti.tree.allChildren.foreach {
        case t@BinaryTree(a, b, c, span) =>
          wspec.featuresForSpan(span.begin, span.end).foreach(index.index)
          wspec.featuresForSplit(span.begin, t.splitPoint, span.end).foreach(index.index)
        case _ =>
      }
    }

    new BasicIndexedSplitSpanFeaturizer(f, new HashExtendingIndex(index, HashFeature(_), hashFeatures))
  }

  class BasicIndexedSplitSpanFeaturizer[W](f: SplitSpanFeaturizer[W], val featureIndex: Index[Feature]) extends IndexedSplitSpanFeaturizer[W] with Serializable {
    def anchor(w: IndexedSeq[W]): IndexedSplitSpanFeatureAnchoring[W] = {
      val anc = f.anchor(w)

      new IndexedSplitSpanFeatureAnchoring[W] {
        def featuresForSplit(begin: Int, split: Int, end: Int): Array[Int] = anc.featuresForSplit(begin, split, end).map(featureIndex)

        def words: IndexedSeq[W] = w

        def featuresForSpan(begin: Int, end: Int): Array[Int] = anc.featuresForSpan(begin, end).map(featureIndex)
      }
    }
  }
}

case class SplitFeature(x: Feature) extends Feature
