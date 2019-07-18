package epic.features

import epic.framework.Feature
import breeze.util.Index
import epic.trees.BinaryTree
import epic.trees.TreeInstance
import epic.features.WordFeaturizer.Modifier
import epic.features.SplitSpanFeaturizer.{ProductSplitSpanFeaturizer, SumSplitSpanFeaturizer}
import epic.util.{AlwaysSeenSet, ThreadLocalBloomFilter, Arrays}
import epic.features.SurfaceFeaturizer.MarkerPos
import scala.collection.mutable.ArrayBuffer

/**
 * TODO
 *
 * @author dlwh
 **/
trait SplitSpanFeaturizer[W] extends SurfaceFeaturizer[W] {
  def anchor(w: IndexedSeq[W]):SplitSpanFeatureAnchoring[W]

  def *(other: SplitSpanFeaturizer[W]) = (this,other) match {
    case (a@SumSplitSpanFeaturizer(as, x),b@SumSplitSpanFeaturizer(bs, y)) => ProductSplitSpanFeaturizer(a, b, y, x)
    case (a,b@SumSplitSpanFeaturizer(bs, x)) => ProductSplitSpanFeaturizer(a, b, x, false)
    case (a@SumSplitSpanFeaturizer(as, x),b) => ProductSplitSpanFeaturizer(a, b, false, x)
    case _ => ProductSplitSpanFeaturizer(this, other)
  }

  override def +(other: SurfaceFeaturizer[W]):SplitSpanFeaturizer[W] = (this,other) match {
    case (SumSplitSpanFeaturizer(as, x),SumSplitSpanFeaturizer(bs, y)) => SumSplitSpanFeaturizer(as ++ bs, x || y)
    case (SumSplitSpanFeaturizer(as, x),b) => SumSplitSpanFeaturizer(as :+ SplitSpanFeaturizer.liftSurfaceFeaturizerToSplitSpan(b), x)
    case (a,SumSplitSpanFeaturizer(bs, x)) => SumSplitSpanFeaturizer(a +: bs, x)
    case _ => SumSplitSpanFeaturizer(IndexedSeq(this, other))
  }

  def +(unit: SplitSpanFeaturizer.unit.type) = new SumSplitSpanFeaturizer(IndexedSeq(this), true)
}


object SplitSpanFeaturizer {
  object split extends SplitPointMarker

  trait DSL extends SurfaceFeaturizer.DSL {
    val split: SplitSpanFeaturizer.split.type = SplitSpanFeaturizer.split

    implicit def splitWFModifier[W]: WordFeaturizer.Modifier[W, split.type, SplitFeaturizer[W]] = new Modifier[W, split.type, SplitFeaturizer[W]] {
      def apply(f: WordFeaturizer[W], t: split.type): SplitFeaturizer[W] = new SplitFeaturizer(f)
    }

    def zeroSplit[W] = new ZeroSplitSpanFeaturizer[W]

    def distance[W](b: MarkerPos, s: split.type, db: DistanceBinner = DistanceBinner()):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](b, s, db)
    }

    def distance[W](s: split.type, b: MarkerPos):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](s, b, DistanceBinner())
    }

    def distance[W](m: MarkerPos, n: MarkerPos):SplitSpanFeaturizer[W] = {
      new SplitSpanDistanceFeaturizer[W](m, n, DistanceBinner())
    }

    def relativeLength[W]:SplitSpanFeaturizer[W] = {
      new RelativeLengthFeaturizer[W]()
    }

    val splitSpanShape = new SplitSpanShapeFeaturizer()

    def distanceToSentenceBoundaries[W] = new DistanceToSentenceBoundariesFeaturizer[W]


    val unit: SplitSpanFeaturizer.unit.type = SplitSpanFeaturizer.unit
  }

  case object unit

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

  class SplitSpanShapeFeaturizer extends SplitSpanFeaturizer[String] {
    def anchor(w: IndexedSeq[String]): SplitSpanFeatureAnchoring[String] = new SplitSpanFeatureAnchoring[String] {
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        Array(SplitShapeFeature(SpanShapeGenerator.splitShapeFor(w, begin, split, end)))
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = Array.empty
    }
  }

  case class SplitShapeFeature(shape: String) extends Feature

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
        case _ => ???
      }
    }

    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = {
      if (a.isInstanceOf[SplitPointMarker] || b.isInstanceOf[SplitPointMarker])
        theSplitNeedingAnchoring
      else
        theNotSplitNeedingAnchoring
    }
  }

  class DistanceToSentenceBoundariesFeaturizer[W] extends SurfaceFeaturizer[W] {
    val db: DistanceBinner = DistanceBinner()
    def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = Array(DistToBOSFeature(db.binnedDistance(begin)), DistToEOSFeature(db.binnedDistance(end, words.length)))
    }
  }

  case class DistToBOSFeature(dist: Int) extends Feature
  case class DistToEOSFeature(dist: Int) extends Feature

  /**
   * Returns the binned difference between the [begin,split) and [split,end) spans.
   * @param db
   * @tparam W
   */
  class RelativeLengthFeaturizer[W] private[SplitSpanFeaturizer](db: DistanceBinner = DistanceBinner()) extends SplitSpanFeaturizer[W] {
    val label = s"RelativeDifference"
    private val theSplitNeedingAnchoring = new SplitSpanFeatureAnchoring[W] with Serializable {
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        Array(DistanceFeature(db.binnedDistance((end-split) - (split-begin)), label))
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
    }

    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = {
        theSplitNeedingAnchoring
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

  case class ZeroSplitSpanFeaturizer[W]() extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        emptyArray
      }

      def featuresForSpan(begin: Int, end: Int): Array[Feature] = emptyArray
    }

  }

  case class SumSplitSpanFeaturizer[W](prods: IndexedSeq[SplitSpanFeaturizer[W]], unitized: Boolean = false) extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      val anchs = prods.map(_.anchor(w)).toArray
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = anchs.flatMap(_.featuresForSpan(begin, end))

      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        Arrays.concatenate(anchs.map(_.featuresForSplit(begin, split, end)):_*)
      }
    }

    override def +(unit: SplitSpanFeaturizer.unit.type) = copy(unitized = true)
  }

  case class ProductSplitSpanFeaturizer[W](a: SplitSpanFeaturizer[W], b: SplitSpanFeaturizer[W], keepJustA: Boolean = false, keepJustB: Boolean = false) extends SplitSpanFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SplitSpanFeatureAnchoring[W] = new SplitSpanFeatureAnchoring[W] {
      val aa = a.anchor(w)
      val ba = b.anchor(w)
      def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        val afeats: Array[Feature] = aa.featuresForSpan(begin, end)
        val bfeats: Array[Feature] = ba.featuresForSpan(begin, end)
        val cross:Array[Feature] = Arrays.crossProduct(afeats, bfeats)(CrossProductFeature(_, _))
        if (keepJustA && keepJustB) {
          Arrays.concatenate[Feature](cross, afeats, bfeats)
        } else if (keepJustA) {
          Arrays.concatenate[Feature](cross, afeats)
        } else if (keepJustB) {
          Arrays.concatenate[Feature](cross, bfeats)
        } else {
          cross
        }

      }

      def featuresForSplit(begin: Int, split: Int, end: Int): Array[Feature] = {
        val aSplit: Array[Feature] = aa.featuresForSplit(begin, split, end)
        val bSplit: Array[Feature] = ba.featuresForSplit(begin, split, end)
        val aSpan: Array[Feature] = aa.featuresForSpan(begin, end)
        val bSpan: Array[Feature] = ba.featuresForSpan(begin, end)
        val results = ArrayBuffer[Array[Feature]](
          Arrays.crossProduct(aSplit, bSpan)(CrossProductFeature(_, _, "Split")),
          Arrays.crossProduct(aSplit, bSplit)(CrossProductFeature(_, _, "Split")),
          Arrays.crossProduct(aSpan, bSplit)(CrossProductFeature(_, _, "Split"))
        )

        if (keepJustA) {
          results += aSplit
        }
        if (keepJustB) {
          results += bSplit
        }

        Arrays.concatenate(results:_*)
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
                     hashFeatures: HashFeature.Scale = HashFeature.Relative(1.0),
                     bloomFilter: Boolean = false,
                     deduplicateFeatures: Boolean = false):IndexedSplitSpanFeaturizer[W] =  {
    def seenSet =  if (bloomFilter) new ThreadLocalBloomFilter[Long](8 * 1024 * 1024 * 50, 3) else AlwaysSeenSet
    val builder = if (deduplicateFeatures) new NonRedundantIndexBuilder[Feature] else new NormalIndexBuilder[Feature]
    for (ti <- trees) {
      val wspec = f.anchor(ti.words)
      ti.tree.allChildren.foreach {
        case t@BinaryTree(a, b, c, span) =>
          builder.add(wspec.featuresForSpan(span.begin, span.end) ++ wspec.featuresForSplit(span.begin, t.splitPoint, span.end))
        case t =>
          builder.add(wspec.featuresForSpan(t.span.begin, t.span.end))
      }
    }

    val index = builder.result()

    new BasicIndexedSplitSpanFeaturizer(f, if (hashFeatures.numFeatures(index.size) != 0) new HashExtendingIndex(index, HashFeature(_), hashFeatures, seenSet) else index)
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
