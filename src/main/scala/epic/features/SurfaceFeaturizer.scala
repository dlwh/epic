package epic.features

import epic.framework.Feature
import epic.features.SurfaceFeaturizer.MarkerPos
import epic.parser.features.StandardSpanFeatures.SpanLengthFeature

/**
 * TODO
 * @author dlwh
 */
trait SurfaceFeaturizer[W] {
  def anchor(words: IndexedSeq[W]):SurfaceFeatureAnchoring[W]

  def +(other: SurfaceFeaturizer[W]) = (this,other) match {
    case (MultiSurfaceFeaturizer(feats),MultiSurfaceFeaturizer(feats2)) => new MultiSurfaceFeaturizer(feats ++ feats2)
    case (MultiSurfaceFeaturizer(feats),_) => new MultiSurfaceFeaturizer(feats :+ other)
    case (_,MultiSurfaceFeaturizer(feats2)) => new MultiSurfaceFeaturizer(this +: feats2)
    case _ => new MultiSurfaceFeaturizer(this, other)
  }
  def *(other:SurfaceFeaturizer[W]) = new ProductSurfaceFeaturizer(this, other)
}

object SurfaceFeaturizer {
  class DSL() {

    /** begin of span */
    object b extends MarkerPos(0)
    /** end of span */
    object e extends MarkerPos(0, false)

    def edges[W](first: MarkedWordFeaturizer[W], last: MarkedWordFeaturizer[W]):SurfaceFeaturizer[W] = new SpanEdgesFeaturizer(first, last)
    val spanShape = new SpanShapeFeaturizer()
    val length = new SpanLengthFeaturizer()
    val sent = new SentencePropertiesFeaturizer()

    implicit class LiftWordFeaturizer[W](wf: WordFeaturizer[W]) {
    }
  }

  case class MarkedWordFeaturizer[W](wf: WordFeaturizer[W], mp: MarkerPos) extends SurfaceFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = {
      val loc = wf.anchor(w)
      new SurfaceFeatureAnchoring[W] {
        def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
          loc.featuresForWord(mp.toPos(begin, end)).map(SpanRelativeFeature(_, mp))
        }
      }

    }
  }

  case class SpanEdgesFeaturizer[W](f1: MarkedWordFeaturizer[W], f2: MarkedWordFeaturizer[W]) extends SurfaceFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = {
      val loc1 = f1.wf.anchor(w)
      val loc2 = f2.wf.anchor(w)
      new SurfaceFeatureAnchoring[W] {
        def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
          val ffs1 = loc1.featuresForWord(f1.mp.toPos(begin, end))
          val ffs2 = loc2.featuresForWord(f2.mp.toPos(begin, end))
          epic.util.Arrays.crossProduct(ffs1, ffs2)(SpanEdgeFeature(f1.mp, f2.mp, _, _))
        }
      }

    }
  }

  case class MarkerPos(offset: Int, relativeToBegin: Boolean = true) {
    def apply(i: Int) = copy(offset + i)
    def +(i: Int) = apply(i)
    def -(i: Int) = apply(-i)

    def toPos(begin: Int, end: Int) = if(relativeToBegin) begin + offset else end + offset

    override def toString =  s"(${if(relativeToBegin) "b" else "e"}${if(offset == 0) "" else if(offset > 0) "+" + offset else offset})"
  }

}

case class SpanRelativeFeature(f: Feature, mp: MarkerPos) extends Feature
case class SpanEdgeFeature(mp1: MarkerPos, mp2: MarkerPos, f1: Feature, f2: Feature) extends Feature

