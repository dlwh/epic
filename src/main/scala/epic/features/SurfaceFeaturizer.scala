package epic.features

import epic.framework.Feature
import epic.features.SurfaceFeaturizer.MarkerPos
import epic.features.WordFeaturizer.Modifier
import epic.trees.Span
import breeze.collection.mutable.TriangularArray

/**
 * TODO
 * @author dlwh
 */
trait SurfaceFeaturizer[W] extends Serializable {
  def anchor(words: IndexedSeq[W]):SurfaceFeatureAnchoring[W]

  def +(other: SurfaceFeaturizer[W]): SurfaceFeaturizer[W] = (this,other) match {
    case (MultiSurfaceFeaturizer(feats),MultiSurfaceFeaturizer(feats2)) => new MultiSurfaceFeaturizer(feats ++ feats2)
    case (MultiSurfaceFeaturizer(feats),_) => new MultiSurfaceFeaturizer(feats :+ other)
    case (_,MultiSurfaceFeaturizer(feats2)) => new MultiSurfaceFeaturizer(this +: feats2)
    case _ => new MultiSurfaceFeaturizer(this, other)
  }
  def *(other:SurfaceFeaturizer[W]) = new ProductSurfaceFeaturizer(this, other)
}

object SurfaceFeaturizer {

  def apply[W](f: (IndexedSeq[W], Span)=>Array[Feature]):SurfaceFeaturizer[W] = new TabulatedSurfaceFeaturizer[W](f)

  /** begin of span */
  object begin extends MarkerPos(0)
  /** end of span */
  object end extends MarkerPos(0, false)

  trait DSL {

    def whenLength[W](filt: Int=>Boolean)(f: SurfaceFeaturizer[W])= new LengthFilteredSurfaceFeaturizer(f, filt)
    def unitLengthSpan[W](f: WordFeaturizer[W]) = new SingleWordSpanFeaturizer(f)

    val begin : SurfaceFeaturizer.begin.type = SurfaceFeaturizer.begin
    val end : SurfaceFeaturizer.end.type = SurfaceFeaturizer.end

    def edges[W](first: MarkedWordFeaturizer[W], last: MarkedWordFeaturizer[W]):SurfaceFeaturizer[W] = new SpanEdgesFeaturizer(first, last)
    val spanShape = new SpanShapeFeaturizer()
    val length = new SpanLengthFeaturizer()
    val sent = new SentencePropertiesFeaturizer()

    implicit def _markerPosModifier[W]: WordFeaturizer.Modifier[W, MarkerPos, MarkedWordFeaturizer[W]] = new Modifier[W, MarkerPos, MarkedWordFeaturizer[W]] {
      def apply(f: WordFeaturizer[W], t: MarkerPos): MarkedWordFeaturizer[W] = new MarkedWordFeaturizer[W](f, t)
    }

    def spanBigrams[W](featurizer: WordFeaturizer[W], begin: MarkerPos, end: MarkerPos): SurfaceFeaturizer[W] = new BigramSurfaceFeaturizer[W](featurizer, begin, end)

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
      val loc2 = if (f1.wf eq f2.wf) loc1 else f2.wf.anchor(w)
      new SurfaceFeatureAnchoring[W] {
        def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
          val ffs1 = loc1.featuresForWord(f1.mp.toPos(begin, end))
          val ffs2 = loc2.featuresForWord(f2.mp.toPos(begin, end))
          epic.util.Arrays.crossProduct(ffs1, ffs2)(SpanEdgeFeature(f1.mp, f2.mp, _, _))
        }
      }

    }
  }

  case class LengthFilteredSurfaceFeaturizer[W](feat: SurfaceFeaturizer[W], f: Int=>Boolean) extends SurfaceFeaturizer[W] with Serializable {
    override def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
      val anch = feat.anchor(words)
      override def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        if (f(end - begin)) anch.featuresForSpan(begin, end) else Array.empty
      }
    }
  }

  case class SingleWordSpanFeaturizer[W](feat: WordFeaturizer[W]) extends SurfaceFeaturizer[W] with Serializable {
    override def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
      val anch = feat.anchor(words)
      override def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
        if (end == begin + 1) anch.featuresForWord(begin) else Array.empty
      }
    }
  }

  case class BigramSurfaceFeaturizer[W](f1: WordFeaturizer[W], b: MarkerPos, e: MarkerPos) extends SurfaceFeaturizer[W] {
    def anchor(w: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = {
      val loc1 = f1.anchor(w)
      new SurfaceFeatureAnchoring[W] {
        def featuresForSpan(begin: Int, end: Int): Array[Feature] = {
          val bb = b.toPos(begin, end)
          val ee = e.toPos(begin, end)
          val res = for(i <- bb until ee - 1) yield {
            val ffs1 = loc1.featuresForWord(i)
            val ffs2 = loc1.featuresForWord(i+1)
            epic.util.Arrays.crossProduct(ffs1, ffs2)(BigramFeature(0, _ ,_).asInstanceOf[Feature])
          }

          epic.util.Arrays.concatenate(res:_*)
        }
      }

    }
  }

  case class MarkerPos(offset: Int, relativeToBegin: Boolean = true) {
    def apply(i: Int) = copy(offset + i)
    def +(i: Int) = apply(i)
    def -(i: Int) = apply(-i)

    def toPos(begin: Int, end: Int) = if (relativeToBegin) begin + offset else end + offset

    override def toString =  s"(${if (relativeToBegin) "b" else "e"}${if (offset == 0) "" else if (offset > 0) "+" + offset else offset})"
  }

  class TabulatedSurfaceFeaturizer[W](f: (IndexedSeq[W], Span)=>Array[Feature]) extends SurfaceFeaturizer[W] {
    override def anchor(words: IndexedSeq[W]): SurfaceFeatureAnchoring[W] = new SurfaceFeatureAnchoring[W] {
      val tab = TriangularArray.tabulate(words.length + 1) { (begin, end) => f(words, Span(begin, end)) }
      override def featuresForSpan(begin: Int, end: Int): Array[Feature] = tab(begin, end)
    }
  }

}

case class BigramFeature(offset: Int, prev: Feature, next: Feature) extends Feature
case class SpanRelativeFeature(f: Feature, mp: MarkerPos) extends Feature
case class SpanEdgeFeature(mp1: MarkerPos, mp2: MarkerPos, f1: Feature, f2: Feature) extends Feature

