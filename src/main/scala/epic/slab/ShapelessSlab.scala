package epic.slab
import epic.slab.Slab._
import epic.trees.Span
import shapeless._
import LUBConstraint._
import ops.hlist.Prepend

trait Annotation

class InefficientShapelessSlab[ContentType, RegionType, AnnotationTypes, L <: HList: <<:[Annotation]#λ](
  val content: ContentType,
  val annotations: L = HNil)(implicit hasBounds: HasBounds[RegionType], extract: ExtractRegion[RegionType, ContentType])
// extends Slab[ContentType, RegionType, AnnotationTypes]
{

  def ++[A <: Annotation, H <: HList: <<:[A]#λ, Out <: HList: <<:[Annotation]#λ]
  (annotations: H)
  (implicit prepend : Prepend.Aux[L, H, Out]): InefficientShapelessSlab[ContentType, RegionType, AnnotationTypes with A, Out] = {
    new InefficientShapelessSlab(this.content, this.annotations ::: annotations)
  }

  def +:[A <: Annotation](annotation: A): InefficientShapelessSlab[ContentType, RegionType, AnnotationTypes with A, A :: L] = {
    new InefficientShapelessSlab(this.content, annotation +: this.annotations)
  }

  def toHList: HList = this.annotations

}


// class SortedShapelessSlab[ContentType,
//                           AnnotationType](val content: ContentType,
//                                           val annotations: Map[Class[_], Vector[(Span, Any)]] = Map.empty,
//                                           val reverseAnnotations: Map[Class[_], Vector[(Span, Any)]] = Map.empty)(implicit extract: ExtractRegion[Span, ContentType]) extends Slab[ContentType, Span, AnnotationType] {


//   override def spanned(region: Span): ContentType = extract(region, content)

//   override def ++[A](annotations: TraversableOnce[(Span, A)]): Slab[ContentType, Span, AnnotationType with A] = {
//     var newAnnotations = this.annotations
//     val grouped = annotations.toIndexedSeq.groupBy(_._2.getClass)
//     for( (clss, group) <- grouped) {
//       newAnnotations = newAnnotations + (clss -> (newAnnotations.getOrElse(clss, Vector.empty) ++ group).sortBy(_._1)(SpanOrdering))
//     }

//     var reverseAnnotations = this.reverseAnnotations
//     for( (clss, group) <- grouped) {
//       reverseAnnotations = reverseAnnotations + (clss -> (reverseAnnotations.getOrElse(clss, Vector.empty) ++ group).sortBy(_._1)(EndFirstSpanOrdering))
//     }
//     new SortedSequenceSlab(content, newAnnotations, reverseAnnotations)
//   }

//   override def following[A >: AnnotationType: ClassTag](region: Span): Iterator[(Span, A)] = {
//     annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
//       var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.end)
//       if(pos < 0) pos = ~pos
//       annotations.view(pos, annotations.length)
//     }.asInstanceOf[Iterator[(Span, A)]]
//   }

//   override def preceding[A >: AnnotationType : ClassTag](region: Span): Iterator[(Span, A)] = {
//     reverseAnnotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
//       var pos = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.end, region.begin + 1)
//       if(pos < 0) pos = ~pos
//       annotations.view(0, pos).reverseIterator
//     }.asInstanceOf[Iterator[(Span, A)]]
//   }

//   override def covered[A >: AnnotationType : ClassTag](region: Span): Iterator[(Span, A)] = {
//     annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatMap { annotations =>
//       var begin = BinarySearch.interpolationSearch(annotations, (_:(Span, Any))._1.begin, region.begin)
//       if(begin < 0) begin = ~begin
//       annotations.view(begin, annotations.length).takeWhile(_._1.end <= region.end)
//     }.asInstanceOf[Iterator[(Span, A)]]
//   }

//   override def iterator[A >: AnnotationType : ClassTag]: Iterator[(Span, A)] = {
//     annotations.filterKeys(implicitly[ClassTag[A]].runtimeClass.isAssignableFrom).valuesIterator.flatten.asInstanceOf[Iterator[(Span, A)]]
//   }
// }
