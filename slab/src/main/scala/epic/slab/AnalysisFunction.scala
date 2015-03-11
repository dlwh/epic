package epic.slab
import epic.slab.typeclasses._
import shapeless._
import ops.hlist._

/**
 * An AnalysisFunction that takes a Slab with declared annotation
 * types in it and outputs a new Slab with additional annotations of
 * new types. All annotators should inherit from this trait.
 *
 * Documentation for the type variables:
 *   C = Content type, most likely String
 *   I = Required annotation types, as an HList of Vector
 *   O = Produced annotation types, as an HList of Vector
 */
trait AnalysisFunction[C, I <: HList, O <: HList] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: SelectMany.Aux[In, I, I], add: AddMany.Aux[In, O, Out]): Slab[C, Out] = {
    slab.add(apply(slab.content, sel(slab.annotations)))(add)
  }
  def apply(content: C, in: I): O
}

// This analysis function requires one input and adds one output type.
trait AnalysisFunction11[C, I, O] extends AnalysisFunction[C, Vector[I] :: HNil, Vector[O] :: HNil] {
  override def apply(content: C, input: Vector[I] :: HNil): Vector[O] :: HNil = {
    apply(content, input.select[Vector[I]]).toVector :: HNil
  }
  def apply(content: C, input: Vector[I]): Iterable[O]
}

// Convert a function of the correct signature to an AnalysisFunction11.
object AnalysisFunction11 {
  def apply[C, I, O](fun: ((C, Vector[I]) => Iterable[O])): AnalysisFunction11[C, I, O] = new AnalysisFunction11[C, I, O] {
    def apply(content: C, input: Vector[I]): Iterable[O] = fun(content, input)
  }
}

// This analysis function takes N input types and adds one output
// type. Does not use the composite pattern because then the passing
// of the implicits to extract the elements from the HList gets
// complicated. For an example, take a look at epic.sequences.Segmenter
trait AnalysisFunctionN1[C, I <: HList, O] extends AnalysisFunction[C, I, Vector[O] :: HNil] {
  def apply(content: C, in: I): Vector[O] :: HNil
}

// This analysis function requires no input types except for the
// content itself.  No input types required, so functions extending
// this trait can be used to construct a new slab directly from
// content.
trait AnalysisFunction01[C, O] extends AnalysisFunction[C, HNil, Vector[O] :: HNil] {
  override def apply(content: C, in: HNil): Vector[O] :: HNil = {
    apply(content).toVector :: HNil
  }
  def apply(content: C): Iterable[O]

  def slabFrom(content: C): Slab[C, Vector[O] :: HNil] = {
    Slab(content, apply(content).toVector :: HNil)
  }
}

// Convert a function of the correct signature to an AnalysisFunction01.
object AnalysisFunction01 {
  def apply[C, O](fun: (C => Iterable[O])): AnalysisFunction01[C, O] = new AnalysisFunction01[C, O] {
    def apply(content: C): Iterable[O] = fun(content)
  }
}

