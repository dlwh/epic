package epic.slab
import epic.slab.typeclasses._
import shapeless._
import ops.hlist._

/**
 * An analysis function that takes a Slab with declared annotation types in it and outputs
 * a new Slab with additional annotations of a new type.
 *
 * Documentation for the type variables:
 *   C = Content type
 *   I = Required annotation type
 *   O = Produced annotation type
 */

// This analysis function requires one input and adds one output type.
class AnalysisFunction11[C, I, O](val fun: (C, List[I]) => Iterable[O]) {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: Selector[In, List[I]], adder: Adder.Aux[In, List[O], Out]): Slab[C, Out] = {
    slab.add(fun(slab.content, slab.select(sel)).toList)
  }
}

// Convert a function of the correct signature to an AnalysisFunction11.
object AnalysisFunction11 {
  def apply[C, I, O](fun: ((C, List[I]) => Iterable[O])): AnalysisFunction11[C, I, O] = new AnalysisFunction11[C, I, O](fun) 
}

// This analysis function takes N input types and adds one output
// type. Does not use the composite pattern because then the passing
// of the implicits to extract the elements from the HList gets
// complicated. For an example, take a look at epic.sequences.Segmenter
trait AnalysisFunctionN1[C, I <: HList, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: SelectMany.Aux[In, I, I], adder: Adder.Aux[In, List[O], Out]): Slab[C, Out]
}

// This analysis function requires no input types except for the
// content itself.  No input types required, so functions extending
// this trait can be used to construct a new slab directly from
// content.
class AnalysisFunction01[C, O](val fun: C => Iterable[O]) {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit adder: Adder.Aux[In, List[O], Out]): Slab[C, Out] = {
    slab.add(fun(slab.content).toList)(adder)
  }
  def slabFrom(content: C): Slab[C, List[O] :: HNil] = {
    Slab(content, fun(content).toList :: HNil)
  }
}

// Convert a function of the correct signature to an AnalysisFunction01.
object AnalysisFunction01 {
  def apply[C, O](fun: (C => Iterable[O])): AnalysisFunction01[C, O] = new AnalysisFunction01[C, O](fun)
}
