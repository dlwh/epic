package epic

import epic.trees.Span

/**
 * TODO
 *
 * @author dlwh
 **/
package object slab {
  // some type aliases
  type StringAnalysisFunction[I, O] = AnalysisFunction[String, Span, I, O]
  type StringSlab[+AnnotationTypes] = Slab[String, Span, AnnotationTypes]
}
