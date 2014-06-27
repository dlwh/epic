package epic

/**
 * TODO
 *
 * @author dlwh
 **/
package object slab {
  // some type aliases


  type StringAnalysisFunction[I <: AnnotatedSpan, O <: AnnotatedSpan] = AnalysisFunction[String, AnnotatedSpan, I, O]
  type StringSlab[+AnnotationTypes <: AnnotatedSpan] = Slab[String, AnnotatedSpan, AnnotationTypes]

}
