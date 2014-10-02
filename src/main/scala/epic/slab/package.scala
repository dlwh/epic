package epic
import shapeless._

/**
 * TODO
 *
 * @author dlwh
 **/
package object slab {
  // some type aliases

  type StringAnalysisFunction = AnalysisFunction[String]
  type StringSlab[L <: HList] = Slab[String, L]

}
