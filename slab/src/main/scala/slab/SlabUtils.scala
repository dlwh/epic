package epic.util
import shapeless._
import shapeless.syntax.typeable._
import ops.hlist._
import epic.slab.typeclasses._
import epic.slab._

package object slabutils {
  implicit class SlabUtilOps[L <: HList](slab: Slab[String, L]) {
    def perSentence[T <: SpanAnnotation](implicit selsentence: SubSelector[L, Vector[Sentence]], selelements: SubSelector[L, Vector[T]]): Vector[Iterable[T]] = {
      val index = SpanIndex[T](slab.select[T](selelements))
      slab.select[Sentence](selsentence).map(sentence => index(sentence.span))
    }
    def tokens(implicit selsentence: SubSelector[L, Vector[Sentence]], seltoken: SubSelector[L, Vector[Token]]): Vector[Iterable[String]] = {
      perSentence[Token](selsentence, seltoken).map(_.map(token => slab.substring(token.span)))
    }
  }
}
