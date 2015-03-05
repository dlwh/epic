package epic.util
import shapeless._
import shapeless.syntax.typeable._
import ops.hlist._
import epic.slab.typeclasses._
import epic.slab._

package object slabutils {
  implicit class SlabUtilOps[L <: HList](slab: Slab[String, L]) {
    def tokens(implicit selsentence: SubSelector[L, List[Sentence]], seltoken: SubSelector[L, List[Token]]): List[Iterable[String]] = {
      val index = SpanIndex(slab.select[Token](seltoken))
      slab.select[Sentence](selsentence).map(sentence => index(sentence.span).map(token => slab.substring(token.span)))
    }
  }
}
