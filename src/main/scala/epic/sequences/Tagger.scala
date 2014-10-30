package epic.sequences

import epic.slab._
import epic.slab.Implicits
import epic.slab.Utils._
import shapeless._
import ops.hlist._
import epic.slab.Indexes._
import epic.trees.AnnotatedLabel

/**
 * A Tagger assigns a sequence of Tags to a Sentence
 *
 * @tparam Tag the type of tag that is annotated
 *
 * @author dlwh
 **/
object aliases {
  type TaggerInput = Vector[Sentence] :: Vector[Token] :: HNil
}
import aliases._

// Basic annotator. The function is passed the Vector of Tokens, one Sentence per time.
class Annotator[T](val fun: ((String, Vector[Token]) => Vector[T])) extends AnalysisFunctionN1[String, TaggerInput, T] {
  override def apply[In <: HList, Out <: HList](slab: Slab[String, In])(implicit sel: SelectMany.Aux[In, TaggerInput, TaggerInput], adder: Adder.Aux[In, T, Vector[T], Out]): Slab[String, Out] = {
    val data = slab.selectMany(sel)
    val index = SpanIndex(slab.content, data.select[Vector[Token]])
    val annotatedSentences = for(sentence <- data.select[Vector[Sentence]]) yield {
      fun(slab.content, index(sentence.span))
    }
    slab.add(annotatedSentences.flatten)(adder)
  }
}

object Annotator {
  def apply[T](fun: ((String, Vector[Token]) => Vector[T])) = new Annotator(fun)
}

// Create a new tagger by creating a new class passing a tagger as
// function.
class Tagger[Tag](val tagger: (Vector[String] => Vector[Tag])) extends Annotator[Tagged[Tag]](Tagger.tag(tagger))

object Tagger {
  // Merges tag information back into the Vector[Tagged[Tag]] format.
  def tag[Tag](fun: Vector[String] => Vector[Tag])(content: String, tokens: Vector[Token]): Vector[Tagged[Tag]] = {
    val strings = tokens.map(t => content.substring(t.span.begin, t.span.end))
    val tagSeq = fun(strings)
    tokens.zip(tagSeq).map({case (token, tag) => Tagged[Tag](token.span, tag)})
  }
  def apply[Tag](fun: Vector[String] => Vector[Tag]): Tagger[Tag] = new Tagger[Tag](fun)

  def posTagger(crf: CRF[AnnotatedLabel, String]) = fromCRF(crf, (a: AnnotatedLabel) => a.label)

  def fromCRF[L, Tag](crf: CRF[L, String], lToTag: L=>Tag):Tagger[Tag] = new CRFTagger(crf, lToTag)

  case class CRFTagger[L, Tag](crf: CRF[L, String], lToTag: L=>Tag) extends Tagger[Tag](v1 => crf.bestSequence(v1).tags.map(lToTag).toVector)
}
