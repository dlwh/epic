package epic.slab
import shapeless._
import ops.hlist._
import Utils._

/**
 * An analysis function that takes a Slab with declared annotation types in it and outputs
 * a new Slab with additional annotations of a new type.
 *
 * Documentation for the type variables:
 *   C = Content type
 *   I = Required annotation type
 *   O = Produced annotation type
 */
trait AnalysisFunction11[C, I, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: Selector[In, Vector[I]], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out]

  // def andThen(other: AnalysisFunction[C]):AnalysisFunction[C] = {
  //   new ComposedAnalysisFunction[C](this, other)
  // }

}

// case class ComposedAnalysisFunction[C](a: AnalysisFunction[C], b: AnalysisFunction[C]) extends AnalysisFunction[C] {
//   def apply[In <: HList, Out <: HList](slab: Slab[C,In]):Slab[C,Out] = {
//     b(a(slab))
//   }
// }


// object AnalysisPipeline {
//   import AnnotatedSpan._

//   // added only to demonstrate necesssity of [I] parameter on analyzers
//   private[AnalysisPipeline] case class Document()
//   private[AnalysisPipeline] def documentAdder(slab: StringSlab[Any]) =
//     slab ++ Iterator(Span(0, slab.content.length) -> Document())

//   def main (args: Array[String]) {
//     def sentenceSegmenter:StringAnalysisFunction[Any, Sentence] = RegexSentenceSegmenter
//     def tokenizer = RegexTokenizer
//     val pipeline = sentenceSegmenter andThen tokenizer

//     val inSlab = Slab("test\n.").+(Span(0, 5) -> Document())
//     val slab = pipeline(inSlab)

//     // just to show what the type is
//     val typedSpan: Slab[String, Span, Document with Sentence with Token] = slab

//     // added only to demonstrate necesssity of [I] parameter on analyzers
//     val paragraphs = slab.iterator[Document].toList


//     val sentences = slab.iterator[Sentence].toList
//     println("\nSENTENCES\n\n" + sentences.map(r => slab.spanned(r._1)).mkString("\n\n"))
    
//     val tokens = slab.iterator[Token].toList
//     println("\nTOKENS\n\n" + tokens.map(r => slab.spanned(r._1)).mkString("\n\n"))

//   }


// }
