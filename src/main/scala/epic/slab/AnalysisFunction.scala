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

// This analysis function requires one input and adds one output type.
trait AnalysisFunction11[C, I, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: Selector[In, Vector[I]], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out] = {
    slab.add(apply(slab.content, slab.select(sel)))
  }
  def apply(content: C, input: Vector[I]): Vector[O]
}

// Convert a function of the correct signature to an AnalysisFunction11.
object AnalysisFunction11 {
  def apply[C, I, O](fun: ((C, Vector[I]) => Vector[O])): AnalysisFunction11[C, I, O] = new AnalysisFunction11[C, I, O] {
    def apply(content: C, input: Vector[I]): Vector[O] = fun(content, input)
  }
}

// This analysis function takes N input types and adds one output
// type. Does not use the composite pattern because then the passing
// of the implicits to extract the elements from the HList gets
// complicated. For an example, take a look at epic.sequences.Segmenter
trait AnalysisFunctionN1[C, I <: HList, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: SelectMany.Aux[In, I, I], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out]
}

// This analysis function requires no input types except for the
// content itself.  No input types required, so functions extending
// this trait can be used to construct a new slab directly from
// content.
trait AnalysisFunction01[C, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out] = {
    slab.add(apply(slab.content))(adder)
  }
  def apply(content: C): Vector[O]

  def slabFrom(content: C): Slab[C, Vector[O] :: HNil] = {
    Slab(content, apply(content) :: HNil)
  }
}

// Convert a function of the correct signature to an AnalysisFunction01.
object AnalysisFunction01 {
  def apply[C, O](fun: (C => Vector[O])): AnalysisFunction01[C, O] = new AnalysisFunction01[C, O] {
    def apply(content: C): Vector[O] = fun(content)
  }
}

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
