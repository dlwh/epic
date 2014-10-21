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

// This analysis function requires one input and adds one output type
trait AnalysisFunction11[C, I, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: Selector[In, Vector[I]], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out]
}

// This analysis function takes N input types and adds one output type
trait AnalysisFunctionN1[C, I <: HList, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: SelectMany.Aux[In, I, I], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out]
}

// This analysis function requires no input types except for the
// content itself.
trait AnalysisFunction01[C, O] {
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out]
}

// A simpler version of the 1 to 1 analysis function which allows for
// an easy definition of new analysis function without directly
// interfacing with the shapeless data structure.
trait SimpleAnalysisFunction[C, I, O] extends AnalysisFunction11[C, I, O] {
  def apply(content: C, in: Vector[I]): Vector[O]
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit sel: Selector[In, Vector[I]], adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out] = slab.add(apply(slab.content, slab.get(sel)))
}

// No input types required, so functions extending this trait can be
// used to construct a new slab directly from content.
trait SourceAnalysisFunction[C, O] extends AnalysisFunction01[C, O] with (C => Vector[O]) {
  def apply(content: C): Vector[O]
  def apply[In <: HList, Out <: HList](slab: Slab[C, In])(implicit adder: Adder.Aux[In, O, Vector[O], Out]): Slab[C, Out] = slab.add(apply(slab.content))(adder)
  def createSlabFrom(content: C): Slab[C, Vector[O] :: HNil] = {
    Slab(content, apply(content) :: HNil)
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
