package epic.everything
package models

import breeze.util.{Encoder, Index}
import breeze.collection.mutable.TriangularArray
import breeze.inference.Factor
import breeze.linalg._
import breeze.linalg.NumericOps.Arrays._
import epic.trees.{Span, AnnotatedLabel}

/**
 *
 * @param sentenceBeliefs  sentence -> property type (by index) -> PropertyBeliefs
 *@author dlwh
 */
case class DocumentBeliefs(sentenceBeliefs: Array[SentenceBeliefs]) extends Factor[DocumentBeliefs] {

  def numSentences = sentenceBeliefs.length

  def beliefsForSentence(s: Int) = sentenceBeliefs(s)

  def *(f: DocumentBeliefs): DocumentBeliefs = {
    require(sentenceBeliefs.length == f.sentenceBeliefs.length)
    DocumentBeliefs(sentenceBeliefs zip f.sentenceBeliefs map { case (a,b) => a * b})
  }

  def /(f: DocumentBeliefs): DocumentBeliefs = {
    require(sentenceBeliefs.length == f.sentenceBeliefs.length)
    DocumentBeliefs(sentenceBeliefs zip f.sentenceBeliefs map { case (a,b) => a / b})
  }

  def logPartition: Double = sentenceBeliefs.map(_.logPartition).sum

  def isConvergedTo(f: DocumentBeliefs, diff: Double): Boolean = {
    (0 until numSentences) forall { i => sentenceBeliefs(i).isConvergedTo(f.sentenceBeliefs(i), diff)}
  }
}

object DocumentBeliefs {
  def forDocument(doc: ProcessedDocument) = {
//    val sentences = doc.sentences.map(SentenceBeliefs.forSentence(_, doc.coref))

    sys.error("TODO")
  }
}


case class SentenceBeliefs(spans: TriangularArray[SpanBeliefs], words: Array[WordBeliefs]) extends Factor[SentenceBeliefs] {
  def length = words.length
  def apply(p: Int) = spans(p)

  def wordBeliefs(w: Int) = words(w)
  def spanBeliefs(beg: Int, end: Int) = spans(beg,end)

  def *(f: SentenceBeliefs): SentenceBeliefs = {
    require(words.length == f.words.length)
    val newSpans = TriangularArray.tabulate(words.length) { (i,j) => if(spans(i, j) eq null) null else spans(i,j) * f.spans(i,j)}
    val newWords = Array.tabulate(words.length) { (i) => words(i) * f.words(i)}
    SentenceBeliefs(newSpans, newWords)
  }


  def /(f: SentenceBeliefs): SentenceBeliefs = {
    require(words.length == f.words.length)
    val newSpans = TriangularArray.tabulate(words.length) { (i,j) => if(spans(i,j) eq null) null else spans(i,j) / f.spans(i,j)}
    val newWords = Array.tabulate(words.length) { (i) => words(i) / f.words(i)}
    SentenceBeliefs(newSpans, newWords)
  }


  def logPartition: Double = spans.map(s => if(s eq null) 0.0 else s.logPartition).data.sum + words.map(_.logPartition).sum

  def isConvergedTo(f: SentenceBeliefs, diff: Double): Boolean = {
    var i = 0
    while(i < length) {
      var j = i + 1
      while(j <= length) {
        if(spans(i,j).ne(null) && !spans(i,j).isConvergedTo(f.spans(i,j), diff) ) return false
        j += 1
      }
      if(!words(i).isConvergedTo(f.words(i), diff) ) return false
      i += 1
    }
    true
  }
}


/**
 * Represents distributions over certain span properties
 * @param span which span I'm talking about
 * @param governor which word governs my span. not my head. my head's head. length = root (i.e. whole setnence), length+1 == off
 * @param label my syntactic label type
 * @param ner my ner type,
 * @param observedNer whether or not the NER is emitted.
 */
case class SpanBeliefs(span: DSpan,
                       governor: Beliefs[Int], // which word governs me. This isn't my head, it's my head's head.
                       label: Beliefs[String], // syntactic label type.
                       ner: Beliefs[NERType.Value],
                       observedNer: Beliefs[Boolean]) extends Factor[SpanBeliefs] {
  def *(f: SpanBeliefs): SpanBeliefs = SpanBeliefs(span, governor * f.governor, label * f.label, ner * f.ner, observedNer * f.observedNer)
  def /(f: SpanBeliefs): SpanBeliefs = SpanBeliefs(span, governor / f.governor, label / f.label, ner / f.ner, observedNer / f.observedNer)

  def logPartition: Double = governor.logPartition + label.logPartition + ner.logPartition + observedNer.logPartition

  def isConvergedTo(f: SpanBeliefs, diff: Double): Boolean = (
    governor.isConvergedTo(f.governor, diff)
      && label.isConvergedTo(f.label, diff)
      && ner.isConvergedTo(f.ner, diff)
      && observedNer.isConvergedTo(f.observedNer, diff)
    )
}


case class WordBeliefs(pos: DPos,
                       governor: Beliefs[Int],
                       span: Beliefs[Span],
                       tag: Beliefs[String],
                       maximalLabel: Beliefs[String],
                       anaphoric: Beliefs[Boolean],
                       anaphor: Beliefs[DPos],
                       coref: Array[Beliefs[Int]]
                       ) extends Factor[WordBeliefs] {
  def *(f: WordBeliefs): WordBeliefs = WordBeliefs(pos, governor * f.governor, span * f.span, tag * f.tag, maximalLabel * f.maximalLabel, f.anaphoric * f.anaphoric, f.anaphor * f.anaphor, Array.tabulate(coref.length)(i => coref(i) * f.coref(i)))

 def /(f: WordBeliefs): WordBeliefs = WordBeliefs(pos, governor / f.governor, span / f.span, tag / f.tag, maximalLabel / f.maximalLabel, f.anaphoric / f.anaphoric, f.anaphor / f.anaphor, Array.tabulate(coref.length)(i => coref(i) / f.coref(i)))

  def logPartition: Double = (
    governor.logPartition
      + span.logPartition
      + tag.logPartition
      + anaphoric.logPartition
      + anaphor.logPartition
      + coref.foldLeft(0.0)(_ + _.logPartition)
    )

  def isConvergedTo(f: WordBeliefs, diff: Double): Boolean = (this eq f) || (
    governor.isConvergedTo(f.governor, diff)
      && span.isConvergedTo(f.span, diff)
      && tag.isConvergedTo(f.tag, diff)
      && anaphoric.isConvergedTo(f.anaphoric, diff)
      && anaphor.isConvergedTo(f.anaphor, diff)
      && tag.isConvergedTo(f.tag, diff)
      && maximalLabel.isConvergedTo(f.tag, diff)
      && (0 until coref.length).forall(i => coref(i).isConvergedTo(f.coref(i), diff))
    )
}


