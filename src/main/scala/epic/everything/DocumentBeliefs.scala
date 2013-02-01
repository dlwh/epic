package epic.everything

import breeze.util.{OptionIndex, DenseIntIndex, Encoder, Index}
import breeze.collection.mutable.TriangularArray
import breeze.inference.Factor
import breeze.linalg._
import breeze.linalg.NumericOps.Arrays._
import epic.trees.{Span, AnnotatedLabel}
import epic.parser.BaseGrammar
import epic.ontonotes.{DPos, DSpan, NERType}
import epic.everything.FeaturizedSentence

/**
 *
 * @param sentences  sentence -> property type (by index) -> PropertyBeliefs
 *@author dlwh
 */
case class DocumentBeliefs(sentences: Array[SentenceBeliefs]) extends Factor[DocumentBeliefs] {

  def numSentences = sentences.length

  def beliefsForSentence(s: Int) = sentences(s)

  def *(f: DocumentBeliefs): DocumentBeliefs = {
    if (f eq null) this
    else {
      require(sentences.length == f.sentences.length)
      DocumentBeliefs(sentences zip f.sentences map { case (a,b) => a * b})
    }
  }

  def /(f: DocumentBeliefs): DocumentBeliefs = {
    if (f eq null) this
    else {
      require(sentences.length == f.sentences.length)
      DocumentBeliefs(sentences zip f.sentences map { case (a,b) => a / b})
    }
  }

  def logPartition: Double = sentences.map(_.logPartition).sum

  def isConvergedTo(f: DocumentBeliefs, diff: Double): Boolean = {
    if (f eq null) false
    else {
      (0 until numSentences) forall { i => sentences(i).isConvergedTo(f.sentences(i), diff)}
    }
  }

  def maxChange(f: DocumentBeliefs): Double = {
    if (f eq null) 0.0
    else {
      (0 until numSentences) map { i => sentences(i).maxChange(f.sentences(i))} max
    }
  }


}

object DocumentBeliefs {
  class Factory(grammar: BaseGrammar[AnnotatedLabel], val nerLabelIndex: Index[NERType.Value], val srlLabelIndex: Index[String]) {
    val sentenceFactory = new SentenceBeliefs.Factory(grammar, nerLabelIndex, srlLabelIndex)
    def nerProp = sentenceFactory.nerProp
    def srlProp = sentenceFactory.srlProp
    def labelProp = sentenceFactory.labelProp
    def optionLabelProp = sentenceFactory.optionLabelProp
    private val initNERBelief = Beliefs.improperUninformed(nerProp)
    private val initSRLBelief = Beliefs.improperUninformed(srlProp)

    def apply(doc: FeaturizedDocument):DocumentBeliefs = {
      val sentences = for(s <- doc.sentences.toArray) yield {
        sentenceFactory(s)
      }

      DocumentBeliefs(sentences)
    }
  }
}


case class SentenceBeliefs(spans: TriangularArray[SpanBeliefs],
                           wordBeliefs: Array[WordBeliefs]) extends Factor[SentenceBeliefs] {
  assert(TriangularArray.arraySize(wordBeliefs.length+1) == spans.data.length)
  def length = wordBeliefs.length
  def apply(p: Int) = spans(p)

  def spanBeliefs(beg: Int, end: Int) = spans(beg,end)

  def *(f: SentenceBeliefs): SentenceBeliefs = {
    if (f eq null) {
      this
    } else {

      require(wordBeliefs.length == f.wordBeliefs.length)
      val newSpans = TriangularArray.tabulate(wordBeliefs.length+1) { (i,j) => if(spans(i, j) eq null) null else spans(i,j) * f.spans(i,j)}
      val newWords = Array.tabulate(wordBeliefs.length) { (i) => wordBeliefs(i) * f.wordBeliefs(i)}
      SentenceBeliefs(newSpans, newWords)
    }
  }


  def /(f: SentenceBeliefs): SentenceBeliefs = {
    if (f eq null) {
      this
    } else {
      require(wordBeliefs.length == f.wordBeliefs.length)
      val newSpans = TriangularArray.tabulate(wordBeliefs.length+1) { (i,j) => if(spans(i,j) eq null) null else spans(i,j) / f.spans(i,j)}
      val newWords = Array.tabulate(wordBeliefs.length) { (i) => wordBeliefs(i) / f.wordBeliefs(i)}
      SentenceBeliefs(newSpans, newWords)
    }
  }


  def logPartition: Double = spans.map(s => if(s eq null) 0.0 else s.logPartition).data.filter(_ != Double.NegativeInfinity).sum + wordBeliefs.map(_.logPartition).filter(_ != Double.NegativeInfinity).sum

  def isConvergedTo(f: SentenceBeliefs, diff: Double): Boolean = {
    var i = 0
    while(i < length) {
      var j = i + 1
      while(j <= length) {
        if(spans(i,j).ne(null) && !spans(i,j).isConvergedTo(f.spans(i,j), diff) ) return false
        j += 1
      }
//      if(!wordBeliefs(i).isConvergedTo(f.wordBeliefs(i), diff) ) return false
      i += 1
    }
    true
  }

  def maxChange(f: SentenceBeliefs): Double = {
    var i = 0
    var max = 0.0
    while(i < length) {
      var j = i + 1
      while(j <= length) {
        if(spans(i,j).ne(null) && !spans(i,j).isConvergedTo(f.spans(i,j))) {
          max = max max spans(i,j).maxChange(f.spans(i, j))
        }
        j += 1
      }
//      max = max max wordBeliefs(i).maxChange(f.wordBeliefs(i))
      i += 1
    }
    max
  }
}

object SentenceBeliefs {
  class Factory(grammar: BaseGrammar[AnnotatedLabel], val nerLabelIndex: Index[NERType.Value], val srlLabelIndex: Index[String]) {

    val nerProp = Property("ner", nerLabelIndex)
    val labelProp = Property("label", grammar.labelIndex)
    val optionLabelProp = Property("option[label]", new OptionIndex(grammar.labelIndex))
    val srlProp = Property("srl", new OptionIndex(srlLabelIndex))

    private val initNERBelief = Beliefs.improperUninformed(nerProp)
    private val initSRLBelief = Beliefs.improperUninformed(srlProp)
    private val labelBeliefs = Beliefs.improperUninformed(labelProp)
    private val optionLabelBeliefs = Beliefs.improperUninformed(optionLabelProp)

    def apply(s: FeaturizedSentence):SentenceBeliefs = {
      val spanGovernorBeliefs = Beliefs.improperUninformed("wordPos+None", new DenseIntIndex(0, s.length+2))
      val wordGovernorBeliefs = Beliefs.improperUninformed("wordPos", new DenseIntIndex(0, s.length+1))
      //        val governedSpanBeliefs = Beliefs.improperUninformed("span", Index{for(b <- 0 until s.length + 1; end <- b until s.length + 1) yield Span(b,end)})
      val spans = TriangularArray.tabulate(s.length+1) { (begin, end) =>
        if(begin < end && s.isPossibleSpan(begin, end))
          SpanBeliefs(DSpan(s.id,s.index,begin, end), spanGovernorBeliefs, optionLabelBeliefs, initNERBelief, Array.fill(s.frames.length)(initSRLBelief))
        else
          null
      }
      val words = Array.tabulate(s.length) { (pos) =>
        WordBeliefs(DPos(s.id,s.index,pos),
          wordGovernorBeliefs,
          //            governedSpanBeliefs,
          labelBeliefs,
          labelBeliefs)
      }

      SentenceBeliefs(spans, words)
    }
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
                       label: Beliefs[Option[AnnotatedLabel]], // syntactic label type. None == Not Constituent
                       ner: Beliefs[NERType.Value],
                       frames: IndexedSeq[Beliefs[Option[String]]]) extends Factor[SpanBeliefs] {
  def *(f: SpanBeliefs): SpanBeliefs = {
    SpanBeliefs(span,
    governor * f.governor,
    label * f.label,
    ner * f.ner,
    {for ((a,b) <- frames zip f.frames) yield a * b})
  }
  def /(f: SpanBeliefs): SpanBeliefs = SpanBeliefs(span,
    governor / f.governor,
    label / f.label,
    ner / f.ner,
    {for ((a,b) <- frames zip f.frames) yield a / b})

  def logPartition: Double = governor.logPartition + label.logPartition + ner.logPartition + frames.map(_.logPartition).sum

  def isConvergedTo(f: SpanBeliefs, diff: Double): Boolean = (
    governor.isConvergedTo(f.governor, diff)
      && label.isConvergedTo(f.label, diff)
      && ner.isConvergedTo(f.ner, diff)
      && {for ((a,b) <- frames zip f.frames iterator) yield a isConvergedTo b}.forall(_ == true)
    //      && observedNer.isConvergedTo(f.observedNer, diff)
    )

  def maxChange(f: SpanBeliefs): Double = {
    governor.maxChange(f.governor) max label.maxChange(f.label) max ner.maxChange(f.ner) max  {if (frames.isEmpty) 0.0 else {for ((a,b) <- frames zip f.frames) yield a.maxChange(b)}.max}
  }
}

case class WordBeliefs(pos: DPos,
                       governor: Beliefs[Int],
                       //                       span: Beliefs[Span],
                       tag: Beliefs[AnnotatedLabel],
                       maximalLabel: Beliefs[AnnotatedLabel]
                       //,
                       //                       anaphoric: Beliefs[Boolean],
                       //                       anaphor: Beliefs[DPos],
                       //                       coref: Array[Beliefs[Int]]
                        ) extends Factor[WordBeliefs] {
  def *(f: WordBeliefs): WordBeliefs = WordBeliefs(pos, governor * f.governor, /*span * f.span,*/ tag * f.tag, maximalLabel * f.maximalLabel)//, f.anaphoric * f.anaphoric, f.anaphor * f.anaphor, Array.tabulate(coref.length)(i => coref(i) * f.coref(i)))

  def /(f: WordBeliefs): WordBeliefs = {
    WordBeliefs(pos,
      governor / f.governor,
      /*span / f.span,*/
      tag / f.tag,
      maximalLabel / f.maximalLabel)
  }//, f.anaphoric / f.anaphoric, f.anaphor / f.anaphor, Array.tabulate(coref.length)(i => coref(i) / f.coref(i)))
  //*/

  def logPartition: Double =  (
    0.0
      + governor.logPartition
      //      + span.logPartition
      + tag.logPartition
      + maximalLabel.logPartition
    //      + anaphoric.logPartition
    //      + anaphor.logPartition
    //      + coref.foldLeft(0.0)(_ + _.logPartition)
    )

  def isConvergedTo(f: WordBeliefs, diff: Double): Boolean = (this eq f) || (
    governor.isConvergedTo(f.governor, diff)
      //      && span.isConvergedTo(f.span, diff)
      && tag.isConvergedTo(f.tag, diff)
      //      && anaphoric.isConvergedTo(f.anaphoric, diff)
      //      && anaphor.isConvergedTo(f.anaphor, diff)
      && maximalLabel.isConvergedTo(f.tag, diff)
    //      && (0 until coref.length).forall(i => coref(i).isConvergedTo(f.coref(i), diff))
    )
}


