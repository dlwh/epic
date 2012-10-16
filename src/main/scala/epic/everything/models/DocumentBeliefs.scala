package epic.everything
package models

import breeze.util.{Encoder, Index}
import breeze.collection.mutable.TriangularArray
import breeze.inference.Factor
import breeze.linalg._
import breeze.linalg.NumericOps.Arrays._

/**
 *
 * @param sentenceBeliefs  sentence -> property type (by index) -> PropertyBeliefs
 *@author dlwh
 */
case class DocumentBeliefs(spanProperties: Index[Property[_]],
                           wordProperties: Index[Property[_]],
                           sentenceBeliefs: Array[SentenceBeliefs]) extends Factor[DocumentBeliefs] {

  def numSentences = sentenceBeliefs.length

  def beliefsForSentence(s: Int) = sentenceBeliefs(s)

  def *(f: DocumentBeliefs): DocumentBeliefs = {
    require(sentenceBeliefs.length == f.sentenceBeliefs.length)
    DocumentBeliefs(spanProperties, wordProperties, sentenceBeliefs zip f.sentenceBeliefs map { case (a,b) => a * b})
  }

  def /(f: DocumentBeliefs): DocumentBeliefs = {
    require(sentenceBeliefs.length == f.sentenceBeliefs.length)
    DocumentBeliefs(spanProperties, wordProperties, sentenceBeliefs zip f.sentenceBeliefs map { case (a,b) => a / b})
  }

  def logPartition: Double = sentenceBeliefs.map(_.logPartition).sum

  def isConvergedTo(f: DocumentBeliefs, diff: Double): Boolean = {
    (0 until numSentences) forall { i => sentenceBeliefs(i).isConvergedTo(f.sentenceBeliefs(i), diff)}
  }
}

object DocumentBeliefs {
  def forDocument(spanProperties: Index[Property[_]], wordProperties: Index[Property[_]])(document: Document) = {
    val sentenceBeliefs = document.sentences.map(SentenceBeliefs.forSentence(spanProperties, wordProperties)(_)).toArray
    new DocumentBeliefs(spanProperties, wordProperties, sentenceBeliefs)

  }

  case class Lens(allSpanProperties: Index[Property[_]],
                  allWordProperties: Index[Property[_]],
                  narrowSpanProperties: Index[Property[_]],
                  narrowWordProperties: Index[Property[_]]) {
    def initialFullBeliefs(document: Document): DocumentBeliefs = {
      forDocument(allSpanProperties, allWordProperties)(document)
    }

    val spanMapping = Encoder.fromIndex(narrowSpanProperties).tabulateArray(p => allSpanProperties(p))
    val wordMapping = Encoder.fromIndex(narrowWordProperties).tabulateArray(p => allWordProperties(p))
    def slice(allBeliefs: DocumentBeliefs) = {
      val mapped = allBeliefs.sentenceBeliefs.map(s =>
        SentenceBeliefs(s.spans.map(span => PropertyBeliefs(spanMapping.map(span.beliefs))),
          s.words.map(word => PropertyBeliefs(wordMapping.map(word.beliefs)))
        )
      )
      new DocumentBeliefs(narrowSpanProperties, narrowWordProperties, mapped)
    }

    /**
     * Merges updated beliefs from projected into allBeliefs, creating a new DocumentBeliefs object.
     * @param allBeliefs
     * @param projected
     */
    def recombine(allBeliefs: DocumentBeliefs, projected: DocumentBeliefs):DocumentBeliefs = {
      def patch(fromAll: PropertyBeliefs, fromProjected: PropertyBeliefs, mapping: Array[Int]) = {
        val result = fromAll.copy(beliefs=fromAll.beliefs.clone)
        var i = 0
        while(i <  mapping.size) {
          result.beliefs(mapping(i)) = fromProjected(i)
          i += 1
        }
        result
      }
      val mapped = for( (sa, sp) <- allBeliefs.sentenceBeliefs zip projected.sentenceBeliefs) yield {
        val spans = TriangularArray.tabulate(sa.words.length){ (b,e) =>
          val fromAll = sa.spanBeliefs(b,e)
          if(fromAll == null) null
          else {
            patch(fromAll, sp.spanBeliefs(b, e), spanMapping)
          }
        }
        val words = Array.tabulate(sa.words.length){ (w) =>
          val fromAll = sa.wordBeliefs(w)
          if(fromAll == null) null
          else {
            patch(fromAll, sp.wordBeliefs(w), wordMapping)
          }
        }
        SentenceBeliefs(spans, words)
      }
      allBeliefs.copy(sentenceBeliefs=mapped)
    }
  }
}


case class SentenceBeliefs(spans: TriangularArray[PropertyBeliefs], words: Array[PropertyBeliefs]) extends Factor[SentenceBeliefs] {
  def length = words.length
  def apply(p: Int) = spans(p)

  def wordBeliefs(w: Int) = words(w)
  def spanBeliefs(beg: Int, end: Int) = spans(beg,end)

  def *(f: SentenceBeliefs): SentenceBeliefs = {
    require(words.length == f.words.length)
    val newSpans = TriangularArray.tabulate(words.length) { (i,j) => spans(i,j) * f.spans(i,j)}
    val newWords = Array.tabulate(words.length) { (i) => words(i) * f.words(i)}
    SentenceBeliefs(newSpans, newWords)
  }


  def /(f: SentenceBeliefs): SentenceBeliefs = {
    require(words.length == f.words.length)
    val newSpans = TriangularArray.tabulate(words.length) { (i,j) => if(spans(i,j) eq null) null else spans(i,j) / f.spans(i,j)}
    val newWords = Array.tabulate(words.length) { (i) => words(i) / f.words(i)}
    SentenceBeliefs(newSpans, newWords)
  }


  def logPartition: Double = spans.map(_.logPartition).data.sum + words.map(_.logPartition).sum

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

object SentenceBeliefs {
  def forSentence(spanProperties: Index[Property[_]], wordProperties: Index[Property[_]])(s: Sentence) = {
    def beliefs(props: Index[Property[_]]) = PropertyBeliefs(Array.tabulate(props.size)(i => Array.fill(props.get(i).arity)(1.0/props.get(i).arity)))
    val forSpans = TriangularArray.fill(s.length)(beliefs(spanProperties))
    val forWords = Array.fill(s.length)(beliefs(wordProperties))

    SentenceBeliefs(forSpans, forWords)
  }

}

/**
 * property -> value for property -> score
 * @param beliefs
 */
case class PropertyBeliefs(beliefs: Array[Array[Double]]) extends Factor[PropertyBeliefs] {
  def apply(property: Int, propValue: Int) = beliefs(property)(propValue)
  def apply(property:Int) = beliefs(property)

  def *(f: PropertyBeliefs): PropertyBeliefs = {
    PropertyBeliefs((beliefs zip f.beliefs).map{ case (a,b) => (new DenseVector(a) :* new DenseVector(b)).data})
  }


  def /(f: PropertyBeliefs): PropertyBeliefs = {
    PropertyBeliefs((beliefs zip f.beliefs).map{ case (a,b) => (new DenseVector(a) :/ new DenseVector(b)).data})
  }

  def isConvergedTo(f: PropertyBeliefs, diff: Double): Boolean = {
    (0 until beliefs.length).forall(i => norm(new DenseVector(f.beliefs(i)) :- new DenseVector(beliefs(i))) <= diff)
  }

  def logPartition: Double = beliefs.map(b => math.log(b.sum)).sum
}


