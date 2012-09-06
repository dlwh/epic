package epic.everything
package models

import epic.coref.Property
import breeze.util.Index
import breeze.collection.mutable.TriangularArray

/**
 *
 * @param spanBeliefs  sentence -> property type (by index) -> SpanBeliefs
 *@author dlwh
 */
class DocumentBeliefs(val index: Index[Property], val spanBeliefs: Array[SentenceBeliefs]) {
  def beliefsForProperty(s: Int, property: Int): SpanBeliefs = {
    spanBeliefs(s)(property)
  }

  def beliefsForSentence(s: Int) = spanBeliefs(s)
}

object DocumentBeliefs {
  def forDocument(propIndex: Index[Property], arities: Array[Int])(document: Document) = {
    assert(arities.size == propIndex.size)
    val sentenceBeliefs = document.sentences.map(SentenceBeliefs.forSentence(propIndex, arities)(_)).toArray
    new DocumentBeliefs(propIndex, sentenceBeliefs)

  }
}


case class SentenceBeliefs(beliefs: Array[SpanBeliefs]) {
  def apply(p: Int) = beliefs(p)
}

object SentenceBeliefs {
  def forSentence(propIndex: Index[Property], arities: Array[Int])(sentence: Sentence) = {
    assert(arities.size == propIndex.size)
    val spans = Array.tabulate(propIndex.size){p =>
      SpanBeliefs(p, TriangularArray.fill(sentence.length)(new Array[Double](arities(p))))
    }
    SentenceBeliefs(spans)
  }
}

/**
 * (begin, end) triangular index -> value for belief -> score
 * @param prop
 * @param beliefs
 */
case class SpanBeliefs(prop: Int, beliefs: TriangularArray[Array[Double]]) {
  def forSpan(begin: Int, end: Int):Array[Double] = beliefs(begin, end)
  def apply(begin: Int, end: Int, propValue: Int) = beliefs(begin,end)(propValue)
}