package epic.everything
package models

import breeze.util.{Encoder, Index}
import breeze.collection.mutable.TriangularArray

/**
 *
 * @param sentenceBeliefs  sentence -> property type (by index) -> SpanBeliefs
 *@author dlwh
 */
case class DocumentBeliefs(spanProperties: Index[Property[_]],
                           wordProperties: Index[Property[_]],
                           sentenceBeliefs: Array[SentenceBeliefs]) {
  def beliefsForProperty(s: Int, property: Int): SpanBeliefs = {
    sentenceBeliefs(s)(property)
  }

  def beliefsForSentence(s: Int) = sentenceBeliefs(s)
}

object DocumentBeliefs {
  def forDocument(spanProperties: Index[Property[_]], wordProperties: Index[Property[_]])(document: Document) = {
    val sentenceBeliefs = document.sentences.map(SentenceBeliefs.forSentence(spanProperties, wordProperties)(_)).toArray
    new DocumentBeliefs(spanProperties, wordProperties, sentenceBeliefs)

  }
}


case class SentenceBeliefs(spans: Array[SpanBeliefs], words: Array[WordBeliefs]) {
  def apply(p: Int) = spans(p)
}

object SentenceBeliefs {
  def forSentence(spanProperties: Index[Property[_]], wordProperties: Index[Property[_]])(s: Sentence) = {
    val forSpans = Array.tabulate(spanProperties.size)(i => SpanBeliefs(i, TriangularArray.fill(s.length)(new Array[Double](spanProperties.get(i).arity))))
    val forWords = Array.tabulate(wordProperties.size)(i => WordBeliefs(i, Array.fill(s.length)(new Array[Double](wordProperties.get(i).arity))))

    SentenceBeliefs(forSpans, forWords)
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


/**
 * (begin, end) triangular index -> value for belief -> score
 * @param prop
 * @param beliefs
 */
case class WordBeliefs(prop: Int, beliefs: Array[Array[Double]]) {
  def forWord(begin: Int):Array[Double] = beliefs(begin)
  def apply(begin: Int, propValue: Int) = beliefs(begin)(propValue)
}
