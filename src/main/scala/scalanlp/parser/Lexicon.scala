package scalanlp.parser
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import breeze.linalg._
import scalanlp.trees.LexicalProduction

/**
 * Just knows what tags are appropriate for a given word
 */
@SerialVersionUID(1)
trait Lexicon[L, W] extends Serializable {
  def tagsForWord(w: W):Iterator[L]
  def knownLexicalProductions: Iterator[LexicalProduction[L, W]]
}

object Lexicon {
  // TODO Probably delete this implicit soon
  implicit def apply[L, W](wordCounts: Counter2[L, W, Double]) = new SimpleLexicon(wordCounts)
}

class UnsmoothedLexicon[L, W](knownProductions: Set[LexicalProduction[L, W]]) extends Lexicon[L, W] {
  import collection.mutable._
  private val byWord = new HashMap[W, Set[L]] with MultiMap[W, L];
  for( LexicalProduction(l, w) <- knownProductions) {
    byWord.addBinding(w, l)
  }

  def knownLexicalProductions = knownProductions.iterator

  def tagsForWord(w: W) = byWord(w).iterator
}

/**
 * A simple lexicon that thresholds to decide when to open up the rare word to all (open) tags
 * @param wordTagCounts (tag -> word -> count)
 * @param openTagThreshold how many different word types does a tag have to be seen with to be considered open.
 * @param closedWordThreshold How many
 */
class SimpleLexicon[L, W](wordTagCounts: Counter2[L, W, Double],
                          openTagThreshold: Int = 50,
                          closedWordThreshold: Int= 10) extends Lexicon[L, W] {
  private val wordCounts:Counter[W, Double] = sum(wordTagCounts, Axis._0)
  private val labelCounts:Counter[L, Double] = sum(wordTagCounts, Axis._1)

  import collection.mutable._

  private val byWord = new HashMap[W, Set[L]] with MultiMap[W, L];
  for( (l, w) <- wordTagCounts.keysIterator) {
    byWord.addBinding(w, l)
  }

  private val openTags = labelCounts.keysIterator.filter(l => wordTagCounts(l, ::).size > openTagThreshold).toSet
  for( (w,v) <- wordCounts.iterator if v < closedWordThreshold) {
    byWord.get(w) match {
      case None => byWord(w) = collection.mutable.Set() ++= openTags
      case Some(set) => set ++= openTags
    }
  }

  def knownLexicalProductions = for( (w,set) <- byWord.iterator; l <- set.iterator) yield LexicalProduction(l, w)

  def tagsForWord(w: W) = byWord.getOrElse(w,openTags).iterator
}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class SignatureLexicon[L,W](initCounts: Counter2[L,W,Double],
                            sigGen: W=>W,
                            signatureThreshold: Double = 2) extends Lexicon[L,W] {
  val (wordCounts, lexicon, sigCounts) = SignatureLexicon.makeSignatureCounts(sigGen, initCounts, signatureThreshold)

  import collection.mutable._

  private val byWord = new HashMap[W, Set[L]] with MultiMap[W, L]
  for( (l, w) <- lexicon.keysIterator) {
    byWord.addBinding(w, l)
  }

  def tagsForWord(w: W) = {
    val sig = asSignature(w)
    byWord.get(sig).map(_.iterator).getOrElse(initCounts.keysIterator.map(_._1))
  }

  def knownLexicalProductions = wordCounts.keysIterator.flatMap(w => tagsForWord(w).map(LexicalProduction(_,w)))

  private def asSignature(w: W): W = {
    if (wordCounts(w) < signatureThreshold) {
      sigGen(w)
    } else {
      w
    }
  }

}

object SignatureLexicon {
  /**
   * Remaps words by their signatures, if they are sufficiently rare (otherwise, a word's signature is itself.)
   *
   * Kind of a horrible hacky method. Sorry.
   *
   * @param sigGen
   * @param counts
   * @param threshold
   * @return word counts, (tag,signature) counts, signature counts
   */
  def makeSignatureCounts[L,W](sigGen: W=>W,
                               counts: Counter2[L, W, Double],
                               threshold: Double): (Counter[W, Double], Counter2[L, W, Double], Counter[W, Double]) = {
    val wordCounts = sum(counts, Axis._0)
    val lexicon = Counter2[L,W,Double]()
    val sigCounts = Counter[W,Double]()
    for( ((l,w),count) <- counts.activeIterator) {
      val sig = if(wordCounts(w) < threshold) sigGen(w) else w

      sigCounts(sig) += count
      lexicon(l,sig) += count
    }

    (wordCounts, lexicon, sigCounts)
  }
}

