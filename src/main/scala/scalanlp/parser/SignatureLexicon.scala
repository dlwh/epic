package scalanlp.parser

import scalala.tensor.{Counter, Counter2}
import scalanlp.trees.LexicalProduction

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
    byWord.getOrElse(sig,Set.empty[L]).iterator
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
    val wordCounts = Counter[W,Double]()
    for( ((l,w),count) <- counts.nonzero.pairs) {
      wordCounts(w) += count
    }
    val lexicon = Counter2[L,W,Double]()
    val sigCounts = Counter[W,Double]()
    for( ((l,w),count) <- counts.nonzero.pairs) {
      val sig = if(wordCounts(w) < threshold) sigGen(w) else w

      sigCounts(sig) += count
      lexicon(l,sig) += count
    }

    (wordCounts, lexicon, sigCounts)
  }
}
