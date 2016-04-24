package epic.sequences

import breeze.util.{Encoder, Index}
import epic.sequences.CRF.Anchoring
import breeze.linalg._
import epic.lexicon.{Lexicon, SimpleLexicon}

/**
 * HiddenMarkovModel, which is the generative special case of a [[epic.sequences.CRF]].
 *
 * @author dlwh
 */
object HMM {
  def apply[L, W](startLabel: L, transitions: Counter2[L, L, Double], emissions: Counter2[L, W, Double], smoothEmissions: Boolean = false):CRF[L, W] = {
    val li = Index[L]()
    li.index(startLabel)
    for( (l1, l2) <- transitions.keysIterator) {
      li.index(l1)
      li.index(l2)
    }

    val enc = Encoder.fromIndex(li)

    val wordCounts = sum(emissions, Axis._0)
    val labelCounts = sum(emissions, Axis._1)
    val indexedLabelCounts = enc.encode(labelCounts)

    val encodedTransitions = logAndNormalize(enc.encode(transitions), Axis._1)
    val totalCount = sum(labelCounts)

    val lex = new SimpleLexicon[L, W](li, emissions)

    new CRF[L, W] {

      def lexicon: Lexicon[L, W] = lex

      def anchor(w: IndexedSeq[W]): Anchoring[L, W] = new Anchoring[L, W] {

        val wcs = w.map(wordCounts(_))
        val validSyms = w.map { w =>
          if (wordCounts(w) >= 10) {
            emissions(::, w).findAll( _ > 0).map(labelIndex(_)).toSet
          } else {
            allSyms
          }
        }

        def words: IndexedSeq[W] = w

        def labelIndex: Index[L] = li

        def startSymbol: L = startLabel

        def scoreTransition(pos: Int, prev: Int, cur: Int): Double = {
          val emitScore = scoreEmission(cur, pos)
          assert(!emitScore.isNaN)
          emitScore + encodedTransitions(prev, cur)
        }

        def scoreEmission(cur: Int, pos: Int): Double = if (smoothEmissions) {
          val w = words(pos)
          var cWord = wcs(pos)
          var cTagWord = emissions(labelIndex.get(cur), w)
          assert(cWord >= cTagWord)
          if (cWord < 10) {
            cWord += 1.0
            cTagWord += indexedLabelCounts(cur) / wordCounts.size
          }

          val pW = cWord / (totalCount + 1.0)
          val pTgW = cTagWord / cWord
          val pTag = indexedLabelCounts(cur) / totalCount
          import math._
          val result = log(pW) + log(pTgW) - log(pTag)
          assert(cTagWord == 0 || result > Double.NegativeInfinity)
          result
        } else {
          val denom = indexedLabelCounts(cur)
          if (denom == 0.0) Double.NegativeInfinity
          else {
            val x = math.log(emissions(labelIndex.get(cur), words(pos)) / denom)
            assert(!x.isNaN, emissions(labelIndex.get(cur),words(pos)) +" " +  denom)
            x
          }
        }

        def validSymbols(pos: Int): Set[Int] = validSyms(pos)
      }

      def labelIndex: Index[L] = li
      val allSyms = Set.empty ++ (0 until labelIndex.size)

      def startSymbol: L = startLabel
    }
  }
}
