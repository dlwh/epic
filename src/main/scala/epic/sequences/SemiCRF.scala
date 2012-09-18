package epic.sequences

import breeze.util.Index
import epic.trees.Span
import breeze.numerics
import epic.sequences.SemiCRF.{Marginal, Anchoring}
import breeze.linalg.{HashVector, SparseVector}
import epic.framework.{StandardExpectedCounts, Feature}
import java.util

/**
 * A Semi-Markov Linear Chain Conditional Random Field, that is, the length
 * of time spent in a state may be longer than 1 tick. Useful for field segmentation or NER.
 * @author dlwh
 */
class SemiCRF[L, W](val model: SemiCRF.Grammar[L, W]) {

  def marginal(w: W) = {
     SemiCRF.Marginal(model.anchor(w))
  }

  def goldMarginal(segmentation: IndexedSeq[(L,Span)], w: W):Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal(model.anchor(w), segmentation, w)
  }


}

object SemiCRF {

  trait Grammar[L, W] {
    def anchor(w: W): Anchoring[L, W]
    def labelIndex: Index[L]
    def startSymbol: L
  }


  trait Anchoring[L, W] {
    def w : W
    def length: Int
    def maxSegmentLength(label: Int): Int
    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int):Double
    def labelIndex: Index[L]
    def startSymbol: L
  }


  trait TransitionVisitor[L, W] {
    def apply(prev: Int, cur: Int, beg: Int, end: Int, count: Double)
  }

  trait Marginal[L, W] {

    def anchoring: Anchoring[L, W]
    def w: W = anchoring.w
    def length: Int = anchoring.length
    /** Visits spans with non-zero score, useful for expected counts */
    def visit( f: TransitionVisitor[L, W])

    /** Log-normalized probability of seeing segment with transition */
    def transitionMarginal(prev:Int, cur: Int, beg: Int, end: Int):Double
    def logPartition: Double

    /** Log-normalized probability of seeing segment */
  }

  object Marginal {

    def apply[L, W](scorer: Anchoring[L, W]):Marginal[L, W] = {

      val forwardScores: Array[Array[Double]] = this.forwardScores(scorer)
      val backwardScore: Array[Array[Double]] = this.backwardScores(scorer)
      val partition = numerics.logSum(forwardScores.last)
      val _s = scorer


      new Marginal[L, W] {

        def anchoring: Anchoring[L, W] = _s

        /** Visits spans with non-zero score, useful for expected counts */
        def visit(f: TransitionVisitor[L, W]) {
          val numLabels = scorer.labelIndex.size
          var end = 1
          while (end <= length) {
            var label = 0
            while (label < numLabels) {

              var start = math.max(end - anchoring.maxSegmentLength(label), 0)
              while (start < end) {
                var prevLabel = 0
                while (prevLabel < numLabels) {
                  val score = math.exp(transitionMarginal(prevLabel, label, start, end))
                  if(score != 0.0)
                    f(prevLabel, label, start, end, score)
                  prevLabel += 1
                }
                start += 1
              }
              label += 1
            }

            end += 1
          }
        }


        /** Log-normalized probability of seing segment with transition */
        def transitionMarginal(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          val withoutTrans = forwardScores(beg)(prev) + backwardScore(end)(cur)
          if(withoutTrans.isInfinite) withoutTrans
          else withoutTrans + anchoring.scoreTransition(prev, cur, beg, end) - logPartition
        }

        def logPartition: Double = partition
      }

    }

    def goldMarginal[L, W](scorer: Anchoring[L, W], segmentation: IndexedSeq[(L,Span)], w: W):Marginal[L, W] = {
      var lastSymbol = scorer.labelIndex(scorer.startSymbol)
      var score = 0.0
      var lastEnd = 0
      val goldEnds = Array.fill(segmentation.last._2.end)(-1)
      val goldLabels = Array.fill(segmentation.last._2.end)(-1)
      val goldPrevLabels = Array.fill(segmentation.last._2.end)(-1)
      for( (l,span) <- segmentation) {
        assert(span.start == lastEnd)
        val symbol = scorer.labelIndex(l)
        score += scorer.scoreTransition(lastSymbol, symbol, span.start, span.end)
        goldEnds(span.start) = span.end
        goldLabels(span.start) = symbol
        goldPrevLabels(span.start) = lastSymbol
        lastSymbol = symbol
        lastEnd = span.end
      }

      val s = scorer

      new Marginal[L, W] {

        def anchoring: Anchoring[L, W] = s

        /** Visits spans with non-zero score, useful for expected counts */
        def visit(f: TransitionVisitor[L, W]) {
          var lastSymbol = scorer.labelIndex(scorer.startSymbol)
          var lastEnd = 0
          for( (l,span) <- segmentation) {
            assert(span.start == lastEnd)
            val symbol = scorer.labelIndex(l)
            f.apply(lastSymbol, symbol, span.start, span.end, 1.0)
            lastEnd = span.end
            lastSymbol = symbol
          }

        }

        /** Log-normalized probability of seing segment with transition */
        def transitionMarginal(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          numerics.logI(goldEnds(beg) == end && goldLabels(beg) == cur && goldPrevLabels(beg) == prev)
        }

        def logPartition: Double = score
      }
    }



    /**
     *
     * @param scorer
     * @return forwardScore(end position)(label) = forward score of ending a segment labeled label in position end position
     */
    private def forwardScores[L, W](scorer: SemiCRF.Anchoring[L, W]): Array[Array[Double]] = {
      val length = scorer.length
      val numLabels = scorer.labelIndex.size
      // total weight (logSum) for ending in pos with label l.
      val forwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
      forwardScores(0)(scorer.labelIndex(scorer.startSymbol)) = 0.0

      val accumArray = new Array[Double](numLabels * length)

      var end = 1
      while (end <= length) {
        var label = 0
        while (label < numLabels) {
          var acc = 0
          var start = math.max(end - scorer.maxSegmentLength(label), 0)
          while (start < end) {
            var prevLabel = 0
            while (prevLabel < numLabels) {
              val prevScore = forwardScores(start)(prevLabel)
              if (prevScore != Double.NegativeInfinity) {
                val score = scorer.scoreTransition(prevLabel, label, start, end) + prevScore
                accumArray(acc) = score
                acc += 1
              }

              prevLabel += 1
            }
            start += 1
          }
          forwardScores(end)(label) = numerics.logSum(accumArray, acc)
          label += 1
        }

        end += 1
      }
      forwardScores
    }

    /**
     * computes the sum of all derivations, starting from a label that ends at pos, and ending
     * at the end of the sequence
     * @param scorer
     * @tparam L
     * @tparam W
     * @return backwardScore(pos)(label)
     */
    private def backwardScores[L, W](scorer: SemiCRF.Anchoring[L, W]): Array[Array[Double]] = {
      val length = scorer.length
      val numLabels = scorer.labelIndex.size
      // total completion weight (logSum) for starting from an end at pos with label l
      val backwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
      util.Arrays.fill(backwardScores(length), 0.0)

      val maxOfSegmentLengths = (0 until numLabels).map(scorer.maxSegmentLength _).max

      val accumArray = new Array[Double](numLabels * maxOfSegmentLengths)
      var start = length - 1
      while(start >= 0) {
        var prevLabel = 0
        while(prevLabel < numLabels) {
          var acc = 0
          var end = math.min(length, start + maxOfSegmentLengths)
          while(end > start) {
            var label = 0
            while(label < numLabels) {
              val prevScore = backwardScores(end)(label)
              if (scorer.maxSegmentLength(label) >= end - start && prevScore != Double.NegativeInfinity) {
                val score = scorer.scoreTransition(prevLabel, label, start, end) + prevScore
                accumArray(acc) = score
                acc += 1
              }

              label += 1
            }
            end -= 1
          }

          backwardScores(start)(prevLabel) = numerics.logSum(accumArray, acc)
          prevLabel += 1
        }

        start -= 1

      }

      backwardScores
    }

  }

  trait IndexedFeaturizer[L, W] {
    def anchor(w: W):AnchoredFeaturizer[L, W]

    def startSymbol: L

    def labelIndex: Index[L]
    def featureIndex: Index[Feature]
  }

  trait AnchoredFeaturizer[L, W] {
    def featureIndex: Index[Feature]
    def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int):SparseVector[Double]
  }


}

