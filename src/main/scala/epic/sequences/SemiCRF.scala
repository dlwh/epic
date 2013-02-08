package epic.sequences

import breeze.util.Index
import epic.trees.Span
import breeze.numerics
import epic.sequences.SemiCRF.Marginal
import breeze.linalg.SparseVector
import epic.framework.{ModelObjective, Feature}
import java.util
import collection.mutable.ArrayBuffer
import util.concurrent.ConcurrentHashMap
import collection.immutable.BitSet
import breeze.collection.mutable.TriangularArray
import java.io.{ObjectInputStream, IOException}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.CachedBatchDiffFunction

/**
 * A Semi-Markov Linear Chain Conditional Random Field, that is, the length
 * of time spent in a state may be longer than 1 tick. Useful for field segmentation or NER.
 *
 * As usual in Epic, all the heavy lifting is done in the companion object and Marginals.
 * @author dlwh
 */
@SerialVersionUID(1L)
class SemiCRF[L, W](val model: SemiCRF.Grammar[L, W]) extends Serializable {

  def marginal(w: IndexedSeq[W]) = {
     SemiCRF.Marginal(model.anchor(w))
  }

  def goldMarginal(segmentation: IndexedSeq[(L,Span)], w: IndexedSeq[W]):Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal(model.anchor(w), segmentation)
  }

  def bestSequence(w: IndexedSeq[W], id: String = "") = {
    SemiCRF.viterbi(model.anchor(w), id)
  }


}

object SemiCRF {

  def buildSimple[L](data: IndexedSeq[Segmentation[L, String]],
                     startSymbol: L, outsideSymbol: L,
                     gazetteer: Gazetteer[Any, String] = Gazetteer.empty[Any, String],
                     opt: OptParams = OptParams()):SemiCRF[L, String] = {
    val model: SemiCRFModel[L, String] = new SegmentationModelFactory[L](startSymbol, outsideSymbol, gazetteer = gazetteer).makeModel(data)

    val obj = new ModelObjective(model, data)
    val cached = new CachedBatchDiffFunction(obj)
    val weights = opt.minimize(cached, obj.initialWeightVector(randomize = true))
    val crf = model.extractCRF(weights)

    crf
  }

  def buildIOModel[L](data: IndexedSeq[Segmentation[L, String]],
                      outsideSymbol: L,
                      gazetteer: Gazetteer[Any, String] = Gazetteer.empty[Any, String],
                      opt: OptParams = OptParams()): SemiCRF[Boolean, String] = {
    val fixedData: IndexedSeq[Segmentation[Boolean, String]] = data.map{s =>
      s.copy(segments=s.segments.map{case (l,span) => (l == outsideSymbol, span)})
    }
    buildSimple(fixedData, false, false, gazetteer, opt)
  }


  trait Grammar[L, W] {
    def anchor(w: IndexedSeq[W]): Anchoring[L, W]
    def labelIndex: Index[L]
    def startSymbol: L
  }

  trait Anchoring[L, W] {
    def words : IndexedSeq[W]
    def length: Int = words.length
    def maxSegmentLength(label: Int): Int
    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int):Double
    def labelIndex: Index[L]
    def startSymbol: L
  }

  trait TransitionVisitor[L, W] {
    def apply(prev: Int, cur: Int, beg: Int, end: Int, count: Double)
  }

  trait Marginal[L, W] extends epic.framework.Marginal {

    def anchoring: Anchoring[L, W]
    def words: IndexedSeq[W] = anchoring.words
    def length: Int = anchoring.length
    /** Visits spans with non-zero score, useful for expected counts */
    def visit( f: TransitionVisitor[L, W])

    /** normalized probability of seeing segment with transition */
    def transitionMarginal(prev:Int, cur: Int, beg: Int, end: Int):Double
    def logPartition: Double

    def spanMarginal(cur: Int, begin: Int, end: Int) = {
      var prev = 0
      val numLabels: Int = anchoring.labelIndex.size
      var sum = 0.0
      while(prev <  numLabels) {
        sum += transitionMarginal(prev, cur, begin, end)
        prev += 1
      }
      sum
    }

    def computeSpanConstraints(threshold: Double = 1E-5):SpanConstraints = {
      val spanMarginals = TriangularArray.fill(length+1)(new Array[Double](anchoring.labelIndex.size))

      this visit new TransitionVisitor[L, W] {
        def apply(prev: Int, cur: Int, beg: Int, end: Int, count: Double)  {
          spanMarginals(beg, end)(cur) += count
        }
      }

      val allowedLabels = spanMarginals.map {  arr =>
         BitSet.empty ++ (0 until arr.length).filter(i => arr(i) >= threshold)
//           BitSet.empty ++ (0 until arr.length)
      }

      val maxLengths = new Array[Int](anchoring.labelIndex.size)
      val allowedStarts = Array.fill(length)(collection.mutable.BitSet.empty)
      for(begin <- 0 until length; end <- (begin+1) to length) {
        for(l <- allowedLabels(begin, end)) {
          maxLengths(l) = math.max(maxLengths(l), end - begin)
          allowedStarts(begin) += l
        }
      }


      new SpanConstraints(maxLengths, allowedStarts.map(BitSet.empty ++ _), allowedLabels)
    }
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
                  val score = transitionMarginal(prevLabel, label, start, end)
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
          if(withoutTrans.isInfinite) 0.0
          else math.exp(withoutTrans + anchoring.scoreTransition(prev, cur, beg, end) - logPartition)
        }



        def logPartition: Double = partition
      }

    }

    def goldMarginal[L, W](scorer: Anchoring[L, W], segmentation: IndexedSeq[(L,Span)]):Marginal[L, W] = {
      var lastSymbol = scorer.labelIndex(scorer.startSymbol)
      var score = 0.0
      var lastEnd = 0
      val goldEnds = Array.fill(segmentation.last._2.end)(-1)
      val goldLabels = Array.fill(segmentation.last._2.end)(-1)
      val goldPrevLabels = Array.fill(segmentation.last._2.end)(-1)
      for( (l,span) <- segmentation) {
        assert(span.start == lastEnd)
        val symbol = scorer.labelIndex(l)
        assert(symbol != -1, s"$l not in index: ${scorer.labelIndex}")
        score += scorer.scoreTransition(lastSymbol, symbol, span.start, span.end)
        assert(!score.isInfinite, " " + segmentation + " " + l + " " + span)
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

        /** normalized probability of seeing segment with transition */
        def transitionMarginal(prev: Int, cur: Int, beg: Int, end: Int): Double = {
          numerics.I(goldEnds(beg) == end && goldLabels(beg) == cur && goldPrevLabels(beg) == prev)
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
                if(score != Double.NegativeInfinity)
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

  @SerialVersionUID(1L)
  case class SpanConstraints(maxLengths: Array[Int],
                             allowedStarts: Array[BitSet],
                             allowedLabels: TriangularArray[BitSet]) {
    def +(constraints: SpanConstraints) = {
      SpanConstraints(Array.tabulate(maxLengths.length)(i => maxLengths(i) max constraints.maxLengths(i)),
        allowedStarts zip constraints.allowedStarts map {case (a,b) => a | b},
        TriangularArray.tabulate(allowedStarts.length+1)((b,e) => allowedLabels(b,e) | constraints.allowedLabels(b, e))
      )
    }
  }

  @SerialVersionUID(1L)
  class ConstraintGrammar[L, W](val crf: SemiCRF[L, W], val threshold: Double = 1E-5) extends Grammar[L, W] with Serializable {
    def startSymbol: L = crf.model.startSymbol
    def labelIndex: Index[L] = crf.model.labelIndex

    // TODO: make weak
    @transient
    private var cache = new ConcurrentHashMap[IndexedSeq[W], SpanConstraints]()

    // Don't delete.
    @throws(classOf[IOException])
    @throws(classOf[ClassNotFoundException])
    private def readObject(oin: ObjectInputStream) {
      oin.defaultReadObject()
      cache = new ConcurrentHashMap[IndexedSeq[W], SpanConstraints]()
    }

    def constraints(w: IndexedSeq[W]): SpanConstraints = {
      var c = cache.get(w)
      if(c eq null) {
        c = crf.marginal(w).computeSpanConstraints(threshold)
        cache.put(w, c)
      }

      c
    }

    def constraints(seg: Segmentation[L,W], keepGold: Boolean = true): SpanConstraints = {
      val orig: SpanConstraints = constraints(seg.words)
      if(keepGold) {
        orig + crf.goldMarginal(seg.segments, seg.words).computeSpanConstraints()
      } else {
        orig
      }
    }


    def anchor(w: IndexedSeq[W]): Anchoring[L, W] = {
      val c = constraints(w)

      new Anchoring[L, W] {
        def words: IndexedSeq[W] = w

        def maxSegmentLength(label: Int): Int = c.maxLengths(label)

        def startSymbol: L = crf.model.startSymbol
        def labelIndex: Index[L] = crf.model.labelIndex

        def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double =
          numerics.logI(c.allowedLabels(beg, end).contains(cur))

      }

    }
  }


  trait IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]):AnchoredFeaturizer[L, W]

    def startSymbol: L

    def labelIndex: Index[L]
    def featureIndex: Index[Feature]
  }

  trait AnchoredFeaturizer[L, W] {
    def featureIndex: Index[Feature]
    def featuresForTransition(prev: Int, cur: Int, start: Int, end: Int):SparseVector[Double]
  }


  def viterbi[L, W](scorer: Anchoring[L ,W], id: String=""):Segmentation[L, W] = {
    val length = scorer.length
    val numLabels = scorer.labelIndex.size
    // total weight (logSum) for ending in pos with label l.
    val forwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
    val forwardLabelPointers = Array.fill(length+1, numLabels)(-1)
    val forwardBeginPointers = Array.fill(length+1, numLabels)(-1)
    forwardScores(0)(scorer.labelIndex(scorer.startSymbol)) = 0.0

    var end = 1
    while (end <= length) {
      var label = 0
      while (label < numLabels) {
        var start = math.max(end - scorer.maxSegmentLength(label), 0)
        while (start < end) {
          var prevLabel = 0
          while (prevLabel < numLabels) {
            val prevScore = forwardScores(start)(prevLabel)
            if (prevScore != Double.NegativeInfinity) {
              val score = scorer.scoreTransition(prevLabel, label, start, end) + prevScore
              if(score > forwardScores(end)(label)) {
                forwardScores(end)(label) = score
                forwardLabelPointers(end)(label) = prevLabel
                forwardBeginPointers(end)(label) = start
              }
            }

            prevLabel += 1
          }
          start += 1
        }
        label += 1
      }

      end += 1
    }
    val segments = ArrayBuffer[(L, Span)]()
    def rec(end: Int, label: Int) {
      if(end != 0) {
        val bestStart = forwardBeginPointers(end)(label)
        segments += (scorer.labelIndex.get(label) -> Span(bestStart, end))
        rec(bestStart, forwardLabelPointers(end)(label))
      }

    }
    rec(length, (0 until numLabels).maxBy(forwardScores(length)(_)))

    Segmentation(segments.reverse, scorer.words, id)
  }


  def posteriorDecode[L, W](m: Marginal[L, W], id: String = "") = {
    val length = m.length
    val numLabels = m.anchoring.labelIndex.size
    val forwardScores = Array.fill(length+1, numLabels)(0.0)
    val forwardLabelPointers = Array.fill(length+1, numLabels)(-1)
    val forwardBeginPointers = Array.fill(length+1, numLabels)(-1)
    forwardScores(0)(m.anchoring.labelIndex(m.anchoring.startSymbol)) = 1.0

    var end = 1
    while (end <= length) {
      var label = 0
      while (label < numLabels) {
        var start = math.max(end - m.anchoring.maxSegmentLength(label), 0)
        while (start < end) {
          var prevLabel = 0
          while (prevLabel < numLabels) {
            val prevScore = forwardScores(start)(prevLabel)
            if (prevScore != 0.0) {
              val score = m.transitionMarginal(prevLabel, label, start, end) + prevScore
              if(score > forwardScores(end)(label)) {
                forwardScores(end)(label) = score
                forwardLabelPointers(end)(label) = prevLabel
                forwardBeginPointers(end)(label) = start
              }
            }

            prevLabel += 1
          }
          start += 1
        }
        label += 1
      }

      end += 1
    }
    val segments = ArrayBuffer[(L, Span)]()
    def rec(end: Int, label: Int) {
      if(end != 0) {
        val bestStart = forwardBeginPointers(end)(label)
        segments += (m.anchoring.labelIndex.get(label) -> Span(bestStart, end))
        rec(bestStart, forwardLabelPointers(end)(label))
      }

    }
    rec(length, (0 until numLabels).maxBy(forwardScores(length)(_)))

    Segmentation(segments.reverse, m.words, id)
  }
}

