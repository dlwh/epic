package epic.sequences

import breeze.util.{OptionIndex, Index}
import epic.trees.Span
import breeze.numerics
import epic.sequences.SemiCRF.Marginal
import breeze.features.FeatureVector
import epic.framework.{VisitableMarginal, ModelObjective, Feature}
import java.util
import collection.mutable.ArrayBuffer
import util.concurrent.ConcurrentHashMap
import collection.immutable.BitSet
import breeze.collection.mutable.TriangularArray
import java.io.{ObjectInputStream, IOException}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.CachedBatchDiffFunction
import breeze.linalg.{softmax, DenseVector}
import epic.constraints.{TagConstraints, LabeledSpanConstraints}
import epic.constraints.LabeledSpanConstraints.NoConstraints
import epic.util.Optional
import epic.features.{WordFeaturizer, SurfaceFeaturizer}

/**
 * A Semi-Markov Linear Chain Conditional Random Field, that is, the length
 * of time spent in a state may be longer than 1 tick. Useful for field segmentation or NER.
 *
 * As usual in Epic, all the heavy lifting is done in the companion object and Marginals.
 * @author dlwh
 */
@SerialVersionUID(1L)
trait SemiCRF[L, W] extends Serializable {
  def scorer(w: IndexedSeq[W]): SemiCRF.Anchoring[L, W]
  def labelIndex: OptionIndex[L]

  def marginal(w: IndexedSeq[W]) = {
     SemiCRF.Marginal(scorer(w))
  }

  def goldMarginal(segmentation: IndexedSeq[(L,Span)], w: IndexedSeq[W]):Marginal[L, W] = {
    SemiCRF.Marginal.goldMarginal(scorer(w), segmentation)
  }

  def bestSequence(w: IndexedSeq[W], id: String = ""): Segmentation[L, W] = {
    SemiCRF.posteriorDecode(marginal(w), id)
  }

}

object SemiCRF {


  def buildSimple[L](data: IndexedSeq[Segmentation[L, String]],
                     gazetteer: Gazetteer[Any, String] = Gazetteer.empty[Any, String],
                     wordFeaturizer: Optional[WordFeaturizer[String]] = None,
                     spanFeaturizer: Optional[SurfaceFeaturizer[String]] = None,
                     opt: OptParams = OptParams(regularization = 1.0)):SemiCRF[L, String] = {
    val model: SemiCRFModel[L, String] = new SegmentationModelFactory[L](gazetteer = gazetteer, wordFeaturizer = wordFeaturizer, spanFeaturizer = spanFeaturizer).makeModel(data)

    val obj = new ModelObjective(model, data)
    val cached = new CachedBatchDiffFunction(obj)
    val weights = opt.minimize(cached, obj.initialWeightVector(false))
//    GradientTester.test(cached, weights, randFraction = 1.0, toString={(i: Int) => model.featureIndex.get(i).toString}, tolerance=0.0)
    val crf = model.extractCRF(weights)

    crf
  }

  def buildIOModel[L](data: IndexedSeq[Segmentation[L, String]],
                      gazetteer: Gazetteer[Any, String] = Gazetteer.empty[Any, String],
                      opt: OptParams = OptParams()): SemiCRF[Unit, String] = {
    val fixedData: IndexedSeq[Segmentation[Unit, String]] = data.map{s =>
      s.copy(segments=s.segments.map{case (l,span) => ((), span)})
    }
    buildSimple(fixedData, gazetteer, opt = opt)
  }

  def fromCRF[L, W](crf: CRF[L, W]):SemiCRF[L, W] = new SemiCRF[L, W] {

    def scorer(w: IndexedSeq[W]): Anchoring[L, W] = new Anchoring[L, W] {
      val anch = crf.anchor(w)

      val constraints: LabeledSpanConstraints[L] = {
        LabeledSpanConstraints.fromTagConstraints(anch)
      }

      def words: IndexedSeq[W] = w

      def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int): Double = {
        if (prev == labelIndex(None) && begin == 0) {
          scoreTransition(labelIndex.apply(Some(crf.startSymbol)), cur, begin, end)
        } else if (end - begin != 1 || prev == crf.labelIndex.size || cur == crf.labelIndex.size) {
          Double.NegativeInfinity
        } else {
          anch.scoreTransition(begin, prev, cur)
        }
      }

      val labelIndex = new OptionIndex(crf.labelIndex)

    }

    override def labelIndex: OptionIndex[L] = new OptionIndex(crf.labelIndex)
  }


  /**
   * An Anchoring encodes all the information needed to score Semimarkov models.
   *
   * In particular, it can score transitions between a previous label (prev)
   * and the next label, which spans from begin to end.
   * @tparam L
   * @tparam W
   */
  trait Anchoring[L, W] {
    def words : IndexedSeq[W]
    def length: Int = words.length
    def constraints: LabeledSpanConstraints[L]
    def maxSegmentLength(label: Int): Int = if (label >= labelIndex.size - 1) 1 else constraints.maxSpanLengthForLabel(label)
    def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int):Double
    def labelIndex: OptionIndex[L]

    def ignoreTransitionModel: Boolean = false

    def *(other: Anchoring[L, W]):Anchoring[L, W] = {
      (this, other) match {
        case (x: IdentityAnchoring[L, W], _) => other
        case (_, x: IdentityAnchoring[L, W]) => this
        case (x, y) => new ProductAnchoring(this, other)
      }
    }
  }

  /**
   * A visitor used by [[epic.sequences.SemiCRF.Marginal]] for giving
   * marginal probabilities over labeled spans.
   * @tparam L
   * @tparam W
   */
  trait TransitionVisitor[L, W] {
    def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double)
  }

  trait Marginal[L, W] extends VisitableMarginal[TransitionVisitor[L, W]] {

    def anchoring: Anchoring[L, W]
    def words: IndexedSeq[W] = anchoring.words
    def length: Int = anchoring.length
    /** Visits spans with non-zero score, useful for expected counts */
    def visit( f: TransitionVisitor[L, W])

    /** normalized probability of seeing segment with transition */
    def transitionMarginal(prev:Int, cur: Int, begin: Int, end: Int):Double
    def logPartition: Double

    def spanMarginal(cur: Int, begin: Int, end: Int): Double = {
      var prev = 0
      val numLabels: Int = anchoring.labelIndex.size
      var sum = 0.0
      while (prev <  numLabels) {
        sum += transitionMarginal(prev, cur, begin, end)
        prev += 1
      }
      sum
    }
    def spanMarginal(begin: Int, end: Int):DenseVector[Double] = DenseVector.tabulate(anchoring.labelIndex.size)(spanMarginal(_, begin, end))

    def computeSpanConstraints(threshold: Double = 1E-5):LabeledSpanConstraints[L] = {
      val spanMarginals = TriangularArray.fill(length+1)(new Array[Double](anchoring.labelIndex.size))

      this visit new TransitionVisitor[L, W] {
        def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double)  {
          spanMarginals(begin, end)(cur) += count
        }
      }

      val allowedLabels = spanMarginals.map {  arr =>
         BitSet.empty ++ (0 until arr.length).filter(i => arr(i) >= threshold)
//           BitSet.empty ++ (0 until arr.length)
      }

      LabeledSpanConstraints(allowedLabels)
    }

    def hasSupportOver(m: Marginal[L, W]):Boolean = {
      object FailureException extends Exception
      try {
        m visit new TransitionVisitor[L, W] {
          def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double) {
            if (count >= 0.0 && transitionMarginal(prev, cur, begin, end)  <= 0.0) {
              throw FailureException
            }
          }
        }
        true
      } catch {
        case FailureException => false
      }
    }

    def decode:String = {
      val buf = new StringBuilder()
      this visit new TransitionVisitor[L, W] {
        def visitTransition(prev: Int, cur: Int, begin: Int, end: Int, count: Double) {
          if (count != 0) {
            buf ++= s"${anchoring.labelIndex.get(prev)} ${anchoring.labelIndex.get(cur)} ($begin,$end) $count\n"
          }
        }
      }
      buf.result()
    }
  }

  object Marginal {

    def maxDerivationMarginal[L, W](scorer: Anchoring[L, W]):Marginal[L, W] = {
      val maxDerivation: Segmentation[L, W] = viterbi(scorer)
      goldMarginal(scorer,maxDerivation.label)
    }

    def apply[L, W](scorer: Anchoring[L, W]):Marginal[L, W] = {

      val forwardScores: Array[Array[Double]] = this.forwardScores(scorer)
      val backwardScore: Array[Array[Double]] = this.backwardScores(scorer, forwardScores)
      val partition = softmax(forwardScores.last)
      val _s = scorer


      new Marginal[L, W] {

        def anchoring: Anchoring[L, W] = _s

        /** Visits spans with non-zero score, useful for expected counts */
        def visit(f: TransitionVisitor[L, W]) {
          val numLabels = scorer.labelIndex.size

          var begin = length - 1
          while (begin >= 0) {
            var prevLabel = 0
            while (prevLabel < numLabels) {
              val forwardPrev = forwardScores(begin)(prevLabel)
              if (forwardPrev != Double.NegativeInfinity) {
                var end = math.min(length, anchoring.constraints.maxSpanLengthStartingAt(begin) + begin)
                while (end > begin) {
                  if (anchoring.constraints.isAllowedSpan(begin, end)) {
                    var label = 0
                    while (label < numLabels) {
                      if (anchoring.constraints.isAllowedLabeledSpan(begin, end, label)) {
                        val prevScore = backwardScore(end)(label)
                        if (anchoring.maxSegmentLength(label) >= end - begin && prevScore != Double.NegativeInfinity) {
                          val score = transitionMarginal(prevLabel, label, begin, end)
                          if (score != 0.0) {
                            f.visitTransition(prevLabel, label, begin, end, score)
                          }
                        }
                      }

                      label += 1
                    }
                  }
                  end -= 1
                }
              }

              prevLabel += 1
            }

            begin -= 1

          }

        }


        /** Log-normalized probability of seing segment with transition */
        def transitionMarginal(prev: Int, cur: Int, begin: Int, end: Int): Double = {
          val withoutTrans = forwardScores(begin)(prev) + backwardScore(end)(cur)
          if (withoutTrans.isInfinite) 0.0
          else math.exp(withoutTrans + anchoring.scoreTransition(prev, cur, begin, end) - logPartition)
        }

        def logPartition: Double = partition
      }

    }

    def goldMarginal[L, W](scorer: Anchoring[L, W], segments: IndexedSeq[(L,Span)]):Marginal[L, W] = {
      var lastSymbol = scorer.labelIndex(None)
      var score = 0.0
      var lastEnd = 0
      val goldEnds = Array.fill(scorer.length)(-1)
      val goldLabels = Array.fill(scorer.length)(-1)
      val goldPrevLabels = Array.fill(scorer.length)(-1)
      val segmentation: Segmentation[L, W] = new Segmentation(segments, scorer.words)
      for ( (l,span) <- segmentation.segmentsWithOutside) {
        assert(span.begin == lastEnd)
        val symbol = scorer.labelIndex(l)
        assert(symbol != -1, s"$l not in index: ${scorer.labelIndex}")
        assert(scorer.constraints.isAllowedLabeledSpan(span.begin, span.end, symbol))
        score += scorer.scoreTransition(lastSymbol, symbol, span.begin, span.end)
        assert(!score.isInfinite, " " + segments + " " + l + " " + span)
        goldEnds(span.begin) = span.end
        goldLabels(span.begin) = symbol
        goldPrevLabels(span.begin) = lastSymbol
        lastSymbol = symbol
        lastEnd = span.end
      }

      val s = scorer

      new Marginal[L, W] {

        def anchoring: Anchoring[L, W] = s

        /** Visits spans with non-zero score, useful for expected counts */
        def visit(f: TransitionVisitor[L, W]) {
          var lastSymbol = scorer.labelIndex(None)
          var lastEnd = 0
          for ( (l,span) <- segmentation.segmentsWithOutside) {
            assert(span.begin == lastEnd)
            val symbol = scorer.labelIndex(l)
            f.visitTransition(lastSymbol, symbol, span.begin, span.end, 1.0)
            lastEnd = span.end
            lastSymbol = symbol
          }

        }

        /** normalized probability of seeing segment with transition */
        def transitionMarginal(prev: Int, cur: Int, begin: Int, end: Int): Double = {
          numerics.I(goldEnds(begin) == end && goldLabels(begin) == cur && goldPrevLabels(begin) == prev)
        }


        def logPartition: Double = score
      }
    }

    /**
     *
     * @param anchoring
     * @return forwardScore(end position)(label) = forward score of ending a segment labeled label in position end position
     */
    private def forwardScores[L, W](anchoring: SemiCRF.Anchoring[L, W]): Array[Array[Double]] = {
      val length = anchoring.length
      val numLabels = anchoring.labelIndex.size
      // total weight (logSum) for ending in pos with label l.
      val forwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
      forwardScores(0)(anchoring.labelIndex(None)) = 0.0

      val accumArray = new Array[Double](numLabels * length)

      var end = 1
      while (end <= length) {
        var label = 0
        while (label < numLabels) {
          var acc = 0
          var begin = math.max(end - anchoring.maxSegmentLength(label), 0)
          while (begin < end) {
            if (anchoring.constraints.isAllowedLabeledSpan(begin, end, label)) {
              var prevLabel = 0
              if (anchoring.ignoreTransitionModel) {
                prevLabel = -1 // ensure that you don't actually need the transition model
                val prevScore = softmax.array(forwardScores(begin), forwardScores(begin).length)
                if (prevScore != Double.NegativeInfinity) {
                  val score = anchoring.scoreTransition(prevLabel, label, begin, end) + prevScore
                  if (score != Double.NegativeInfinity) {
                    accumArray(acc) = score
                    acc += 1
                  }
                }
              } else {
                while (prevLabel < numLabels) {
                  val prevScore = forwardScores(begin)(prevLabel)
                  if (prevScore != Double.NegativeInfinity) {
                    val score = anchoring.scoreTransition(prevLabel, label, begin, end) + prevScore
                    if (score != Double.NegativeInfinity) {
                      accumArray(acc) = score
                      acc += 1
                    }
                  }

                  prevLabel += 1
                }
              }
            }

            begin += 1
          }
          forwardScores(end)(label) = softmax.array(accumArray, acc)
          label += 1
        }

        end += 1
      }
      forwardScores
    }

    /**
     * computes the sum of all derivations, starting from a label that ends at pos, and ending
     * at the end of the sequence
     * @param anchoring anchoring to score spans
     * @tparam L label type
     * @tparam W word type
     * @return backwardScore(pos)(label)
     */
    private def backwardScores[L, W](anchoring: SemiCRF.Anchoring[L, W], forwardScores: Array[Array[Double]]): Array[Array[Double]] = {
      val length = anchoring.length
      val numLabels = anchoring.labelIndex.size
      // total completion weight (logSum) for starting from an end at pos with label l
      val backwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
      util.Arrays.fill(backwardScores(length), 0.0)

      val maxOfSegmentLengths = (0 until numLabels).map(anchoring.maxSegmentLength _).max

      val accumArray = new Array[Double](numLabels * math.min(maxOfSegmentLengths, length))

      var begin = length - 1
      while (begin >= 0) {
        var prevLabel = 0
        while (prevLabel < numLabels) {
          if (forwardScores(begin)(prevLabel) != Double.NegativeInfinity) {
            var acc = 0 // index into accumArray
            var end = math.min(anchoring.constraints.maxSpanLengthStartingAt(begin) + begin, length)
            while (end > begin) {
              if (anchoring.constraints.isAllowedSpan(begin, end)) {
                var label = 0
                while (label < numLabels) {
                  if (anchoring.constraints.isAllowedLabeledSpan(begin, end, label)) {
                    val prevScore = backwardScores(end)(label)
                    if (anchoring.maxSegmentLength(label) >= end - begin && prevScore != Double.NegativeInfinity) {
                      val score = anchoring.scoreTransition(prevLabel, label, begin, end) + prevScore
                      if (score != Double.NegativeInfinity) {
                        accumArray(acc) = score
                        acc += 1
                      }
                    }
                  }

                  label += 1
                }
              }
              end -= 1
            }

            if (acc > 0)
              backwardScores(begin)(prevLabel) = softmax(new DenseVector(accumArray, 0, 1, acc))
          }
          prevLabel += 1
        }

        begin -= 1

      }

      backwardScores
    }



  }

  trait ConstraintSemiCRF[L, W] extends SemiCRF[L, W] with LabeledSpanConstraints.Factory[L, W] {
    def constraints(w: IndexedSeq[W]): LabeledSpanConstraints[L]
    def constraints(seg: Segmentation[L,W], keepGold: Boolean = true): LabeledSpanConstraints[L]
  }

  @SerialVersionUID(1L)
  class IdentityConstraintSemiCRF[L, W](val labelIndex: OptionIndex[L]) extends ConstraintSemiCRF[L, W] with Serializable { outer =>
    def scorer(w: IndexedSeq[W]) = new Anchoring[L,W]() {
      def words = w
      def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int) = 0.0
      def labelIndex = outer.labelIndex

      def constraints: LabeledSpanConstraints[L] = NoConstraints
    }


    def constraints(w: IndexedSeq[W]) = NoConstraints

    def constraints(seg: Segmentation[L, W], keepGold: Boolean) = NoConstraints
  }

  @SerialVersionUID(1L)
  class BaseModelConstraintSemiCRF[L, W](val crf: SemiCRF[L, W], val threshold: Double = 1E-5) extends ConstraintSemiCRF[L, W] with Serializable {
    def labelIndex = crf.labelIndex

    // TODO: make weak
    @transient
    private var cache = new ConcurrentHashMap[IndexedSeq[W], LabeledSpanConstraints[L]]()

    // Don't delete.
    @throws(classOf[IOException])
    @throws(classOf[ClassNotFoundException])
    private def readObject(oin: ObjectInputStream) {
      oin.defaultReadObject()
      cache = new ConcurrentHashMap[IndexedSeq[W], LabeledSpanConstraints[L]]()
    }

    def constraints(w: IndexedSeq[W]): LabeledSpanConstraints[L] = {
      var c = cache.get(w)
      if (c eq null) {
        c = crf.marginal(w).computeSpanConstraints(threshold)
        cache.put(w, c)
      }

      c
    }

    def constraints(seg: Segmentation[L,W], keepGold: Boolean = true): LabeledSpanConstraints[L] = {
      val orig: LabeledSpanConstraints[L]= constraints(seg.words)
      if (keepGold) {
        orig | crf.goldMarginal(seg.segments, seg.words).computeSpanConstraints()
      } else {
        orig
      }
    }


    def scorer(w: IndexedSeq[W]): Anchoring[L, W] = {
      val c = constraints(w)

      new Anchoring[L, W] {
        def words: IndexedSeq[W] = w


        def constraints: LabeledSpanConstraints[L] = c

        def labelIndex:OptionIndex[L] = crf.labelIndex

        def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int): Double =
          numerics.logI(c.isAllowedLabeledSpan(begin, end, cur))

      }

    }
  }


  trait IndexedFeaturizer[L, W] {
    def anchor(w: IndexedSeq[W]):AnchoredFeaturizer[L, W]


    def labelIndex: OptionIndex[L]
    def featureIndex: Index[Feature]

    def hasTransitionFeatures: Boolean = true
  }

  trait AnchoredFeaturizer[L, W] {
    def featureIndex: Index[Feature]
    def featuresForTransition(prev: Int, cur: Int, begin: Int, end: Int):FeatureVector
  }


  def viterbi[L, W](anchoring: Anchoring[L ,W], id: String=""):Segmentation[L, W] = {
    val length = anchoring.length
    val numLabels = anchoring.labelIndex.size
    // total weight (logSum) for ending in pos with label l.
    val forwardScores = Array.fill(length+1, numLabels)(Double.NegativeInfinity)
    val forwardLabelPointers = Array.fill(length+1, numLabels)(-1)
    val forwardBeginPointers = Array.fill(length+1, numLabels)(-1)
    forwardScores(0)(anchoring.labelIndex(None)) = 0.0

    var end = 1
    while (end <= length) {
      var label = 0
      while (label < numLabels) {

        var begin = math.max(end - anchoring.maxSegmentLength(label), 0)

        while (begin < end) {
          if (anchoring.constraints.isAllowedLabeledSpan(begin, end, label)) {
            var prevLabel = 0
            while (prevLabel < numLabels) {
              val prevScore = forwardScores(begin)(prevLabel)
              if (prevScore != Double.NegativeInfinity) {
                val score = anchoring.scoreTransition(prevLabel, label, begin, end) + prevScore
                if (score > forwardScores(end)(label)) {
                  forwardScores(end)(label) = score
                  forwardLabelPointers(end)(label) = prevLabel
                  forwardBeginPointers(end)(label) = begin
                }
              }

              prevLabel += 1
            }
          }
          begin += 1
        }
        label += 1
      }

      end += 1
    }
    val segments = ArrayBuffer[(L, Span)]()
    def rec(end: Int, label: Int) {
      if (end != 0) {
        val bestStart = forwardBeginPointers(end)(label)
        anchoring.labelIndex.get(label).foreach { l =>
          segments += (l -> Span(bestStart, end))
        }
        rec(bestStart, forwardLabelPointers(end)(label))
      }

    }
    rec(length, (0 until numLabels).maxBy(forwardScores(length)(_)))

    Segmentation(segments.reverse, anchoring.words, id)
  }


  def posteriorDecode[L, W](m: Marginal[L, W], id: String = "") = {
    val length = m.length
    val numLabels = m.anchoring.labelIndex.size
    val forwardScores = Array.fill(length+1, numLabels)(0.0)
    val forwardLabelPointers = Array.fill(length+1, numLabels)(-1)
    val forwardBeginPointers = Array.fill(length+1, numLabels)(-1)
    forwardScores(0)(m.anchoring.labelIndex(None)) = 1.0

    var end = 1
    while (end <= length) {
      var label = 0
      while (label < numLabels) {
        var begin = math.max(end - m.anchoring.maxSegmentLength(label), 0)
        while (begin < end) {
          var prevLabel = 0
          while (prevLabel < numLabels) {
            val prevScore = forwardScores(begin)(prevLabel)
            if (prevScore != 0.0) {
              val score = m.transitionMarginal(prevLabel, label, begin, end) + prevScore
              if (score > forwardScores(end)(label)) {
                forwardScores(end)(label) = score
                forwardLabelPointers(end)(label) = prevLabel
                forwardBeginPointers(end)(label) = begin
              }
            }

            prevLabel += 1
          }
          begin += 1
        }
        label += 1
      }

      end += 1
    }
    val segments = ArrayBuffer[(L, Span)]()
    def rec(end: Int, label: Int) {
      if (end != 0) {
        val bestStart = forwardBeginPointers(end)(label)
        m.anchoring.labelIndex.get(label).foreach { l =>
          segments += (l -> Span(bestStart, end))
        }

        rec(bestStart, forwardLabelPointers(end)(label))
      }

    }
    rec(length, (0 until numLabels).maxBy(forwardScores(length)(_)))

    Segmentation(segments.reverse, m.words, id)
  }

  case class ProductAnchoring[L, W](a: Anchoring[L ,W], b: Anchoring[L, W]) extends Anchoring[L, W] {
    if ((a.labelIndex ne b.labelIndex) && (a.labelIndex != b.labelIndex)) throw new IllegalArgumentException("Elements of product anchoring must have the same labelIndex!")

    def words: IndexedSeq[W] = a.words

    val constraints: LabeledSpanConstraints[L] = a.constraints & b.constraints

    def scoreTransition(prev: Int, cur: Int, begin: Int, end: Int): Double = {
      var score = a.scoreTransition(prev, cur, begin, end)
      if (score != Double.NegativeInfinity) {
        score += b.scoreTransition(prev, cur, begin, end)
      }
      score
    }

    def labelIndex = a.labelIndex
  }

  class IdentityAnchoring[L, W](val words: IndexedSeq[W], val labelIndex: OptionIndex[L], val constraints: LabeledSpanConstraints[L]) extends Anchoring[L, W] {
    def scoreTransition(prev: Int, cur: Int, beg: Int, end: Int): Double = 0.0

    def canStartLongSegment(pos: Int): Boolean = true
  }

}

