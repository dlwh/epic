package epic.redux

import breeze.linalg.{DenseMatrix, DenseVector}
import epic.framework._
import breeze.util.{Index, Lens}
import breeze.collection.mutable.TriangularArray
import epic.everything.models.{Beliefs, SpanBeliefs, SentenceBeliefs, Property}
import epic.everything.models.SpanBeliefs


/**
 * PropertyPropagation associates two variables in the same span with on another, for
 * instance the label of the span and the ner type.
 */
object PropertyPropagation {

  /**
   * For properties that do not depend on the sentence, this is a model
   * for associating them.
   */
  def simpleModel[T, U](beliefsFactory: SentenceBeliefs.Factory,
                        prop1: Property[T], lens1: Lens[SpanBeliefs, Beliefs[T]],
                        prop2: Property[U], lens2: Lens[SpanBeliefs, Beliefs[U]]): PropertyPropagation.Model[T, U] = {
    val l1 = lens1
    val l2 = lens2

    val fi = Index[Feature]()
    val features = Array.tabulate(prop1.size, prop2.size){(p1,p2) =>
      val f = AssociationFeature(prop1.index.get(p1), prop2.index.get(p2))
      Array(fi.index(f))
    }


    val assoc = new AssociationFeaturizer[T, U] {
      def lens1: Lens[SpanBeliefs, Beliefs[T]] = l1
      def lens2: Lens[SpanBeliefs, Beliefs[U]] = l2

      val featureIndex: Index[Feature] =  fi

      val grounding = new AssociationAnchoring[T, U] {
        def featuresFor(p1: Int, p2: Int): Array[Int] = features(p1)(p2)
      }

      def anchor(sent: FeaturizedSentence, begin: Int, end: Int): AssociationAnchoring[T, U] = grounding
    }

    new Model(beliefsFactory, assoc)
  }

  case class AssociationFeature[T, U](a: T, b: U) extends Feature

  /**
   *
   * @author dlwh
   */
  class Model[T, U](beliefsFactory: SentenceBeliefs.Factory, assoc: AssociationFeaturizer[T, U]) extends EvaluableModel[FeaturizedSentence] {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Inference = PropertyPropagation.Inference[T, U]
    type Marginal = PropertyPropagation.Marginal

    def featureIndex: Index[Feature] = assoc.featureIndex

    def initialValueForFeature(f: Feature): Double = math.random * 1E-4 - 2

    def inferenceFromWeights(weights: DenseVector[Double]) = {
      new Inference(beliefsFactory, weights, assoc)
    }

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.loss -> ecounts.counts
    }

    case class EvaluationResult() extends epic.framework.EvaluationResult[EvaluationResult] {
      def +(other: EvaluationResult): EvaluationResult = this
    }

    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence): EvaluationResult = {
      EvaluationResult()
    }
  }


  class Inference[T, U](beliefsFactory: SentenceBeliefs.Factory,
                        weights: DenseVector[Double],
                        scorer: AssociationFeaturizer[T, U]) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal


    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = sent

    def apply(v1: FeaturizedSentence, v2: SentenceBeliefs): FeaturizedSentence = v1

    def baseAugment(sent: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(sent)
    }

    def score(grounding: AssociationAnchoring[T, U], ass1: Int, ass2: Int) = {
      math.exp(dot(weights, grounding.featuresFor(ass1, ass2)) )
    }

    def scores(grounding: AssociationAnchoring[T, U], b1: Beliefs[_], b2: Beliefs[_]) = {
      val result = DenseMatrix.zeros[Double](b1.size, b2.size)
      var p1 = 0
      while (p1 < result.rows) {
        var p2 = 0
        if (b1.beliefs(p1) != 0.0)
          while (p2 < result.cols) {
            result(p1, p2) = score(grounding, p1, p2) * b1.beliefs(p1) * b2.beliefs(p2)
            p2 += 1
          }
        p1 += 1
      }

      result
    }

    def marginal(sentence: FeaturizedSentence, sentenceBeliefs: SentenceBeliefs): (Marginal) = {
      val spans = TriangularArray.tabulate(sentence.length + 1) { (begin, end) =>
        val grounding = scorer.anchor(sentence, begin, end)
        val current = sentenceBeliefs.spanBeliefs(begin, end)
        if (begin == end || (current eq null))  {
          null
        } else {
          val b1 = scorer.lens1(current)
          val b2 = scorer.lens2(current)
          val r = scores(grounding, b1, b2)
          val partition = breeze.linalg.sum(r)
          assert(partition >= 0.0, f"$partition%.3E $b1 $b2")
          assert(!partition.isInfinite, f"$partition%.3E $b1 $b2")
          assert(!partition.isNaN, f"$partition%.3E $b1 $b2")
          r
        }
      }

      val marginal =  new Marginal(spans)
      assert(!marginal.logPartition.isNaN)
      assert(!marginal.logPartition.isInfinite)
      marginal
    }

    def goldMarginal(v: FeaturizedSentence, aug: SentenceBeliefs): (Marginal) = marginal(v, aug)

    def emptyCounts = StandardExpectedCounts.zero(scorer.featureIndex)

    def countsFromMarginal(sentence: FeaturizedSentence, sentenceMarginal: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      accum.loss += sentenceMarginal.logPartition * scale
      for {
        begin <- 0 until sentence.length
        end <- (begin+1) to sentence.length
      } {
        val anchoring = scorer.anchor(sentence, begin, end)
        val current = sentenceMarginal.spans(begin, end)
        if(current != null) {
          val partition = breeze.linalg.sum(current)
          if (begin == end || (current eq null))  {
            null
          } else {
            var p1 = 0
            while (p1 < current.rows) {
              var p2 = 0
              while (p2 < current.cols) {
                val features = anchoring.featuresFor(p1, p2)
                for(f <- features) {
                  accum.counts(f) += scale * current(p1,p2) / partition
                }
                p2 += 1
              }
              p1 += 1
            }
          }
        }
      }

      accum
    }

    // turns the marginal p(var1,var2) => q(var1)q(var2)
    def project(sent: FeaturizedSentence, myBeliefs: Marginal, oldBeliefs: SentenceBeliefs): SentenceBeliefs = {
      val newSpans = TriangularArray.tabulate(oldBeliefs.length+1) { (begin, end) =>
        val current = myBeliefs.spans(begin, end)
        if(current eq null) null
        else {
          val old: SpanBeliefs = oldBeliefs.spanBeliefs(begin, end)
          val old1: Beliefs[T] = scorer.lens1.get(old)
          val old2: Beliefs[U] = scorer.lens2.get(old)
          val marg1 = DenseVector.zeros[Double](current.rows)
          val marg2 = DenseVector.zeros[Double](current.cols)
          var p1 = 0
          while( p1 < current.rows) {
            var p2 = 0
            var rsum = 0.0
            if (old1(p1) != 0.0) {
              while(p2 < current.cols) {
                rsum += current(p1,p2)
                marg2(p2) += current(p1, p2)
                p2 += 1
              }
            }
            marg1(p1) = rsum
            p1 += 1
          }
          val partition = breeze.linalg.sum(marg1)
          marg1 /= partition
          marg2 /= partition

          val half = scorer.lens1.set(old, old1.copy(beliefs=marg1))
          scorer.lens2.set(half, old2.copy(beliefs=marg2))
        }
      }

      oldBeliefs.copy(spans=newSpans)
    }
  }



  trait AssociationFeaturizer[T, U] {
    def lens1: Lens[SpanBeliefs, Beliefs[T]]
    def lens2: Lens[SpanBeliefs, Beliefs[U]]
    def featureIndex: Index[Feature]
    def anchor(sent: FeaturizedSentence, begin: Int, end: Int):AssociationAnchoring[T, U]
  }

  trait AssociationAnchoring[T, U] {
    def featuresFor(p1: Int, p2: Int):Array[Int]
  }


  private def dot(weights: DenseVector[Double], features: Array[Int]) = {
    var i = 0
    var score = 0.0
    val w = weights.data
    while(i < features.length) {
      val f = features(i)
      if (f != -1)
        score += w(weights.offset + weights.stride * f)
      i += 1
    }
    score
  }


  case class Marginal(spans: TriangularArray[DenseMatrix[Double]]) extends epic.framework.Marginal {
    val logPartition = {
      val part = spans.map(s => if (s eq null) 0.0 else math.log(breeze.linalg.sum(s))).data.sum
      assert(!part.isNaN, spans.data.mkString("{", ",", "}"))
      part
    }
  }
}

