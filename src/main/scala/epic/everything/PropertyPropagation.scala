package epic.everything

import breeze.linalg._
import breeze.numerics._
import epic.framework._
import breeze.util.{Index, Lens}
import breeze.collection.mutable.TriangularArray


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
      def centralLens: Lens[SpanBeliefs, Beliefs[T]] = l1
      def satelliteLenses = IndexedSeq(l2)

      val featureIndex: Index[Feature] =  fi

      val grounding = new AssociationAnchoring[T, U] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int): Array[Int] = features(p1)(p2)
      }

      override def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, U] = grounding
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

    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence, logResults:Boolean): EvaluationResult = {
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

    def score(grounding: AssociationAnchoring[T, U], ass1: Int, satIndex: Int, ass2: Int) = {
      dot(weights, grounding.featuresFor(ass1, satIndex: Int, ass2))
    }

    def scores(grounding: AssociationAnchoring[T, U], b1: Beliefs[_], index: Int, b2: Beliefs[_]) = {
      val result = DenseMatrix.zeros[Double](b1.size, b2.size)
      result := Double.NegativeInfinity
      var p1 = 0
      while (p1 < result.rows) {
        var p2 = 0
        if (b1.beliefs(p1) > 1E-6)
          while (p2 < result.cols) {
            if(b2.beliefs(p2) > 1E-6)
              result(p1, p2) = score(grounding, p1, index, p2) + math.log(b2.beliefs(p2))
            p2 += 1
          }
        p1 += 1
      }


      result
    }

    def marginal(sentence: FeaturizedSentence, sentenceBeliefs: SentenceBeliefs): (Marginal) = {
      var logPartition = 0.0
      val spans = TriangularArray.tabulate(sentence.length + 1) { (begin, end) =>
        val current = sentenceBeliefs.spanBeliefs(begin, end)
        if (begin == end || (current eq null))  {
          null
        } else {
          val grounding = scorer.anchor(sentence, current, begin, end)
          val b1 = scorer.centralLens(current)
          val potentials = for ( (lens2, index) <- scorer.satelliteLenses.zipWithIndex) yield {
            val b2 = lens2(current)
            scores(grounding, b1, index, b2)
          }

          // unnormalized marginal for central a
          val centralMarginal = potentials.map(softmax(_, Axis._1)).reduceLeft(_ += _)
          centralMarginal += log(b1.beliefs)
          val partition = softmax(centralMarginal)
          logPartition += partition
          assert(!partition.isInfinite, f"$partition%.3E $b1 $potentials")
          assert(!partition.isNaN, f"$partition%.3E $b1 $potentials")
          potentials.foreach(m => edgeMarginal(m, centralMarginal))
          val edgeMarginals:IndexedSeq[DenseMatrix[Double]] = potentials
          exp.inPlace(centralMarginal -= partition)
          SpanMarginal(grounding, centralMarginal, edgeMarginals)
        }
      }

      val marginal =  new Marginal(logPartition, spans)
      assert(!marginal.logPartition.isNaN)
      assert(!marginal.logPartition.isInfinite)
      marginal
    }

    private def edgeMarginal(m: DenseMatrix[Double], marginal: DenseVector[Double]) {
      var p1 = 0
      while (p1 < m.rows) {
        val ascore = marginal(p1)
        var p2 = 0
        while (p2 < m.cols) {
          m(p1,p2) += ascore
          p2 += 1
        }
        p1 += 1
      }
      val partition = softmax(m)
//      assert( math.abs(partition - otherPartition) < 1E-4, partition + " " + otherPartition)
      m -= partition
      exp.inPlace(m)
    }

    def goldMarginal(v: FeaturizedSentence, aug: SentenceBeliefs): (Marginal) = marginal(v, aug)

    def emptyCounts = StandardExpectedCounts.zero(scorer.featureIndex)

    def countsFromMarginal(sentence: FeaturizedSentence, sentenceMarginal: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      accum.loss += sentenceMarginal.logPartition * scale
      for {
        begin <- 0 until sentence.length
        end <- (begin+1) to sentence.length
      } {
        val spanMarginal = sentenceMarginal.spans(begin, end)
        val anchoring = spanMarginal.anchoring
        if (spanMarginal == null || begin == end)  {
          null
        } else for( (current, index) <- sentenceMarginal.spans(begin, end).satellites.zipWithIndex ) {
          var p1 = 0
          while (p1 < current.rows) {
            var p2 = 0
            while (p2 < current.cols) {
              val features = anchoring.featuresFor(p1, index, p2)
              for(f <- features) {
                accum.counts(f) += scale * current(p1,p2)
              }
              p2 += 1
            }
            p1 += 1
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
          val old1: Beliefs[T] = scorer.centralLens.get(old)
          var newBeliefs = scorer.centralLens.set(old, old1.updated(current.centralMarginal))
          for ( (lens2, index) <- scorer.satelliteLenses.zipWithIndex) {
            val old2: Beliefs[U] = lens2.get(newBeliefs)
            val marginalizedSat: DenseVector[Double] = sum(current.satellites(index).t, Axis._1)
            assert(marginalizedSat.length == old2.property.size)
            assert((sum(marginalizedSat) - 1.0).abs < 1E-6, marginalizedSat.toString + " " + old2)
            newBeliefs = lens2.set(newBeliefs, old2.updated(marginalizedSat))
          }
          newBeliefs
        }
      }

      oldBeliefs.copy(spans=newSpans)
    }
  }


  trait Featurizer[T, U] {
    def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int):FeatureAnchoring[T, U]
  }

  trait FeatureAnchoring[T, U] {
    def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Feature]
  }


  trait AssociationFeaturizer[T, U] {
    def centralLens: Lens[SpanBeliefs, Beliefs[T]]
    def satelliteLenses: IndexedSeq[Lens[SpanBeliefs, Beliefs[U]]]
    def featureIndex: Index[Feature]
    def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int):AssociationAnchoring[T, U]
  }

  trait AssociationAnchoring[T, U] {
    def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Int]
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


  case class Marginal(logPartition: Double, spans: TriangularArray[SpanMarginal]) extends epic.framework.Marginal {
//    val logPartition = {
//      val part = spans.map(s => if (s eq null) 0.0 else math.log(breeze.linalg.sum(s))).data.sum
//      assert(!part.isNaN, spans.data.mkString("{", ",", "}"))
//      part
//    }
  }

  case class SpanMarginal(anchoring: AssociationAnchoring[_, _], centralMarginal: DenseVector[Double], satellites: IndexedSeq[DenseMatrix[Double]])


}


