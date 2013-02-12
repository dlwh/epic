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

  trait AssociationPacket[T] {
    def lens: Lens[SpanBeliefs, Beliefs[T]]
    def featureIndex: Index[_]
    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, assignment: Int): Array[Int]
  }

  trait SequencePacket[T] {
    def lens: Lens[SpanBeliefs, IndexedSeq[Beliefs[T]]]
    def featureIndex: Index[_]
    def featuresFor(fs: FeaturizedSentence, b: SpanBeliefs, begin: Int, end: Int, component: Int, assignment: Int): Array[Int]
  }

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
      def satelliteLenses = lift(l2)

      val featureIndex: Index[Feature] =  fi

      val grounding = new AssociationAnchoring[T, U] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int): Array[Int] = features(p1)(p2)
      }

      override def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, U] = grounding
    }

    new Model(beliefsFactory, assoc)
  }



  case class AssociationFeature[T, U](a: T, b: U) extends Feature

  def packetModel[T](beliefsFactory: SentenceBeliefs.Factory, packet1: AssociationPacket[T], otherPackets: IndexedSeq[AssociationPacket[_]]) = {

    val fi = Index[Feature]()
    val featureMatrices = for(packet2 <- otherPackets) yield {
      Array.tabulate(packet1.featureIndex.size, packet2.featureIndex.size) {(p1, p2) =>
       fi.index(AssociationFeature(packet1.featureIndex.get(p1), packet2.featureIndex.get(p2)))
      }
    }

    val assoc = new AssociationFeaturizer[T, Any] {
      def centralLens: Lens[SpanBeliefs, Beliefs[T]] = packet1.lens
      override val satelliteLenses = lift(otherPackets.map(_.lens):_*).asInstanceOf[Lens[SpanBeliefs, IndexedSeq[Beliefs[Any]]]]

      val featureIndex: Index[Feature] =  fi


      def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, Any] = new AssociationAnchoring[T, Any] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Int] = {
          val feats1 = packet1.featuresFor(sent, spanBeliefs, begin, end, p1)
          val feats2 = otherPackets(satIndex).featuresFor(sent, spanBeliefs, begin, end, p2)
          val arr = new Array[Int](feats1.length * feats2.length)
          var off = 0
          var i = 0
          while(i < feats1.length) {
            var j = 0
            val featureRow = featureMatrices(satIndex)(feats1(i))
            while(j < feats2.length) {
              arr(off) = featureRow(feats2(j))
              off += 1
              j += 1
            }
            i += 1
          }
          arr
        }

      }

    }

    new Model(beliefsFactory, assoc)
  }

  def sequencePacketModel[T, U](beliefsFactory: SentenceBeliefs.Factory, packet1: AssociationPacket[T], packet2: SequencePacket[U]) = {
    val fi = Index[Feature]()
    val featureMatrices =
      Array.tabulate(packet1.featureIndex.size, packet2.featureIndex.size) {(p1, p2) =>
       fi.index(AssociationFeature(packet1.featureIndex.get(p1), packet2.featureIndex.get(p2)))
      }

    val assoc = new AssociationFeaturizer[T, U] {
      def centralLens: Lens[SpanBeliefs, Beliefs[T]] = packet1.lens
      override val satelliteLenses = packet2.lens

      val featureIndex: Index[Feature] =  fi


      def anchor(sent: FeaturizedSentence, spanBeliefs: SpanBeliefs, begin: Int, end: Int): AssociationAnchoring[T, U] = new AssociationAnchoring[T, U] {
        def featuresFor(p1: Int, satIndex: Int, p2: Int):Array[Int] = {
          val feats1 = packet1.featuresFor(sent, spanBeliefs, begin, end, p1)
          val feats2 = packet2.featuresFor(sent, spanBeliefs, begin, end, satIndex, p2)
          val arr = new Array[Int](feats1.length * feats2.length)
          var off = 0
          var i = 0
          while(i < feats1.length) {
            var j = 0
            val featureRow = featureMatrices(feats1(i))
            while(j < feats2.length) {
              arr(off) = featureRow(feats2(j))
              off += 1
              j += 1
            }
            i += 1
          }
          arr
        }

      }

    }

    new Model(beliefsFactory, assoc)
  }




  /**
   *
   * @author dlwh
   */
  class Model[T, U](beliefsFactory: SentenceBeliefs.Factory, assoc: AssociationFeaturizer[T, U]) extends epic.framework.Model[FeaturizedSentence] {
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

  }


  class Inference[T, U](beliefsFactory: SentenceBeliefs.Factory,
                        weights: DenseVector[Double],
                        scorer: AssociationFeaturizer[T, U]) extends ProjectableInference[FeaturizedSentence, SentenceBeliefs] {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal


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
          val potentials = for ( (b2, index) <- scorer.satelliteLenses(current).zipWithIndex) yield {
            scores(grounding, b1, index, b2)
          }

          // unnormalized marginal for central a
          val centralMarginal = potentials.map(softmax(_, Axis._1)).reduceLeft(_ += _)
          var i = 0
          while(i < b1.beliefs.length) {
            val score = b1.beliefs(i)
            if (score >= 0)
              centralMarginal(i) += log(score)
            i += 1
          }
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
        if (spanMarginal == null || begin == end)  {
          null
        } else for( (current, index) <- sentenceMarginal.spans(begin, end).satellites.zipWithIndex ) {
          val anchoring = spanMarginal.anchoring
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
          val newSets:IndexedSeq[Beliefs[U]] = for ( (old2, index) <- scorer.satelliteLenses(newBeliefs).zipWithIndex) yield {
            val marginalizedSat: DenseVector[Double] = sum(current.satellites(index).t, Axis._1)
            assert(marginalizedSat.length == old2.property.size)
            assert((sum(marginalizedSat) - 1.0).abs < 1E-6, marginalizedSat.toString + " " + old2)
            old2.copy(beliefs=marginalizedSat)
          }
          newBeliefs = scorer.satelliteLenses.set(newBeliefs, newSets)
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
    def satelliteLenses: Lens[SpanBeliefs, IndexedSeq[Beliefs[U]]]
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


  private def lift[T, U](lens: Lens[T, U]*):Lens[T, IndexedSeq[U]] = new Lens[T, IndexedSeq[U]] {
    def get(t: T): IndexedSeq[U] = lens.toIndexedSeq.map(_.get(t))

    def set(t: T, u: IndexedSeq[U]): T = (lens zip u).foldLeft(t)( (b, lensu) => lensu._1.set(b, lensu._2))
  }
}


