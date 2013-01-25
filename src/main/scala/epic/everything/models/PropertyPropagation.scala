package epic.everything.models

import epic.everything.{ProcessedDocument}
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.inference.bp
import epic.framework.{StandardExpectedCounts, Feature}
import breeze.util.{Encoder, Index, Lens}
import breeze.collection.mutable.TriangularArray
import java.io.{PrintWriter, FileWriter}


object PropertyPropagation {

  def simpleModel[T, U](beliefsFactory: DocumentBeliefs.Factory,
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

      def anchor(doc: ProcessedDocument, sent: Int, begin: Int, end: Int): AssociationAnchoring[T, U] = grounding
    }

    new Model(beliefsFactory, assoc)
  }

  case class AssociationFeature[T, U](a: T, b: U) extends Feature

  /**
   *
   * @author dlwh
   */
  class Model[T, U](beliefsFactory: DocumentBeliefs.Factory, assoc: AssociationFeaturizer[T, U]) extends DocumentAnnotatingModel {
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


  class Inference[T, U](beliefsFactory: DocumentBeliefs.Factory,
                        weights: DenseVector[Double],
                        scorer: AssociationFeaturizer[T, U]) extends epic.framework.ProjectableInference[ProcessedDocument, DocumentBeliefs] with DocumentAnnotatingInference {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal


    def apply(v1: ProcessedDocument, v2: DocumentBeliefs): ProcessedDocument = v1

    def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
      beliefsFactory(doc)
    }

    def score(grounding: AssociationAnchoring[T, U], ass1: Int, ass2: Int) = {
      math.exp(dot(weights, grounding.featuresFor(ass1, ass2)) )
    }

    def scores(grounding: AssociationAnchoring[T, U], prop1: Property[_], prop2: Property[_]) = {
      val result = DenseMatrix.zeros[Double](prop1.size, prop2.size)
      var p1 = 0
      while (p1 < result.rows) {
        var p2 = 0
        while (p2 < result.cols) {
          result(p1, p2) = score(grounding, p1, p2)
          p2 += 1
        }
        p1 += 1
      }

      result
    }

    def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal) = {
      val sentences = for ((sentence, sentenceBeliefs) <- doc.sentences zip aug.sentences) yield {
        val spans = TriangularArray.tabulate(sentence.length + 1) { (begin, end) =>

          val grounding = scorer.anchor(doc, sentence.index, begin, end)
          val current = sentenceBeliefs.spanBeliefs(begin, end)
          if (begin == end || (current eq null))  {
            null
          } else {
            val b1 = scorer.lens1(current)
            val b2 = scorer.lens2(current)
            val r = (b1.beliefs * b2.beliefs.t) :*= scores(grounding, b1.property, b2.property)
            val partition = breeze.linalg.sum(r)
            assert(partition != 0.0, partition + "\n" + b1 +"\n\n" + b2)
            assert(!partition.isInfinite, partition + "\n" + b1 +"\n\n" + b2)
            assert(!partition.isNaN, partition)
            r
          }
        }

        new SentenceMarginal(spans)
      }
      val marginal = new PropertyPropagation.Marginal(sentences)
      assert(!marginal.logPartition.isNaN)
      assert(!marginal.logPartition.isInfinite)
      marginal
    }

    def goldMarginal(v: ProcessedDocument, aug: DocumentBeliefs): (Marginal) = marginal(v, aug)

    def emptyCounts = StandardExpectedCounts.zero(scorer.featureIndex)

    def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      accum.loss += marg.logPartition * scale
      for {
        (sentence, sentenceMarginal) <- doc.sentences zip marg.sentences
        begin <- 0 until sentence.length
        end <- (begin+1) to sentence.length
      } {
        val anchoring = scorer.anchor(doc, sentence.index, begin, end)
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
    def project(doc: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      val newSentences = for( (myBeliefs, oldBeliefs) <- m.sentences zip oldAugment.sentences) yield {
        val newSpans = TriangularArray.tabulate(oldBeliefs.length+1) { (begin, end) =>
          val current = myBeliefs.spans(begin, end)
          if(current eq null) null
          else {
            val old: SpanBeliefs = oldBeliefs.spanBeliefs(begin, end)
            val marg1 = DenseVector.zeros[Double](current.rows)
            val marg2 = DenseVector.zeros[Double](current.cols)
            var p1 = 0
            while( p1 < current.rows) {
              var p2 = 0
              var rsum = 0.0
              while(p2 < current.cols) {
                rsum += current(p1,p2)
                marg2(p2) += current(p1, p2)
                p2 += 1
              }
              marg1(p1) = rsum
              p1 += 1
            }
            val partition = breeze.linalg.sum(marg1)
            marg1 /= partition
            marg2 /= partition

            val half = scorer.lens1.set(old, scorer.lens1.get(old).copy(beliefs=marg1))
            scorer.lens2.set(half, scorer.lens2.get(half).copy(beliefs=marg2))
          }
        }

        oldBeliefs.copy(spans=newSpans)
      }

      DocumentBeliefs(newSentences.toArray)
    }
  }



  trait AssociationFeaturizer[T, U] {
    def lens1: Lens[SpanBeliefs, Beliefs[T]]
    def lens2: Lens[SpanBeliefs, Beliefs[U]]
    def featureIndex: Index[Feature]
    def anchor(doc: ProcessedDocument, sent: Int, begin: Int, end: Int):AssociationAnchoring[T, U]
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


  case class Marginal(sentences: IndexedSeq[SentenceMarginal]) extends epic.framework.Marginal {
    val logPartition = sentences.map(_.logPartition).sum
  }
  case class SentenceMarginal(spans: TriangularArray[DenseMatrix[Double]]) {
    val logPartition = {
      val part = spans.map(s => if (s eq null) 0.0 else math.log(breeze.linalg.sum(s))).data.sum
      assert(!part.isNaN, spans.data.mkString("{", ",", "}"))
      part
    }
  }
}

/*
/**
 *
 * @author dlwh
 */
object PropertyPropagation {
  class Inference(beliefsFactory: DocumentBeliefs.Factory,
                  val featureIndex: Index[Feature]) extends epic.framework.ProjectableInference[ProcessedDocument, DocumentBeliefs] {
    def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
      beliefsFactory(doc)
    }

    def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal) = {
      val sentences = for ((sentence, sentenceBeliefs) <- doc.sentences zip aug.sentences) yield {
        val spans = TriangularArray.tabulate(sentence.length + 1) { (begin, end) =>
          val current = sentenceBeliefs.spanBeliefs(begin, end)
          if (begin == end || (current eq null))  {
            null
          } else {
            val graph: bp.Model = graphMaker.forSpan(sentence, begin, end, current)
            val beliefs = BeliefPropagation.infer(graph)
            beliefs
          }
        }

        new SentenceMarginal(spans)
      }
      val marginal = new PropertyPropagation.Marginal(sentences)
      marginal
    }

    def goldMarginal(v: ProcessedDocument, aug: DocumentBeliefs): (Marginal) = marginal(v, aug)


    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal

    def emptyCounts = StandardExpectedCounts.zero(featureIndex)

    def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
      for(s <- marg.sentences; sp <- s.spans; f@Factor(_, _) <- sp.model.factors) {
        f.tallyExpectedCounts(sp, accum.counts, scale)
      }

      accum
    }

    def project(doc: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      for( (myBeliefs, oldBeliefs) <- m.sentences zip oldAugment.sentences) yield {
        val newSpans = TriangularArray.tabulate(oldBeliefs.length+1) { (begin, end) =>
          val current: BeliefPropagation.Beliefs = myBeliefs.spans(begin, end)
          val old: SpanBeliefs = oldBeliefs.spanBeliefs(begin, end)
        }

      }

    }
  }

  case class Marginal(sentences: IndexedSeq[SentenceMarginal]) extends epic.framework.Marginal {
    val logPartition = sentences.map(_.logPartition).sum
  }
  case class SentenceMarginal(spans: TriangularArray[BeliefPropagation.Beliefs]) {
    val logPartition = spans.map(s => if (s eq null) 0.0 else s.logPartition).data.sum
  }

  case class Factor[T, U](link: IndexedLink[T, U], scores: DenseMatrix[Double]) extends bp.Factor {
    import link._
    val variables: IndexedSeq[Variable[_]] = IndexedSeq(link.p1.toVariable, link.p2.toVariable)

    def logApply(assignments: Array[Int]): Double = {
      scores(assignments(0), assignments(1))
    }

    def tallyExpectedCounts(beliefs: bp.BeliefPropagation.Beliefs, weights: DenseVector[Double], scale: Double) {
      val marg = beliefs.factorMarginalFor(this)
      val arr = Array(0,0)
      for(i <- 0 until p1.choices.size; j <- 0 until p2.choices.size) {
        arr(0) = i
        arr(1) = j
        val count = marg(arr)
        for (f <- features(i)(j))
          weights(f) += count * scale
      }
    }



  }

  trait Association[T, U] {
    def anchor(d: ProcessedDocument, span: DSpan, beliefs: SpanBeliefs):Link[T, U]
  }

  trait Link[T, U] {
    def prop1: Property[T]
    def prop2: Property[U]
    def featuresFor(a: Int, b: Int):Array[Feature]

    def indexed(index: Index[Feature]) = {
      val features = Array.tabulate(prop1.size, prop2.size){ (p1, p2) =>
        featuresFor(p1, p2).map(index(_)).filter(_ == -1)
      }

      new IndexedLink(prop1, prop2, features)
    }
  }

  case class IndexedAssociation[T, U](association: Association[T, U], index: Index[Feature]) {

    def factor(weights: DenseVector[Double], d: ProcessedDocument, span: DSpan, beliefs: SpanBeliefs) = {
      association.anchor(d, span, beliefs).indexed(index).factor(weights)
    }

  }

  case class IndexedLink[T, U](p1: Property[T], p2: Property[U],
                        features: Array[Array[Array[Int]]]) {
    def factor(weights: DenseVector[Double]):Factor[T, U] = {
      val scores = DenseMatrix.tabulate(p1.index.size, p2.index.size){ (a,b) =>
        dot(weights: DenseVector[Double], features(a)(b))
      }
      new Factor(this, scores)
    }

    private def dot(weights: DenseVector[Double], features: Array[Int]) = {
      var i = 0
      var score = 0.0
      while(i < features.length) {
        val f = features(i)
        if (f != -1)
          score += weights(f)
        i += 1
      }
      score
    }
  }




  case class LenAssociation[T, U](lens1: Lens[SpanBeliefs, Beliefs[T]], lens2: Lens[SpanBeliefs, Beliefs[U]]) extends Association {
  }

  class GraphBuilder {
    val spans = new Associations
    class Associations {
      def associations = _associations

      val _associations = ArrayBuffer[Association[_, _]]()

      def +=[T, U](assoc: Association[T, U]) {_associations += assoc}
    }

    def buildFactory(beliefsFactory: DocumentBeliefs.Factory, docs: IndexedSeq[ProcessedDocument]):GraphFactory = {
      val index = Index[Feature]()

      for(d <- docs) {
        val beliefs = beliefsFactory(d)
        for(s <- beliefs.sentences; span <- s.spans.data if span != null) {
          for(assoc <- spans.associations) {
            val link = assoc.anchor(d, span.span, span)
            for(p1 <- 0 until  link.prop1.size; p2 <- 0 until link.prop2.size) {
              for(f <- link.featuresFor(p1, p2)) {
                index.index(f)
              }
            }
          }
        }
      }

      new GraphFactory(this, index)
    }
  }

  case class GraphFactory(builder: GraphBuilder, featureIndex: Index[Feature]) {

    def graphFor(doc: ProcessedDocument, span: DSpan, beliefs: SpanBeliefs, weights: DenseVector[Double]):bp.Model = {
      // factors
      val factors = for(assoc <- builder.spans.associations) yield assoc.anchor(doc, span, beliefs).indexed(featureIndex).factor(weights)
      // current beliefs:
      for(p <- factors.toSet.flatMap(_.variables)) {

      }

    }

  }

}
*/