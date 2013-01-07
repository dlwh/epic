package epic.everything.models

import epic.everything.{ProcessedDocument, DPos, Document, DSpan}
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.inference.bp
import bp.{BeliefPropagation, Variable}
import epic.framework.{ProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Index, Lens}
import breeze.collection.mutable.TriangularArray
import collection.mutable.ArrayBuffer

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

    def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = {
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
      marginal -> marginal.logPartition
    }

    def goldMarginal(v: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = marginal(v, aug)


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
    def ground(d: ProcessedDocument, span: DSpan, beliefs: SpanBeliefs):Link[T, U]
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
      association.ground(d, span, beliefs).indexed(index).factor(weights)
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
            val link = assoc.ground(d, span.span, span)
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
      val factors = for(assoc <- builder.spans.associations) yield assoc.ground(doc, span, beliefs).indexed(featureIndex).factor(weights)
      // current beliefs:
      for(p <- factors.toSet.flatMap(_.variables)

    }

  }

}

*/