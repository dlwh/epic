package epic.everything.models

import epic.everything.{ProcessedDocument, DPos, Document, DSpan}
import breeze.linalg.DenseVector
import breeze.inference.bp
import bp.{BeliefPropagation, Variable}
import epic.framework.{ProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Index, Lens}
import breeze.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
object PropertyPropagation {

  class Model(builder: GraphBuilder) extends DocumentAnnotatingModel {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Inference = PropertyPropagator
    type Marginal = PropertyPropagation.Marginal

    def featureIndex: Index[Feature] = builder.features

    def initialValueForFeature(f: Feature): Double = math.random * 1E-4

    def inferenceFromWeights(weights: DenseVector[Double]) = new PropertyPropagator(builder, weights)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      ecounts.loss -> ecounts.counts
    }
  }

  class PropertyPropagator(builder: GraphBuilder,
                           weights: DenseVector[Double]) extends DocumentAnnotatingInference with ProjectableInference[ProcessedDocument, DocumentBeliefs] {
    type ExpectedCounts = epic.framework.StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal
    def emptyCounts = StandardExpectedCounts.zero[Feature](builder.features)

    def apply(v1: ProcessedDocument, v2: DocumentBeliefs): ProcessedDocument = v1

    def countsFromMarginal(doc: ProcessedDocument, marg: Marginal, aug: DocumentBeliefs): ExpectedCounts = {
      val ec = emptyCounts
      for( (s, marginals) <- doc.sentences zip marg.sentences) {
        if(marginals ne null) {
          var b = 0
          while(b < s.length) {
            var e = b+1
            while(e <= s.length) {
              val spans = marginals.spans(b,e)
              if( spans ne null) {
                ec.loss += spans.logPartition
                for( ff@Factor(_,_,) <- spans.model.factorIndex)  {
                  ff.tallyExpectedCounts(spans, ec.counts)
                }

              }
              e += 1
            }
            b += 1
          }

          /*
          b = 0
          while(b < s.length) {
            val words = marginals.words(b)
            ec.loss += words.logPartition
            for( ff@Factor(_,_,_,_) <- words.model.factorIndex)  {
              ff.tallyExpectedCounts(words, ec.counts)
            }
            b += 1
          }
          */
        }
      }

      ec

    }


    def project(v: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      val sentences = for(s <- m.sentences) yield {
        val wordBeliefs = s.words.map(b => new PropertyBeliefs(b.beliefs.toArray.map(_.data)))
        val spanBeliefs = s.spans.map(b => new PropertyBeliefs(b.beliefs.toArray.map(_.data)))
        new SentenceBeliefs(spanBeliefs, wordBeliefs)
      }


      oldAugment.copy(sentenceBeliefs=sentences.toArray)
    }

    def baseAugment(v: ProcessedDocument): DocumentBeliefs = null

    def marginal(doc: ProcessedDocument, beliefs: DocumentBeliefs): (Marginal, Double) = {
      val sentences = for( (s, sentBeliefs) <- doc.sentences zip beliefs.sentenceBeliefs) yield {
        if(sentBeliefs eq null) null
        else {
          val spans = TriangularArray.tabulate(s.length){ (b,e) =>
            val spanInput: PropertyBeliefs = sentBeliefs.spanBeliefs(b,e)
            if(spanInput == null) null
            else {
              val factors = (0 until spanInput.beliefs.length) map { prop =>
                val variable = beliefs.spanProperties.get(prop).toVariable
                Factor(variable)(spanInput(prop))
              }

              BeliefPropagation.infer((spanModel /: factors)(_ + _))
            }
          }

          val words = Array.tabulate(s.length){ (b) =>
            val wordInput = sentBeliefs.wordBeliefs(b)
            val factors = (0 until wordInput.beliefs.length) map { prop =>
              val variable = beliefs.wordProperties.get(prop).toVariable
              Factor(variable)(wordInput(prop))
            }

            BeliefPropagation.infer((wordModel /: factors)(_ + _))
          }

          new SentenceMarginal(spans, words)
        }
      }

      (Marginal(sentences),sentences.filter(_ ne null).map(s => s.spans.data.filter( _ ne null).map(_.logPartition).sum + s.words.map(_.logPartition).sum).sum)
    }


    def goldMarginal(v: ProcessedDocument, aug: DocumentBeliefs): (Marginal, Double) = marginal(v, aug)

    def projectGold(v: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      project(v, m, oldAugment)
    }
  }

  case class Marginal(sentences: IndexedSeq[SentenceMarginal]) extends epic.framework.Marginal {
    def logPartition: Double = sentences.map(_.logPartition).sum
  }
  case class SentenceMarginal(spans: TriangularArray[bp.BeliefPropagation.Beliefs],
                              words: Array[bp.BeliefPropagation.Beliefs],
                              logPartition: Double) extends epic.framework.Marginal

  class GraphBuilder(factory: LinkFactory, val features: Index[Feature]) {
    def buildLinks(doc: Document):IndexedSeq[TriangularArray[IndexedSeq[IndexedLink[_, _]]]] = {
      for(s <- doc.sentences) yield TriangularArray.tabulate(s.length) {(b, e) =>
        factory.linksFor(doc, DSpan(s.docId, s.sentId, b, e)).map(_.indexed(features))
      }
    }
  }

  trait LinkFactory {
    def linksFor(doc: Document, span: DSpan):IndexedSeq[Link[_, _]]
    //    def linksFor(doc: Document, span: DPos):IndexedSeq[Link[_, _]]
    def featuresFor(docs: IndexedSeq[Document]):Iterable[Feature]
  }


  class BasicLinkFactory[T, U](p1: Property[T],  p2: Property[U],  agreement: Boolean = false) extends LinkFactory with Serializable {
    val link = Link(p1, p2, Array.tabulate(p1.size, p2.size){(a,b) =>
      if(agreement) {
        if(a == b)
          Array(AgreementFeature(p1.name, p2.name), AssociationFeature(p1.name, p2.name, a, b) )
        else
          Array(DisagreementFeature(p1.name, p2.name), AssociationFeature(p1.name, p2.name, a, b) )
      } else {
        Array(AssociationFeature(p1.name, p2.name, a, b))
      }
    })

    def linksFor(doc: Document, span: DSpan): IndexedSeq[Link[_, _]] = {
      IndexedSeq(link)
    }

    //    def linksFor(doc: Document, span: DPos):IndexedSeq[Link[_, _]]
    def featuresFor(docs: IndexedSeq[Document]): Iterable[Feature] = link.features.flatten.flatten
  }

  case class AssociationFeature(prop1: String, prop2: String, v1: Any, v2: Any) extends Feature
  case class AgreementFeature(prop1: String, prop2: String) extends Feature
  case class DisagreementFeature(prop1: String, prop2: String) extends Feature

  case class Link[T, U](p1: Property[T], p2: Property[U], features: Array[Array[Array[Feature]]]) {
    def indexed(index: Index[Feature]): IndexedLink[T, U] = IndexedLink(p1, p2, features.map(_.map(_.map(index))))
  }

  case class IndexedLink[T, U](p1: Property[T], p2: Property[U],
                               features: Array[Array[Array[Int]]]) {

    def factor(weights: DenseVector[Double]):Factor[T, U] = {
      val scores: Array[Array[Double]] = features.map(_ map (_.map(i => if(i >= 0) weights(i) else 0.0).sum))
      new Factor(this, scores)
    }
  }

  case class Factor[T, U](link: IndexedLink[T, U],
                          scores: Array[Array[Double]]) extends bp.Factor {
    import link._
    val variables: IndexedSeq[Variable[_]] = IndexedSeq(p1.toVariable, p2.toVariable)

    def logApply(assignments: Array[Int]): Double = {
      scores(assignments(0))(assignments(1))
    }

    def tallyExpectedCounts(beliefs: bp.BeliefPropagation.Beliefs, weights: DenseVector[Double]) {
      val marg = beliefs.factorMarginalFor(this)
      val arr = Array(0,0)
      for(i <- 0 until p1.choices.size; j <- 0 until p2.choices.size) {
        arr(0) = i
        arr(1) = j
        val count = marg(arr)
        for(f <- features(i)(j)) weights(f) += count
      }
    }
  }

}


