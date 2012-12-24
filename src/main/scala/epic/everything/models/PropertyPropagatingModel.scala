package epic.everything.models

import epic.framework.{ProjectableInference, StandardExpectedCounts, Feature}
import breeze.util.{Encoder, MutableIndex, Index}
import breeze.inference.bp
import bp.{BeliefPropagation, Variable, Factor}
import breeze.linalg.DenseVector
import collection.mutable.ArrayBuffer
import epic.everything.{DocumentAnnotator, ProcessedDocument}
import breeze.collection.mutable.TriangularArray

/*
object PropertyPropagation {
  /**
   *
   * @author dlwh
   */
  class Model(builder: PropertyPropagator.Builder) extends DocumentAnnotatingModel {
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

  case class Marginal(sentences: IndexedSeq[SentenceMarginal]) extends epic.framework.Marginal {
    def logPartition: Double = sentences.map(_.logPartition).sum
  }
  case class SentenceMarginal(spans: TriangularArray[bp.BeliefPropagation.Beliefs],
                              words: Array[bp.BeliefPropagation.Beliefs],
                              logPartition: Double) extends epic.framework.Marginal


  class PropertyPropagator(builder: PropertyPropagator.Builder,
                           weights: DenseVector[Double]) extends DocumentAnnotatingInference with ProjectableInference[ProcessedDocument, DocumentBeliefs] {
    type ExpectedCounts = epic.framework.StandardExpectedCounts[Feature]
    type Marginal = PropertyPropagation.Marginal
    def emptyCounts = StandardExpectedCounts.zero[Feature](builder.features)

    def apply(v1: ProcessedDocument, v2: DocumentBeliefs): ProcessedDocument = v1

    val spanModel = builder.spans.toBP(weights)
    val wordModel = builder.words.toBP(weights)


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
                for( ff@PropertyPropagator.Factor(_,_,_,_) <- spans.model.factorIndex)  {
                  ff.tallyExpectedCounts(spans, ec.counts)
                }

              }
              e += 1
            }
            b += 1
          }

          b = 0
          while(b < s.length) {
            val words = marginals.words(b)
            ec.loss += words.logPartition
            for( ff@PropertyPropagator.Factor(_,_,_,_) <- words.model.factorIndex)  {
              ff.tallyExpectedCounts(words, ec.counts)
            }
            b += 1
          }
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

  object PropertyPropagator {

    case class Factor[T, U](link: Link[T, U],
                            scores: Array[Array[Double]],
                            agreeScore: Double = 0,
                            disagreeScore: Double = -1) extends bp.Factor {
      import link._

      val variables: IndexedSeq[Variable[_]] = IndexedSeq(p1.toVariable, p2.toVariable)

      def logApply(assignments: Array[Int]): Double = {
        scores(assignments(0))(assignments(1)) + {if (assignments(0) == assignments(1)) agreeScore else disagreeScore}
      }

      def tallyExpectedCounts(beliefs: bp.BeliefPropagation.Beliefs, weights: DenseVector[Double]) {
        val marg = beliefs.factorMarginalFor(this)
        val arr = Array(0,0)
        for(i <- 0 until p1.choices.size; j <- 0 until p2.choices.size) {
          arr(0) = i
          arr(1) = j
          val count = marg(arr)
          weights(features(i)(j)) += count
          if(agreeFeature != -1 && i == j) {
            weights(agreeFeature) += count
          } else if(disagreeFeature != -1 && i != j) {
            weights(disagreeFeature) += count
          }
        }
      }
    }

    case class Link[T, U](p1: Property[T], p2: Property[U],
                          features: Array[Array[Int]],
                          agreeFeature: Int = -1,
                          disagreeFeature: Int = -1) {
      def factor(weights: DenseVector[Double]):Factor[T, U] = {
        val scores: Array[Array[Double]] = features.map(_ map (weights apply _))
        val agreeScore = if(agreeFeature >= 0) weights(agreeFeature) else 0.0
        val disagreeScore = if(agreeFeature >= 0) weights(disagreeFeature) else 0.0
        new Factor(this, scores, agreeScore, disagreeScore)
      }
    }

    case class AssociationFeature(prop1: String, prop2: String, v1: Any, v2: Any) extends Feature
    case class AgreementFeature(prop1: String, prop2: String) extends Feature
    case class DisagreementFeature(prop1: String, prop2: String) extends Feature
    case class Builder(spanProperties: Index[Property[_]], wordProperties: Index[Property[_]]) {
      val features = Index[Feature]()

      val words = new AssociationBuilder(wordProperties, wordLinks)
      val spans = new AssociationBuilder(spanProperties, spanLinks)

      val wordLinks = new ArrayBuffer[Link[_,_]]()
      val spanLinks = new ArrayBuffer[Link[_,_]]()

      class AssociationBuilder(props: Index[Property[_]], assoc: ArrayBuffer[Link[_, _]]) {

        def toBP(weights: DenseVector[Double]):bp.Model = {
          var m = bp.Model.empty

          for(w <- assoc) {
            m += w.factor(weights)
          }
          m
        }

        def associate(prop: Property[_], prop2: Property[_], agreement: Boolean=false) = {
          val fs = Array.tabulate(prop.choices.size, prop2.choices.size){ (i,j) =>
            val f = AssociationFeature(prop.name, prop2.name, prop.choices.get(i), prop.choices.get(j))
            features.index(f)
          }

          val (agr, dis) = if(agreement) {
            (features.index(AgreementFeature(prop.name, prop2.name)), features.index(DisagreementFeature(prop.name, prop2.name)))
          } else {
            (-1, -1)
          }

          Link(prop, prop2, fs, agr, dis)
        }
      }


    }
  }

}
*/
