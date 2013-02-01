package epic.everything

import epic.framework._
import breeze.util.Index
import breeze.linalg.DenseVector
import epic.everything.DocumentModelAdaptor.TypeHack

object DocumentModelAdaptor {
  type CompatibleModel = EvaluableModel[FeaturizedSentence] { type Inference <: DocumentInferenceAdaptor.SentenceInference}
  // ugh scala
  trait TypeHack extends Model[FeaturizedDocument] with EvaluableModel[FeaturizedDocument] { self =>
    val sentenceModel: CompatibleModel
    type ExpectedCounts = sentenceModel.ExpectedCounts
    type Marginal = DocumentModelAdaptor.Marginal[sentenceModel.Marginal]
    type Inference = DocumentInferenceAdaptor[sentenceModel.Inference] {type ExpectedCounts = TypeHack.this.ExpectedCounts; type Marginal = DocumentModelAdaptor.Marginal[sentenceModel.Marginal]}
  }
  case class Marginal[M<:epic.framework.Marginal](sentences: IndexedSeq[M]) extends epic.framework.Marginal {
    def logPartition: Double = sentences.map(_.logPartition).sum
  }

}

/**
 * 
 * @author dlwh
 */
class DocumentModelAdaptor(factory: DocumentBeliefs.Factory, val sentenceModel: DocumentModelAdaptor.CompatibleModel) extends TypeHack {
  type EvaluationResult = sentenceModel.EvaluationResult

  def featureIndex: Index[Feature] = sentenceModel.featureIndex

  def initialValueForFeature(f: Feature): Double = sentenceModel.initialValueForFeature(f)

  def inferenceFromWeights(weights: DenseVector[Double]):Inference = {
    // shut up Scala, I've had enough of your shit.
    (new DocumentInferenceAdaptor[sentenceModel.Inference](factory, sentenceModel.inferenceFromWeights(weights))).asInstanceOf[Inference]
  }

  def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
    sentenceModel.expectedCountsToObjective(ecounts)
  }

  def evaluate(guess: FeaturizedDocument, gold: FeaturizedDocument): EvaluationResult = {
    val results = for( (guessSent, goldSent) <- guess.sentences.zip(gold.sentences)) yield sentenceModel.evaluate(guessSent, goldSent)
    results.reduceLeft(_ + _)
  }

}

class DocumentInferenceAdaptor[Inf<:DocumentInferenceAdaptor.SentenceInference]
                              (factory:DocumentBeliefs.Factory,
                               val sentenceInference: Inf) extends
                              ProjectableInference[FeaturizedDocument, DocumentBeliefs]
                                  with AnnotatingInference[FeaturizedDocument] {
  def baseAugment(v: FeaturizedDocument): DocumentBeliefs = factory(v)

  type ExpectedCounts = sentenceInference.ExpectedCounts
  type Marginal = DocumentModelAdaptor.Marginal[sentenceInference.Marginal]

  def marginal(doc: FeaturizedDocument, aug: DocumentBeliefs): Marginal = {
    DocumentModelAdaptor.Marginal(for( (sent, beliefs) <- doc.sentences.zip(aug.sentences)) yield {
       sentenceInference.marginal(sent, beliefs)
    })
  }

  def emptyCounts: ExpectedCounts = sentenceInference.emptyCounts

  def goldMarginal(doc: FeaturizedDocument, aug: DocumentBeliefs): Marginal = {
    DocumentModelAdaptor.Marginal(for( (sent, beliefs) <- doc.sentences.zip(aug.sentences)) yield {
      sentenceInference.goldMarginal(sent, beliefs)
    })
  }

  def annotate(datum: FeaturizedDocument, m: Marginal): FeaturizedDocument = {
    val newSents = for ( (sent,marg) <- datum.sentences zip m.sentences) yield {
      sentenceInference.annotate(sent, marg)
    }
    datum.copy(newSents)
  }

  def countsFromMarginal(v: FeaturizedDocument, marg: Marginal, accum: ExpectedCounts, scale: Double): ExpectedCounts = {
    for( (s,m) <- v.sentences zip marg.sentences) {
      sentenceInference.countsFromMarginal(s, m, accum, scale)
    }
    accum
  }

  def project(v: FeaturizedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    val newSents = for ( (sent, (m,aug)) <- v.sentences zip (m.sentences zip oldAugment.sentences)) yield {
      sentenceInference.project(sent, m, aug)
    }
    oldAugment.copy(sentences=newSents.toArray)
  }
}


object DocumentInferenceAdaptor {
  type SentenceInference = ProjectableInference[FeaturizedSentence, SentenceBeliefs] with AnnotatingInference[FeaturizedSentence]
}