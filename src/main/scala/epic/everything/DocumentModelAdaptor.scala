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
    type Inference = DocumentInferenceAdaptor[sentenceModel.Inference] { type Marginal = DocumentModelAdaptor.Marginal[sentenceModel.Marginal]; type Scorer = self.Scorer}
    type Scorer = DocumentModelAdaptor.Scorer[sentenceModel.Scorer]
  }
  case class Scorer[Scorer](scorers: IndexedSeq[Scorer])
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

  def evaluate(guess: FeaturizedDocument, gold: FeaturizedDocument, logResults: Boolean): EvaluationResult = {
    val results = for( (guessSent, goldSent) <- guess.sentences.zip(gold.sentences)) yield sentenceModel.evaluate(guessSent, goldSent, logResults)
    results.reduceLeft(_ + _)
  }


  def emptyCounts: ExpectedCounts = sentenceModel.emptyCounts

  def accumulateCounts(s: Scorer, d: FeaturizedDocument, m: Marginal, accum: ExpectedCounts, scale: Double):Unit = {
    for(i <- 0 until d.sentences.length)
      sentenceModel.accumulateCounts(s.scorers(i), d.sentences(i), m.sentences(i), accum, scale)
  }


}

class DocumentInferenceAdaptor[Inf<:DocumentInferenceAdaptor.SentenceInference]
                              (factory:DocumentBeliefs.Factory,
                               val sentenceInference: Inf) extends
                              ProjectableInference[FeaturizedDocument, DocumentBeliefs]
                                  with AnnotatingInference[FeaturizedDocument] {
  def baseAugment(v: FeaturizedDocument): DocumentBeliefs = factory(v)

  type Marginal = DocumentModelAdaptor.Marginal[sentenceInference.Marginal]

  type Scorer = DocumentModelAdaptor.Scorer[sentenceInference.Scorer]


  def scorer(v: FeaturizedDocument): Scorer = {
    new Scorer(v.sentences.map(sentenceInference.scorer _))
  }

  def marginal(scorer: Scorer, doc: FeaturizedDocument, aug: DocumentBeliefs): Marginal = {
    DocumentModelAdaptor.Marginal(for( i <- 0 until doc.sentences.length) yield {
       sentenceInference.marginal(scorer.scorers(i), doc.sentences(i), aug.beliefsForSentence(i)):sentenceInference.Marginal
    })
  }


  def goldMarginal(scorer: Scorer, doc: FeaturizedDocument, aug: DocumentBeliefs): Marginal = {
    DocumentModelAdaptor.Marginal(for( i <- 0 until doc.sentences.length) yield {
      sentenceInference.goldMarginal(scorer.scorers(i), doc.sentences(i), aug.beliefsForSentence(i)):sentenceInference.Marginal
    })
  }

  def annotate(datum: FeaturizedDocument, m: Marginal): FeaturizedDocument = {
    val newSents = for ( (sent,marg) <- datum.sentences zip m.sentences) yield {
      sentenceInference.annotate(sent, marg)
    }
    datum.copy(newSents)
  }

  def project(v: FeaturizedDocument, scorer: Scorer, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    val newSents = for ( i <- 0 until v.sentences.length) yield {
      sentenceInference.project(v.sentences(i), scorer.scorers(i), m.sentences(i), oldAugment.beliefsForSentence(i))
    }
    oldAugment.copy(sentences=newSents.toArray)
  }
}


object DocumentInferenceAdaptor {
  type SentenceInference = ProjectableInference[FeaturizedSentence, SentenceBeliefs] with AnnotatingInference[FeaturizedSentence]
}
