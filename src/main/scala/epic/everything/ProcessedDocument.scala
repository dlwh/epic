package epic.everything

import epic.trees._
import epic.coref.{CorefInstanceFeaturizer, FeaturizedCorefInstance, CorefInstance}
import epic.sequences.Segmentation
import epic.trees.StandardTreeProcessor
import epic.parser.ParseChart.SparsityPattern
import models.DocumentBeliefs
import epic.parser.projections.ConstraintCoreGrammar

/**
 * 
 * @author dlwh
 */
case class ProcessedDocument(sentences: IndexedSeq[ProcessedSentence],
//                             coref: FeaturizedCorefInstance,
                             id: String="") {
  def treeInstances = sentences.map(s => TreeInstance(s.id +"-tree", s.tree, s.words))

}

case class ProcessedSentence(words: IndexedSeq[String],
                             tree: BinarizedTree[AnnotatedLabel],
                             sparsity: SparsityPattern,
                             ner: Segmentation[NERType.Value, String],
                             speaker: Option[String],
                             id: String="") {

  def length= words.length
}


object ProcessedDocument {
  case class Factory(treeProcessor: StandardTreeProcessor,
                     constraints: ConstraintCoreGrammar[AnnotatedLabel, String],
                     corefFeaturizer: CorefInstanceFeaturizer) extends (Document=>ProcessedDocument) {

    def apply(d: Document):ProcessedDocument = {
      val newSentences = for(s <- d.sentences) yield {
        val seg = s.nerSegmentation
        var tree = treeProcessor(s.tree.map(_.treebankString))
        tree = UnaryChainRemover.removeUnaryChains(tree)

        ProcessedSentence(s.words, tree, constraints.rawConstraints(s.words).sparsity, seg, s.speaker, s.id)
      }

      ProcessedDocument(newSentences, /*corefFeaturizer.featurizeDocument(d),*/ d.id)
    }

  }
}