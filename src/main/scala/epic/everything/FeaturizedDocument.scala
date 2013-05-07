package epic.everything

import epic.trees._
import epic.sequences.{Gazetteer, SemiCRF, Segmentation}
import epic.ontonotes.{Frame, Document, NERType}
import epic.parser.projections.{GoldTagPolicy, ConstraintCoreGrammar}
import epic.coref.CorefInstanceFeaturizer
import breeze.util.Index
import epic.framework.Feature
import breeze.linalg.Counter2
import epic.trees.StandardTreeProcessor
import scala.Some
import epic.parser._
import java.util.concurrent.atomic.AtomicInteger
import epic.lexicon.Lexicon
import epic.features.IndexedSpanFeaturizer

/**
 * 
 * @author dlwh
 */
case class FeaturizedDocument(sentences: IndexedSeq[FeaturizedSentence], id: String="")

case class FeaturizedSentence(index: Int, words: IndexedSeq[String],
                              treeOpt: Option[BinarizedTree[AnnotatedLabel]],
                              constituentSparsity: ChartConstraints[AnnotatedLabel],
                              nerOpt: Option[Segmentation[NERType.Value, String]],
                              nerConstraints: SemiCRF.SpanConstraints,
                              frames: IndexedSeq[Frame],
                              featureAnchoring: IndexedSpanFeaturizer#Localization,
                              speaker: Option[String] = None,
                              id: String = "")  {
  def featuresForSpan(begin: Int, end: Int) = featureAnchoring.featuresForSpan(begin, end)

  def featuresForWord(w: Int) = featureAnchoring.featuresForWord(w)

  def withTree(tree: BinarizedTree[AnnotatedLabel]) = copy(treeOpt = Some(tree))

  def treeInstance = TreeInstance(id +"-tree", tree, words)

  def tree = treeOpt.get
  def ner = nerOpt.get

  def length = words.length

  def isPossibleSpan(begin: Int, end: Int) = (
//    true
    constituentSparsity.isActiveSpan(begin, end)
      || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
  )

  def isPossibleMaximalSpan(begin: Int, end: Int) = (
    (end-begin) == 1 || constituentSparsity.hasMaximalLabel(begin, end)
    )

  def isPossibleConstituent(begin: Int, end: Int) = {
    (end - begin) > 0 && constituentSparsity.isActiveSpan(begin, end)
  }
}


object FeaturizedDocument {


  def makeFactory(treeProcessor: StandardTreeProcessor,
                  parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
                  nerConstrainer: SemiCRF.ConstraintSemiCRF[NERType.Value, String],
                  tagWordCounts: Counter2[AnnotatedLabel, String, Double],
                  corefFeaturizer: CorefInstanceFeaturizer)(docs: IndexedSeq[Document]): (Factory, IndexedSeq[FeaturizedDocument]) = {
    val wordFeatureIndex, spanFeatureIndex = Index[Feature]()


    val srlLabels = Index[String](Iterator("O") ++ docs.iterator.flatMap(_.sentences).flatMap(_.srl).flatMap(_.args).map(_.arg))

    val count = new AtomicInteger(0)
    val constraints = for(d <- docs.par) yield {
      println(s"in ${d.id}")
      val r = for(s <- d.sentences.par) yield {
        val seg = s.nerSegmentation
        var tree = treeProcessor(s.tree.map(_.treebankString))
        tree = UnaryChainRemover.removeUnaryChains(tree)
        val policy: GoldTagPolicy[AnnotatedLabel] = GoldTagPolicy.goldTreeForcing(tree.map(parseConstrainer.grammar.labelIndex))
        val constituentSparsity = parseConstrainer.rawConstraints(s.words, policy).sparsity
        val nerConstraints = nerConstrainer.constraints(s.nerSegmentation, keepGold = true)

        (tree, seg, constituentSparsity, nerConstraints)
      }
      println(s"done: ${d.id} ${count.getAndIncrement}/${docs.length}")
      r.seq
    }

    val docsWithValidSpans = for( (d,other) <- docs zip constraints; (s, (tree, seg, constituentSparsity, nerConstraints)) <- d.sentences zip other) yield {
      def isPossibleSpan(begin: Int, end: Int) = (
          constituentSparsity.isActiveSpan(begin,end)
            || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
          )

      s.words -> (isPossibleSpan _)
    }

    val featurizer = IndexedSpanFeaturizer.forTrainingSet(docsWithValidSpans, tagWordCounts, Gazetteer.ner())

    val featurized = for( ((d, other)) <- docs zip constraints.seq) yield {
      val newSentences = for( (s, (tree, seg, constituentSparsity, nerConstraints)) <- d.sentences zip other) yield {
        def isPossibleSpan(begin: Int, end: Int) = (
          constituentSparsity.isActiveSpan(begin,end)
            || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
          )

        val loc = featurizer.anchor(s.words, isPossibleSpan)


        FeaturizedSentence(s.index, s.words,
          Some(tree),
          constituentSparsity,
          Some(seg),
          nerConstraints,
          s.srl,
          loc,
          s.speaker,
          s.id)
      }

      FeaturizedDocument(newSentences, d.id+"-featurized")
    }

    new Factory(treeProcessor, parseConstrainer, nerConstrainer, srlLabels, featurizer, corefFeaturizer, wordFeatureIndex, spanFeatureIndex) -> featurized
  }

  case class Factory(treeProcessor: StandardTreeProcessor,
                    parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
//                     graphFeaturizer: PropertyPropagation.GraphBuilder,
                    nerConstrainer: SemiCRF.ConstraintSemiCRF[NERType.Value, String],
                    srlLabelIndex: Index[String],
                    featurizer: IndexedSpanFeaturizer,
                    corefFeaturizer: CorefInstanceFeaturizer,
                    wordFeatureIndex: Index[Feature],
                    spanFeatureIndex: Index[Feature]) extends (Document=>FeaturizedDocument) {
    def outsideSrlLabel: String = "O"


    def grammar: BaseGrammar[AnnotatedLabel] = parseConstrainer.grammar
    def lexicon: Lexicon[AnnotatedLabel, String] = parseConstrainer.lexicon
    def nerLabelIndex = nerConstrainer.labelIndex


    def apply(d: Document):FeaturizedDocument = apply(d, keepGoldTree = false)


   def apply(d: Document, keepGoldTree: Boolean = false):FeaturizedDocument = {
     val newSentences = for(s <- d.sentences) yield {
       val seg = s.nerSegmentation
       var tree = treeProcessor(s.tree.map(_.treebankString))
       tree = UnaryChainRemover.removeUnaryChains(tree)
       val policy: GoldTagPolicy[AnnotatedLabel] = if (keepGoldTree) GoldTagPolicy.goldTreeForcing(tree.map(parseConstrainer.grammar.labelIndex)) else GoldTagPolicy.noGoldTags
       val constituentSparsity = parseConstrainer.rawConstraints(s.words, policy).sparsity
       val nerConstraints = nerConstrainer.constraints(s.nerSegmentation, keepGold = keepGoldTree)

       def isPossibleSpan(begin: Int, end: Int) = begin != end && (
         constituentSparsity.isActiveSpan(begin,end)
           || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
         )

       val loc = featurizer.anchor(s.words, isPossibleSpan)

       FeaturizedSentence(s.index, s.words,
         Some(tree),
         constituentSparsity,
         Some(seg),
         nerConstraints,
         s.srl,
         loc,
         s.speaker,
         s.id +"-featurized")
     }

     FeaturizedDocument(newSentences, /*corefFeaturizer.featurizeDocument(d),*/ d.id)
   }

 }
}