package epic.everything

import epic.trees._
import epic.sequences.Segmentation
import epic.ontonotes.{Sentence, Frame, Document, NERType}
import epic.parser.projections.{GoldTagPolicy, ConstraintCoreGrammar}
import epic.coref.CorefInstanceFeaturizer
import breeze.util.Index
import epic.trees.StandardTreeProcessor
import scala.Some
import epic.parser._
import epic.lexicon.Lexicon
import epic.constraints.{ChartConstraints, SpanConstraints, LabeledSpanConstraints}
import epic.features.{SurfaceFeaturizer, IndexedSurfaceAnchoring, IndexedSurfaceFeaturizer}
import epic.util.{Cache, Has2}

/**
 * 
 * @author dlwh
 */
case class FeaturizedDocument(sentences: IndexedSeq[FeaturizedSentence], id: String="")

case class FeaturizedSentence(index: Int, words: IndexedSeq[String],
                              treeOpt: Option[BinarizedTree[AnnotatedLabel]],
                              constituentSparsity: ChartConstraints[AnnotatedLabel],
                              nerOpt: Option[Segmentation[NERType.Value, String]],
                              nerConstraints: LabeledSpanConstraints[NERType.Value],
                              frames: IndexedSeq[Frame],
                              featureAnchoring: IndexedSurfaceAnchoring[String],
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
    constituentSparsity.isAllowedSpan(begin, end)
      || nerConstraints.isAllowedSpan(begin,end)
  )

  def isPossibleMaximalSpan(begin: Int, end: Int) = (
    (end-begin) == 1 || constituentSparsity.hasMaximalLabel(begin, end)
    )

  def isPossibleConstituent(begin: Int, end: Int) = {
    (end - begin) > 0 && constituentSparsity.isAllowedSpan(begin, end)
  }
}


object FeaturizedDocument {


  def makeFactory(treeProcessor: StandardTreeProcessor,
                  feat: SurfaceFeaturizer[String],
                  parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
                  nerLabelIndex: Index[NERType.Value],
                  nerConstrainer: LabeledSpanConstraints.Factory[NERType.Value, String],
                  corefFeaturizer: CorefInstanceFeaturizer = null)(docs: IndexedSeq[Document]): (Factory, IndexedSeq[FeaturizedDocument]) = {


    val srlLabels = Index[String](Iterator("O") ++ docs.iterator.flatMap(_.sentences).flatMap(_.srl).flatMap(_.args).map(_.arg))

    val sentenceConstrainer = parseConstrainer | nerConstrainer

    val featurizer = IndexedSurfaceFeaturizer.fromData(feat, docs.flatMap(_.sentences.map(_.words)), sentenceConstrainer)

    val featurized = for( d <- docs) yield {
      val newSentences = for( s <- d.sentences) yield {
        var tree = treeProcessor(s.tree.map(_.treebankString))
        tree = UnaryChainRemover.removeUnaryChains(tree)
        val constituentSparsity = parseConstrainer.rawConstraints(s.words).sparsity
        val nerConstraints = nerConstrainer.get(s.words)

        val loc = featurizer.anchor(s.words)


        FeaturizedSentence(s.index, s.words,
          Some(tree),
          constituentSparsity,
          Some(s.nerSegmentation),
          nerConstraints,
          s.srl,
          loc,
          s.speaker,
          s.id)
      }

      FeaturizedDocument(newSentences, d.id+"-featurized")
    }

    new Factory(treeProcessor, parseConstrainer, nerLabelIndex, nerConstrainer, srlLabels, featurizer, corefFeaturizer) -> featurized
  }

  case class Factory(treeProcessor: StandardTreeProcessor,
                    parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
                    nerLabelIndex: Index[NERType.Value],
                    nerConstrainer: LabeledSpanConstraints.Factory[NERType.Value, String],
                    srlLabelIndex: Index[String],
                    featurizer: IndexedSurfaceFeaturizer[String],
                    corefFeaturizer: CorefInstanceFeaturizer) extends (Document=>FeaturizedDocument) {
    def outsideSrlLabel: String = "O"

    def wordFeatureIndex = featurizer.wordFeatureIndex
    def spanFeatureIndex = featurizer.spanFeatureIndex


    def grammar: BaseGrammar[AnnotatedLabel] = parseConstrainer.grammar
    def lexicon: Lexicon[AnnotatedLabel, String] = parseConstrainer.lexicon


    def apply(d: Document):FeaturizedDocument = apply(d, keepGoldTree = false)


   def apply(d: Document, keepGoldTree: Boolean = false):FeaturizedDocument = {
     val newSentences = for(s <- d.sentences) yield {
       val seg = s.nerSegmentation
       var tree = treeProcessor(s.tree.map(_.treebankString))
       tree = UnaryChainRemover.removeUnaryChains(tree)
       val policy: GoldTagPolicy[AnnotatedLabel] = if (keepGoldTree) GoldTagPolicy.goldTreeForcing(tree.map(parseConstrainer.grammar.labelIndex)) else GoldTagPolicy.noGoldTags
       val constituentSparsity = parseConstrainer.rawConstraints(s.words, policy).sparsity
       val nerConstraints = nerConstrainer.get(s.words)

       val loc = featurizer.anchor(s.words)

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