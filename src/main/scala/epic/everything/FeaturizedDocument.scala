package epic.everything

import epic.trees._
import epic.sequences.Segmentation
import epic.ontonotes.NERType
import epic.coref.CorefInstanceFeaturizer
import breeze.util.Index
import epic.parser._
import epic.lexicon.Lexicon
import epic.constraints.{ChartConstraints, LabeledSpanConstraints}
import epic.features._
import epic.util.CacheBroker
import scala.util.Try
import epic.parser.projections.{GrammarRefinements, ReachabilityProjection}
import epic.trees.StandardTreeProcessor
import epic.trees.TreeInstance
import epic.ontonotes.Frame
import epic.ontonotes.Document

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
                              wordFeatureAnchoring: IndexedWordAnchoring[String],
                              speaker: Option[String] = None,
                              id: String = "")  extends IndexedSurfaceAnchoring[String] with IndexedWordAnchoring[String] {
  def featuresForSpan(begin: Int, end: Int) = {
    featureAnchoring.featuresForSpan(begin, end)
  }

  def featuresForWord(w: Int) = {
    wordFeatureAnchoring.featuresForWord(w)
  }


  override def toString: String = s"FeaturizedSentence($index, $words, $treeOpt, $nerOpt, ..., $speaker, $id)"

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
                  wordFeaturizer: WordFeaturizer[String],
                  feat: SurfaceFeaturizer[String],
                  grammar: BaseGrammar[AnnotatedLabel], lexicon: Lexicon[AnnotatedLabel, String],
                  parseConstrainer: ChartConstraints.Factory[AnnotatedLabel, String],
                  nerLabelIndex: Index[NERType.Value],
                  nerConstrainer: LabeledSpanConstraints.Factory[NERType.Value, String],
                  corefFeaturizer: CorefInstanceFeaturizer = null)(docs: IndexedSeq[Document])(implicit broker: CacheBroker): (Factory, IndexedSeq[FeaturizedDocument]) = {


    val srlLabels = Index[String](Iterator("O") ++ docs.iterator.flatMap(_.sentences).flatMap(_.srl).flatMap(_.args).map(_.arg))

    val sentenceConstrainer = parseConstrainer | nerConstrainer

    val reachable = {
      val treeInstances = docs.flatMap(_.sentences.map(_.treeInstance(treeProcessor)))
      val refGrammar = GenerativeParser.extractGrammar(AnnotatedLabel.TOP, treeInstances)
      new ReachabilityProjection(grammar, lexicon, refGrammar)
    }

    val words: IndexedSeq[IndexedSeq[String]] = docs.flatMap(_.sentences.map(_.words))
    val wfeaturizer = IndexedWordFeaturizer.fromData(wordFeaturizer, words)
    val featurizer = IndexedSurfaceFeaturizer.fromData(feat, words, sentenceConstrainer)

    val featurized = for( d <- docs) yield {
      val newSentences = for( s <- d.sentences) yield {
        val constituentSparsity = parseConstrainer.constraints(s.words)
        var tree = treeProcessor(s.tree.map(_.treebankString))
        tree = UnaryChainCollapser.collapseUnaryChains(tree)
        tree = reachable.forTree(tree, s.words, constituentSparsity)
        val nerConstraints = nerConstrainer.get(s.words)

        val loc = featurizer.anchor(s.words)
        val wloc = wfeaturizer.anchor(s.words)

        FeaturizedSentence(s.index, s.words,
          Some(tree),
          constituentSparsity,
          Some(s.nerSegmentation),
          nerConstraints,
          s.srl,
          loc,
          wloc,
          s.speaker,
          s.id)
      }

      FeaturizedDocument(newSentences, d.id+"-featurized")
    }

    new Factory(treeProcessor, grammar, lexicon, parseConstrainer, nerLabelIndex, nerConstrainer, srlLabels, featurizer, wfeaturizer, corefFeaturizer) -> featurized
  }

  case class Factory(treeProcessor: StandardTreeProcessor,
                    grammar: BaseGrammar[AnnotatedLabel],
                    lexicon: Lexicon[AnnotatedLabel, String],
                    parseConstrainer: ChartConstraints.Factory[AnnotatedLabel, String],
                    nerLabelIndex: Index[NERType.Value],
                    nerConstrainer: LabeledSpanConstraints.Factory[NERType.Value, String],
                    srlLabelIndex: Index[String],
                    featurizer: IndexedSurfaceFeaturizer[String],
                    wordFeaturizer: IndexedWordFeaturizer[String],
                    corefFeaturizer: CorefInstanceFeaturizer) extends (Document=>FeaturizedDocument) {
    def outsideSrlLabel: String = "O"

    def wordFeatureIndex = wordFeaturizer.featureIndex
    def spanFeatureIndex = featurizer.featureIndex


   def apply(d: Document):FeaturizedDocument = {
     val newSentences = for(s <- d.sentences) yield {
       val seg = s.nerSegmentation
       var tree = treeProcessor(s.tree.map(_.treebankString))
       tree = UnaryChainCollapser.collapseUnaryChains(tree)
       val constituentSparsity = Try{parseConstrainer.constraints(s.words)}.getOrElse(ChartConstraints.noSparsity[AnnotatedLabel])
       val nerConstraints = nerConstrainer.get(s.words)

       val loc = featurizer.anchor(s.words)
       val wloc = wordFeaturizer.anchor(s.words)

       FeaturizedSentence(s.index, s.words,
         Some(tree),
         constituentSparsity,
         Some(seg),
         nerConstraints,
         s.srl,
         loc,
         wloc,
         s.speaker,
         s.id +"-featurized")
     }

     FeaturizedDocument(newSentences, /*corefFeaturizer.featurizeDocument(d),*/ d.id)
   }

 }
}