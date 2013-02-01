package epic.redux

import breeze.collection.mutable.TriangularArray
import epic.trees._
import epic.parser.ParseChart.SparsityPattern
import epic.sequences.{SemiCRF, Segmentation}
import epic.ontonotes.{Frame, Document, NERType}
import epic.parser.projections.{GoldTagPolicy, ConstraintCoreGrammar}
import epic.coref.CorefInstanceFeaturizer
import breeze.util.Index
import epic.framework.Feature
import breeze.linalg.{Axis, Counter2}
import epic.parser.features.BasicFeaturizer
import epic.trees.StandardTreeProcessor
import epic.sequences.Segmentation
import scala.Some
import epic.parser._
import java.util.concurrent.atomic.AtomicInteger

/**
 * 
 * @author dlwh
 */
case class FeaturizedDocument(sentences: IndexedSeq[FeaturizedSentence], id: String="")

case class FeaturizedSentence(index: Int, words: IndexedSeq[String],
                              treeOpt: Option[BinarizedTree[AnnotatedLabel]],
                              constituentSparsity: SparsityPattern,
                              nerOpt: Option[Segmentation[NERType.Value, String]],
                              nerConstraints: SemiCRF.SpanConstraints,
                              frames: IndexedSeq[Frame],
                              wordFeatures: IndexedSeq[Array[Int]],
                              spanFeatures: TriangularArray[Array[Int]],
                              speaker: Option[String] = None,
                              id: String = "")  {
  def featuresForSpan(begin: Int, end: Int) = spanFeatures(begin, end)

  def featuresForWord(w: Int) = wordFeatures(w)

  def withTree(tree: BinarizedTree[AnnotatedLabel]) = copy(treeOpt = Some(tree))

  def treeInstance = TreeInstance(id +"-tree", tree, words)

  def tree = treeOpt.get
  def ner = nerOpt.get

  def length = words.length

  def isPossibleSpan(begin: Int, end: Int) = (
//    true
    constituentSparsity.activeTriangularIndices.contains(TriangularArray.index(begin,end))
      || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
  )
}


object FeaturizedDocument {


  def makeFactory(treeProcessor: StandardTreeProcessor,
                  parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
                  nerConstrainer: SemiCRF.ConstraintGrammar[NERType.Value, String],
                  tagWordCounts: Counter2[AnnotatedLabel, String, Double],
                  corefFeaturizer: CorefInstanceFeaturizer)(docs: IndexedSeq[Document]): (Factory, IndexedSeq[FeaturizedDocument]) = {
    val wordFeatureIndex, spanFeatureIndex = Index[Feature]()

    val featurizer = new BasicFeaturizer(tagWordCounts, breeze.linalg.sum(tagWordCounts, Axis._0))

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

    val featurized = for( ((d, other)) <- docs zip constraints.seq) yield {
      val newSentences = for( (s, (tree, seg, constituentSparsity, nerConstraints)) <- d.sentences zip other) yield {
        def isPossibleSpan(begin: Int, end: Int) = (
          constituentSparsity.activeTriangularIndices.contains(TriangularArray.index(begin,end))
            || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
          )

        val loc = featurizer.anchor(s.words)
        val words = Array.tabulate(s.words.length)(loc.featuresForWord(_).map(wordFeatureIndex.index(_)))
        val spans = TriangularArray.tabulate(s.words.length+1){ (beg, end) =>
          if(isPossibleSpan(beg, end)) {
            loc.featuresForSpan(beg, end).map(spanFeatureIndex.index(_))
          } else {
            null
          }
        }


        FeaturizedSentence(s.index, s.words,
          Some(tree),
          constituentSparsity,
          Some(seg),
          nerConstraints,
          s.srl,
          words, spans,
          s.speaker,
          s.id)
      }

      FeaturizedDocument(newSentences, d.id+"-featurized")
    }

    new Factory(treeProcessor, parseConstrainer, nerConstrainer, featurizer, corefFeaturizer, wordFeatureIndex, spanFeatureIndex) -> featurized
  }

  case class Factory(treeProcessor: StandardTreeProcessor,
                    parseConstrainer: ConstraintCoreGrammar[AnnotatedLabel, String],
//                     graphFeaturizer: PropertyPropagation.GraphBuilder,
                    nerConstrainer: SemiCRF.ConstraintGrammar[NERType.Value, String],
                    featurizer: BasicFeaturizer,
                    corefFeaturizer: CorefInstanceFeaturizer,
                    wordFeatureIndex: Index[Feature],
                    spanFeatureIndex: Index[Feature]) extends (Document=>FeaturizedDocument) {

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

       def isPossibleSpan(begin: Int, end: Int) = (
         constituentSparsity.activeTriangularIndices.contains(TriangularArray.index(begin,end))
           || (nerConstraints.allowedLabels(begin,end).ne(null) && nerConstraints.allowedLabels(begin,end).nonEmpty)
         )

       val loc = featurizer.anchor(s.words)
       val words = Array.tabulate(s.words.length)(loc.featuresForWord(_).map(wordFeatureIndex(_)).filter(_ != -1))
       val spans = TriangularArray.tabulate(s.words.length+1){ (beg, end) =>
         if(isPossibleSpan(beg, end)) {
           loc.featuresForSpan(beg, end).map(spanFeatureIndex(_)).filter(_ != -1)
         } else {
           null
         }
       }

       FeaturizedSentence(s.index, s.words,
         Some(tree),
         constituentSparsity,
         Some(seg),
         nerConstraints,
         s.srl,
         words, spans,
         s.speaker,
         s.id)
     }

     FeaturizedDocument(newSentences, /*corefFeaturizer.featurizeDocument(d),*/ d.id)
   }

 }
}