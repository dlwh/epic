package epic.parser.models

import epic.parser._
import epic.lexicon.Lexicon
import epic.trees._
import epic.trees.annotations.TreeAnnotator
import breeze.linalg.Counter2
import epic.parser.projections.GrammarRefinements
import epic.trees.BinaryRule
import epic.trees.UnaryRule
import epic.trees.TreeInstance
import epic.parser.ParserParams.XbarGrammar

/**
 * @author jda
 */
class BerkeleyModel[CoarseLabel,AnnLabel,Word](val xbarGrammar: BaseGrammar[CoarseLabel],
                                               val xbarLexicon: Lexicon[CoarseLabel,Word],
                                               val baseGrammar: BaseGrammar[AnnLabel],
                                               val refinements: GrammarRefinements[CoarseLabel, (AnnLabel,List[Int])],
                                               val refinedGrammar: SimpleRefinedGrammar[CoarseLabel,(AnnLabel,List[Int]),Word]) {

  val parser = Parser(refinedGrammar)

  def split: BerkeleyModel[CoarseLabel,AnnLabel,Word] = BerkeleyModel.makeSplit(this)

}

object BerkeleyModel {

  def makeInitial(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  initialAnnotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel]):  BerkeleyModel[AnnotatedLabel,AnnotatedLabel,String] = {

    type Word = String
    type SplitLabel = (AnnotatedLabel, List[Int])

    val annotatedTrees = trainTrees.map(initialAnnotator(_))
    val (xbarGrammar, xbarLexicon) = XbarGrammar().xbarGrammar(annotatedTrees)

    // get all productions in the training set
    // will be used to set initial weights
    val (baseWordCounts, baseBinaryCounts, baseUnaryCounts) = GenerativeParser.extractCounts(annotatedTrees)

    val baseGrammar: BaseGrammar[AnnotatedLabel] = BaseGrammar(annotatedTrees.head.tree.label,
      baseBinaryCounts,
      baseUnaryCounts)

    def toRefined(label: AnnotatedLabel): Seq[SplitLabel] = Seq((label, Nil))
    def fromRefined(label: SplitLabel): AnnotatedLabel = label._1
    def splitRule[L, SL](rule: Rule[L], split: L=>Seq[SL]): Seq[Rule[SL]] = rule match {
      case BinaryRule(a, b, c) => for (aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
      // note that because everything only gets a single refinement, we don't care whether a == b
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

    val baseRefinements = GrammarRefinements(xbarGrammar,baseGrammar,(_: AnnotatedLabel).baseAnnotatedLabel)
    val presplitLabels = baseGrammar.labelIndex.map(l => l -> toRefined(l)).toMap
    val stateSplitRefinements = GrammarRefinements(baseGrammar,
                                                   toRefined _,
                                                   {splitRule(_: Rule[AnnotatedLabel], presplitLabels)},
                                                   fromRefined _)
    val refinements = baseRefinements compose stateSplitRefinements

    def wordCountKeyRefiner = {(tag: AnnotatedLabel, word: Word) => (toRefined(tag).head, word)}.tupled
    def ruleCountKeyRefiner[R] = {(tag: AnnotatedLabel, rule: Rule[AnnotatedLabel]) => (toRefined(tag).head, splitRule(rule, presplitLabels).head.asInstanceOf[R])}.tupled

    //val splitWordCounts = Counter2[SplitLabel, Word, Double](baseWordCounts.mapPairs((k, v) => (wordCountKeyRefiner(k), v)))
    //val splitBinaryCounts = Counter2[SplitLabel, BinaryRule[SplitLabel], Double](baseBinaryCounts.mapPairs((k, v) => (ruleCountKeyRefiner(k), v)))
    //val splitUnaryCounts = Counter2[SplitLabel, UnaryRule[SplitLabel], Double](baseUnaryCounts.mapPairs((k, v) => (ruleCountKeyRefiner(k), v)))

    // above is not supported due to an incorrectly-exported implicit in Breeze. Until it's working, do:

    val splitWordCounts = Counter2[SplitLabel, Word, Double](for {
      (k, v) <- baseWordCounts.iterator
      rk = wordCountKeyRefiner(k)
    } yield (rk._1, rk._2, v))
    val splitBinaryCounts = Counter2[SplitLabel, BinaryRule[SplitLabel], Double](for {
      (k, v) <- baseBinaryCounts.iterator
      rk = ruleCountKeyRefiner(k)
    } yield (rk._1, rk._2, v))
    val splitUnaryCounts = Counter2[SplitLabel, UnaryRule[SplitLabel], Double](for {
      (k, v) <- baseUnaryCounts.iterator
      rk = ruleCountKeyRefiner(k)
    } yield (rk._1, rk._2, v))

    val refinedGrammar = RefinedGrammar.generative(xbarGrammar, xbarLexicon, refinements, splitBinaryCounts, splitUnaryCounts, splitWordCounts)

    //val splitCounts = baseGrammar.labelIndex.map(l => l -> 1).toMap

    new BerkeleyModel(xbarGrammar, xbarLexicon, baseGrammar, refinements, refinedGrammar)

  }

  final val NewSubstates = 2

  def makeSplit[CoarseLabel,AnnLabel,Word](model: BerkeleyModel[CoarseLabel,AnnLabel,Word]): BerkeleyModel[CoarseLabel,AnnLabel,Word] = {

    type SplitLabel = (AnnLabel, List[Int])

    def toRefined(label: SplitLabel): Seq[SplitLabel] = (0 until NewSubstates).map(i => (label._1, i :: label._2))
    def fromRefined(label: SplitLabel): SplitLabel = (label._1, label._2.tail)
    def splitRule[L, SL](rule: Rule[L], split: L=>Seq[SL]): Seq[Rule[SL]] = rule match {
      case BinaryRule(a, b, c) => for (aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
      // don't allow non-identity refinements for identity unaries
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }
    val presplitLabels = model.refinedGrammar.refinedGrammar.labelIndex.map(l => l -> toRefined(l)).toMap

    val stateSplitRefinements = GrammarRefinements(model.refinedGrammar.refinedGrammar,
                                                   toRefined _,
                                                   {splitRule(_: Rule[SplitLabel], presplitLabels)},
                                                   fromRefined _)

    val refinements = model.refinements compose stateSplitRefinements

    val wordCounts = Counter2[SplitLabel,Word,Double] = ???
    val binaryCounts: Counter2[SplitLabel,BinaryRule[SplitLabel],Double] = ???
    val unaryCounts: Counter2[SplitLabel,UnaryRule[SplitLabel],Double] = ???

    val refinedGrammar = RefinedGrammar.generative(model.xbarGrammar, model.xbarLexicon, refinements, binaryCounts, unaryCounts, wordCounts)

    new BerkeleyModel(model.xbarGrammar, model.xbarLexicon, model.baseGrammar, refinements, refinedGrammar)


  }

}
