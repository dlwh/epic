package epic.everything

import breeze.linalg._
import epic.trees._
import epic.parser._
import epic.parser.models._
import epic.framework._
import breeze.util.Index
import epic.parser.projections.AnchoredSpanProjector
import breeze.collection.mutable.TriangularArray
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints

/**
 * Sentence Parser
 * @author dlwh
 */
object LiftedParser {
  type L = AnnotatedLabel
  type W = String

  class Model(factory: SentenceBeliefs.Factory, val model: ParserModel[L, W], viterbi: Boolean = false) extends StandardExpectedCounts.Model[FeaturizedSentence] with EvaluableModel[FeaturizedSentence] {
    type Marginal = ParseMarginal[AnnotatedLabel, String]
    type Inference = LiftedParser.Inference

    def featureIndex: Index[Feature] = model.featureIndex

    def initialValueForFeature(f: Feature): Double = model.initialValueForFeature(f)

    def inferenceFromWeights(weights: DenseVector[Double]) = {
      val gram = model.inferenceFromWeights(weights)
      new Inference(factory, gram, viterbi)
    }

    // evaluation
    type EvaluationResult = ParseEval.Statistics

    def evaluate(guess: FeaturizedSentence, gold: FeaturizedSentence, logResults: Boolean): ParseEval.Statistics = {
      val guessTree: Tree[String] = makeNormalTree(guess.tree)
      val goldTree: Tree[String] = makeNormalTree(gold.tree)
      val stats =  new ParseEval(Set("","''", "``", ".", ":", ",", "TOP")) apply (guessTree, goldTree)
      if (logResults) println("Guess:\n" + guessTree.render(guess.words) + "\n Gold:\n" + goldTree.render(gold.words) + "\n" + stats)
      stats
    }

    private def makeNormalTree(tree: BinarizedTree[AnnotatedLabel]):Tree[String] = {
      val chains = AnnotatedLabelChainReplacer
      Trees.debinarize(chains.replaceUnaries(tree).map(_.label))
    }

    def accumulateCounts(d: FeaturizedSentence, m: Marginal, accum: ExpectedCounts, scale: Double) {
      model.accumulateCounts(d.treeInstance, m, accum, scale)
    }
  }

  class Inference(beliefsFactory: SentenceBeliefs.Factory,
                  inf: ParserInference[L, W], viterbi: Boolean) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {

    type Marginal = ParseMarginal[AnnotatedLabel, String]
    type ExpectedCounts = StandardExpectedCounts[Feature]
    def emptyCounts = inf.emptyCounts

    def baseAugment(sent: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(sent)
    }

    def marginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
       val anchoring =  new Anchoring(inf.grammar.grammar, inf.grammar.lexicon, sent.words, aug, sent.constituentSparsity)
      if(viterbi) {
        ParseMarginal.maxDerivationMarginal(AugmentedAnchoring(inf.grammar.anchor(sent.words), anchoring))
      } else {
        inf.marginal(sent.treeInstance, anchoring)
      }
    }

    def projectGold(v: FeaturizedSentence, m: Marginal, oldAugment: SentenceBeliefs): SentenceBeliefs = {
      project(v, m, oldAugment)
    }

    def goldMarginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val anchoring =  new Anchoring(inf.grammar.grammar, inf.grammar.lexicon, sent.words, aug, sent.constituentSparsity)
      inf.goldMarginal(sent.treeInstance, anchoring)
    }

    def project(s: FeaturizedSentence, marg: Marginal, oldAugment: SentenceBeliefs): SentenceBeliefs = {
      val m = marg
      val old = oldAugment
      val info = new AnchoredSpanProjector().projectSpanPosteriors(m)
      val words = Array.tabulate(s.length) { w =>
        val oldW = old.wordBeliefs(w)
        oldW.copy(tag=oldW.tag.updated(info.botType(w, w + 1)(0 until inf.grammar.grammar.labelIndex.size)))
      }

      val spans = TriangularArray.tabulate(m.length+1) { (r, c) =>
        if(old.spanBeliefs(r, c) == null) null
        else {
          val span: SpanBeliefs = old.spanBeliefs(r, c)
          span.copy(label=span.label.updated(info.topType(r,c)))
        }
      }

      oldAugment.copy(spans=spans, wordBeliefs=words)
    }

    // annotation methods
    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val newTree: BinarizedTree[AnnotatedLabel] = m match {
        case m:ChartMarginal[AnnotatedLabel, String] => new MaxConstituentDecoder[AnnotatedLabel, String]().extractBestParse(m)
        case m:TreeMarginal[AnnotatedLabel, String] => m.tree.map(_._1)
      }
      sent.withTree(newTree)
    }
  }


  /**
   * This anchoring corresponds to the information produced by LexGovernorProjector,
   * along with the LexGrammar's information.
   * @author dlwh
   */
  final case class Anchoring[L, W](grammar: BaseGrammar[L],
                              lexicon: Lexicon[L, W],
                              words: IndexedSeq[W],
                              beliefs: SentenceBeliefs,
                              override val sparsityPattern: ChartConstraints[L]) extends CoreAnchoring[L, W] {
    // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
    // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
    // but the dynamic program does not visit all spans for all parses, only those
    // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
    // and then we divide out p(not span) for spans in the tree.

    // Also note that this code and the Visitor in LexGovernorProjector are basically
    // duals of one another. IF you change one you should change the other.

    private val notConstituent = grammar.labelIndex.size

    def length = words.length


    def addConstraints(cs: ChartConstraints[L]): CoreAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & cs)

    /**
     * The premultiplication constant.
     */
    private val normalizingPiece = beliefs.spans.data.filter(_ ne null).map { b =>
      val notCon1 = b.label(notConstituent)

      val score1 = if(notCon1 <= 0.0) 0.0 else math.log(notCon1)

      score1
    }.sum


    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int): Double = 0

    def scoreUnaryRule(begin: Int, end: Int, rule: Int): Double = {
      val parent = grammar.parent(rule)
      val sLabel = beliefs.spanBeliefs(begin, end).label(parent)
      var notASpan =  beliefs.spanBeliefs(begin, end).label(notConstituent)

      if(begin == end - 1)
        assert(notASpan < 1E-6 || notASpan == 1.0, (begin,end) + " " + beliefs.spanBeliefs(begin, end).label)

      if(notASpan <= 0.0) notASpan = 1.0

      var baseScore = 0.0
      baseScore += math.log(sLabel / notASpan)
      if(begin == 0 && end == length) {
        baseScore += normalizingPiece
      }
      baseScore

    }


    def scoreSpan(begin: Int, end: Int, label: Int): Double = {
      var baseScore = 0.0

      if(begin + 1 == end) {
        baseScore += math.log(beliefs.wordBeliefs(begin).tag(label))
      }
      assert(!baseScore.isNaN)

      baseScore
    }




  }
}
