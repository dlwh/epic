package epic.everything

import breeze.linalg._
import breeze.numerics._
import epic.trees._
import epic.parser._
import epic.parser.models._
import epic.framework._
import models.LexGrammarBundle
import breeze.util.{Encoder, OptionIndex, Index}
import epic.parser.projections.LexGovernorProjector
import breeze.collection.mutable.TriangularArray
import epic.lexicon.Lexicon

/**
 * Sentence Lexicalized Parser
 * @author dlwh
 */
object SentLexParser {
// laziness
  type L = AnnotatedLabel
  type W = String

  class Model(factory: SentenceBeliefs.Factory,
              bundle: LexGrammarBundle[L, W],
              reannotate: (BinarizedTree[L], Seq[W])=>BinarizedTree[L],
              indexed: IndexedLexFeaturizer[L, W],
              viterbi: Boolean = false,
              initFeatureValue: Feature=>Option[Double] = {(f: Feature) => None}) extends EvaluableModel[FeaturizedSentence] {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = ParseMarginal[AnnotatedLabel, String]
    type Inference = SentLexParser.Inference

    def featureIndex: Index[Feature] = indexed.index

    def initialValueForFeature(f: Feature): Double = initFeatureValue(f).getOrElse(0.0)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      (ecounts.loss, ecounts.counts)
    }


    def inferenceFromWeights(weights: DenseVector[Double]) = {
      val gram = bundle.makeGrammar(indexed, weights)
      def ann(fs: FeaturizedSentence):BinarizedTree[(L, Int)] = {
        val reannotated = reannotate(fs.tree, fs.words)

        val headed = bundle.headFinder.annotateHeadIndices(fs.tree)
        headed

      }
      new Inference(factory, gram, ann _, indexed, viterbi)
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
  }

  class Inference(beliefsFactory: SentenceBeliefs.Factory,
                  grammar: LexGrammar[AnnotatedLabel, String],
                  reannotate: (FeaturizedSentence)=>BinarizedTree[(AnnotatedLabel, Int)],
                  featurizer: IndexedLexFeaturizer[AnnotatedLabel, String],
                  viterbi: Boolean) extends AnnotatingInference[FeaturizedSentence] with ProjectableInference[FeaturizedSentence, SentenceBeliefs] {

    type Marginal = ParseMarginal[AnnotatedLabel, String]
    type ExpectedCounts = StandardExpectedCounts[Feature]
    def emptyCounts = StandardExpectedCounts.zero(featurizer.index)

    def baseAugment(sent: FeaturizedSentence): SentenceBeliefs = {
      beliefsFactory(sent)
    }


    def marginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
       val anchoring =  new Anchoring(grammar, sent.words, aug)
      if(viterbi) {
        ParseMarginal.maxDerivationMarginal(AugmentedAnchoring(anchoring, sent.constituentSparsity))
      } else {
        AugmentedAnchoring(anchoring, sent.constituentSparsity).marginal
      }
    }

    def checkForTree(aug: SentenceBeliefs, tree: BinarizedTree[L]) = {
      for (t <- tree.allChildren) t match {
        case UnaryTree( label, _, _, span) =>
          val labelScore = aug.spanBeliefs(t.span.begin, t.span.end).label(grammar.labelIndex(label))
          if (labelScore <= 0) {
            println("problem with unary: " + label + " " + span + " "
              + " " + Encoder.fromIndex(new OptionIndex(grammar.labelIndex)).decode(aug.spanBeliefs(t.span.begin, t.span.end).label.beliefs))
          }
        case _ =>
      }
      this
    }

    def projectGold(v: FeaturizedSentence, m: Marginal, oldAugment: SentenceBeliefs): SentenceBeliefs = {
      project(v, m, oldAugment)
    }


    def goldMarginal(sent: FeaturizedSentence, aug: SentenceBeliefs): Marginal = {
      val anchoring =  new Anchoring(grammar, sent.words, aug)
      new TreeMarginal(AugmentedAnchoring(anchoring), reannotate(sent))
    }

    def countsFromMarginal(sent: FeaturizedSentence,
                           marg: Marginal,
                           counts: ExpectedCounts,
                           scale: Double): ExpectedCounts = {
      marg.expectedCounts(featurizer, counts, scale)
      counts
    }

    def project(s: FeaturizedSentence, marg: Marginal, oldAugment: SentenceBeliefs): SentenceBeliefs = {
      val m = marg
      val old = oldAugment
      val info = new LexGovernorProjector(grammar).apply(m.anchoring.refined, m)
      val words = Array.tabulate(s.length) { w =>
        val oldW = old.wordBeliefs(w)
        val newGov = info.wordGovernor(w)
        axpy(1E-6, signum(oldW.governor.beliefs), newGov)
        newGov := normalize(newGov, 1)
        oldW.copy(governor=oldW.governor.updated(newGov),
          //            span=oldW.span.copy(beliefs=info.governedSpan(w)),
          tag=oldW.tag.updated(info.wordTag(w)) //),
//          maximalLabel=oldW.maximalLabel.updated(info.maximalLabelType(w))
        )

      }

      val spans = TriangularArray.tabulate(m.length+1) { (r, c) =>
        if(old.spanBeliefs(r, c) == null) null
        else {
          val span: SpanBeliefs = old.spanBeliefs(r, c)
          assert(!info.spanType(r,c).valuesIterator.exists(_.isInfinite), info.spanType(r,c))
          assert(info.spanType(r,c).sum != 0.0, info.spanType(r,c))
          assert((info.spanGovernor(r,c).sum - 1.0 abs) < 1E-4 || info.spanGovernor(r,c).sum == info.spanGovernor(r,c).length, info.spanGovernor + " " + (r,c))

          val newGov = info.spanGovernor(r,c)
          axpy(1E-6, signum(span.governor.beliefs), newGov)
          newGov := normalize(newGov, 1)

          span.copy(governor=span.governor.updated(newGov),
            label=span.label.updated(info.spanType(r,c))
          )
        }
      }

      oldAugment.copy(spans=spans, wordBeliefs=words)
    }


    // annotation methods
    def annotate(sent: FeaturizedSentence, m: Marginal): FeaturizedSentence = {
      val newTree = new MaxConstituentDecoder[AnnotatedLabel, String]().extractBestParse(m.asInstanceOf[ChartMarginal[AnnotatedLabel, String]])
      sent.withTree(newTree)
    }
  }


  /**
   * This anchoring corresponds to the information produced by LexGovernorProjector,
   * along with the LexGrammar's information.
   * @author dlwh
   */
  final class Anchoring[L, W](val lexGrammar: LexGrammar[L, W],
                              val words: IndexedSeq[W],
                              val beliefs: SentenceBeliefs) extends RefinedAnchoring[L, W] {
    // we employ the trick in Klein's thesis and in the Smith & Eisner BP paper
    // which is as follows: we want to multiply \prod_(all spans) p(span type of span or not a span)
    // but the dynamic program does not visit all spans for all parses, only those
    // in the actual parse. So instead, we premultiply by \prod_{all spans} p(not span)
    // and then we divide out p(not span) for spans in the tree.

    /*
    for(i <- 0 until words.length){
      val indices = beliefs.wordBeliefs(i).governor.beliefs.findAll(x => x * (1-x) > 1E-5)
      if(indices.nonEmpty) println(i +" " + beliefs.wordBeliefs(i).governor + " " + indices)
      else {
        val indices =  beliefs.wordBeliefs(i).governor.beliefs.findAll(x => x != 0.0 && x != 1.0)
        if(indices.nonEmpty) println(indices)
      }
      for(j <- (i+1) to words.length if beliefs.spanBeliefs(i, j) ne null) {
        val indices = beliefs.spanBeliefs(i,j).governor.beliefs.findAll(x => x * (1-x) > 1E-5)
        if(indices.nonEmpty) println((i,j) + "  " + beliefs.spanBeliefs(i,j).governor + " " + indices)
      }
    }
    */

    // Also note that this code and the Visitor in LexGovernorProjector are basically
    // duals of one another. IF you change one you should change the other.

    private val anchoring = lexGrammar.anchor(words)
    override def annotationTag: Int = anchoring.annotationTag

    private val notConstituent = grammar.labelIndex.size

    def length = words.length
    def grammar: BaseGrammar[L] = lexGrammar.grammar

    def lexicon: Lexicon[L, W] = lexGrammar.lexicon

    /**
     * The premultiplication constant.
     */
    private val normalizingPiece = beliefs.spans.data.filter(_ ne null).map { b =>
      val notCon1 = b.label(notConstituent)
      val notCon2 = b.governor(length + 1)

      val score1 = if(notCon1 <= 0.0) 0.0 else math.log(notCon1)
      val score2 = if(notCon2 <= 0.0) 0.0 else math.log(notCon2)

      score1 + score2
    }.sum

    private val attachCache = epic.util.Arrays.fillArray(length, length, Double.NaN)


    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      val head = anchoring.headIndex(ref)
      val dep = anchoring.depIndex(ref)

      // will swap these if rule types are different.
      // trying to avoid code duplication
      var headCell: SpanBeliefs = beliefs.spans(split, end)
      var depSpanCell: SpanBeliefs = beliefs.spans(begin, split)
      if(headCell == null || depSpanCell == null) return Double.NegativeInfinity

      if (!lexGrammar.isHeadOnRightForRule(rule)) {
        val cc = headCell
        headCell = depSpanCell
        depSpanCell = cc
      }

      var cached = attachCache(dep)(head)
      if (java.lang.Double.isNaN(cached)) {
        val sMax = 1.0
//        val sMax = if(lexGrammar.isHeadOnRightForRule(rule)) {
//          beliefs.wordBeliefs(dep).maximalLabel(grammar.leftChild(rule))
//        } else {
//          beliefs.wordBeliefs(dep).maximalLabel(grammar.rightChild(rule))
//        }
        val depScore = beliefs.wordBeliefs(dep).governor(head)
        val sGovScore = depSpanCell.governor(head)
        var notASpan = depSpanCell.governor(length + 1)
        if(notASpan <= 0.0) notASpan = 1.0
        val sSelfHeadScore = headCell.governor(head)
        var headSpanNotSpan = headCell.governor(length+1)
        if(headSpanNotSpan <= 0.0) headSpanNotSpan = 1.0

        cached = if(depScore <= 0.0 || sGovScore <= 0.0 || sMax <= 0.0 || sSelfHeadScore <= 0.0) {
          Double.NegativeInfinity
        } else {
          math.log(depScore * sGovScore / notASpan  *   sMax) + math.log(sSelfHeadScore/headSpanNotSpan)
        }
       // assert(cached.abs < 1E-4 || cached == Double.NegativeInfinity, cached)
        assert(!java.lang.Double.isNaN(cached))
        attachCache(dep)(head) = cached
      }

      if (cached == Double.NegativeInfinity) {
        cached
      } else {
        cached + anchoring.scoreBinaryRule(begin, split, end, rule, ref)
      }
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
      val parent = grammar.parent(rule)
      val sLabel = beliefs.spanBeliefs(begin, end).label(parent)
      var notASpan =  beliefs.spanBeliefs(begin, end).label(notConstituent)
        if(notASpan <= 0.0) notASpan = 1.0
        var baseScore = anchoring.scoreUnaryRule(begin, end, rule, ref)
        baseScore += math.log(sLabel / notASpan)
        if (begin == 0 && end == length) { // root, get the length
          val wordGovScore = beliefs.wordBeliefs(ref).governor(length)
          val sSpanGov =  beliefs.spanBeliefs(begin, end).governor(length)
          var sNotSpan2 = beliefs.spanBeliefs(begin, end).governor(length + 1)
          if (sNotSpan2 <= 0.0) sNotSpan2 = 1.0

          baseScore += math.log(wordGovScore)
//          baseScore += math.log(math.max(sMax, 1E-8))
          baseScore += math.log(sSpanGov / sNotSpan2)
//          assert(!baseScore.isNaN, s"norma: $normalizingPiece slabel: $sLabel notaspan: $notASpan ${anchoring.scoreUnaryRule(begin, end, rule, ref)}")
//          assert(!baseScore.isInfinite, s"norma: $normalizingPiece  notASpan2: $sNotSpan2 smax: $sMax slabel: $sLabel notaspan: $notASpan ${anchoring.scoreUnaryRule(begin, end, rule, ref)} $begin $end $ref ${(begin until end).map(words)} ${grammar.index.get(rule)}")
        }

        if(begin == 0 && end == length) {
          baseScore += normalizingPiece
        }
        baseScore
//      }
    }

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
      var baseScore = anchoring.scoreSpan(begin, end, label, ref)

      if(begin + 1 == end) {
        baseScore += math.log(beliefs.wordBeliefs(begin).tag(label))
      }
      assert(!baseScore.isNaN)

      baseScore
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int): Array[Int] = anchoring.validLabelRefinements(begin, end, label)

    def numValidRefinements(label: Int): Int = anchoring.numValidRefinements(label)

    def numValidRuleRefinements(rule: Int): Int = anchoring.numValidRuleRefinements(rule)

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
      anchoring.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      anchoring.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int): Int = {
      anchoring.leftChildRefinement(rule, ruleRef)
    }

    def rightChildRefinement(rule: Int, ruleRef: Int): Int = {
      anchoring.rightChildRefinement(rule, ruleRef)
    }

    def parentRefinement(rule: Int, ruleRef: Int): Int = {
      anchoring.parentRefinement(rule, ruleRef)
    }

    def childRefinement(rule: Int, ruleRef: Int): Int = {
      anchoring.childRefinement(rule, ruleRef)
    }


    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int): Int = {
      anchoring.ruleRefinementFromRefinements(r, refA, refB)
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int): Int = {
      anchoring.ruleRefinementFromRefinements(r, refA, refB, refC)
    }


    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int): Array[Int] = {
      anchoring.validCoarseRulesGivenParentRefinement(a, refA)
    }


    override def validRuleRefinementsGivenParent(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int, parentRef: Int): Array[Int] = {
      anchoring.validRuleRefinementsGivenParent(begin, splitBegin, splitEnd, end, rule, parentRef)
    }

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] =  {
      anchoring.validParentRefinementsGivenRule(begin, splitBegin, splitEnd, end, rule)
    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
      anchoring.validRuleRefinementsGivenLeftChild(begin, split, completionBegin, completionEnd, rule, childRef)
    }

    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      anchoring.validRuleRefinementsGivenRightChild(completionBegin, completionEnd, split, end, rule, childRef)
    }

    def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
      anchoring.validLeftChildRefinementsGivenRule(begin, end, completionBegin, completionEnd, rule)
    }

    def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
      anchoring.validRightChildRefinementsGivenRule(completionBegin, completionEnd, begin, end, rule)
    }
  }
}
