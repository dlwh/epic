package epic.everything.models

import epic.parser.models._
import epic.trees._
import breeze.util.Index
import epic.framework.{StandardExpectedCounts, ProjectableInference, Feature}
import breeze.linalg.DenseVector
import epic.everything.ProcessedDocument
import epic.parser._
import models.LexGrammarBundle
import projections.LexGovernorProjector
import breeze.collection.mutable.TriangularArray


/**
 * Model class for the lexicalized parser in the DocumentBeliefs framework.
 *
 * @author dlwh
 */
object DocLexParser {
  // laziness
  type L = AnnotatedLabel
  type W = String

  class Model(factory: DocumentBeliefs.Factory,
              bundle: LexGrammarBundle[L, W],
              reannotate: (BinarizedTree[L], Seq[W])=>BinarizedTree[L],
              indexed: IndexedLexFeaturizer[L, W],
              initFeatureValue: Feature=>Option[Double] = {(f: Feature) => None}) extends DocumentAnnotatingModel {
    type ExpectedCounts = StandardExpectedCounts[Feature]
    type Marginal = DocLexParser.Marginal[ParseMarginal[AnnotatedLabel, String]]
    type Inference = DocLexParser.Inference

    def featureIndex: Index[Feature] = indexed.index

    def initialValueForFeature(f: Feature): Double = initFeatureValue(f).getOrElse(0.0)

    def expectedCountsToObjective(ecounts: ExpectedCounts): (Double, DenseVector[Double]) = {
      (ecounts.loss, ecounts.counts)
    }

    def inferenceFromWeights(weights: DenseVector[Double]) = {
      val gram = bundle.makeGrammar(indexed, weights)
      def ann(tree: BinarizedTree[L], words: Seq[W]):BinarizedTree[(L, Int)] = {
        val reannotated = reannotate(tree, words)
        val headed = bundle.headFinder.annotateHeadIndices(reannotated)
        headed

      }
      new Inference(factory, gram, ann _, indexed)
    }


    // evaluation
    type EvaluationResult = ParseEval.Statistics

    def evaluate(guess: ProcessedDocument, gold: ProcessedDocument): ParseEval.Statistics = {
      val individual = for ( (gs, gd) <- guess.sentences zip gold.sentences) yield {

        val stats =  new ParseEval(Set("","''", "``", ".", ":", ",", "TOP")) apply (makeNormalTree(gs.tree), makeNormalTree(gd.tree))

        stats
      }

      individual.reduceLeft(_ + _)
    }

    def makeNormalTree(tree: BinarizedTree[AnnotatedLabel]):Tree[String] = {
      val chains = AnnotatedLabelChainReplacer
      Trees.debinarize(chains.replaceUnaries(tree).map(_.label))
    }
  }

  case class Marginal[+Inner<:epic.framework.Marginal](sentences: IndexedSeq[Inner]) extends epic.framework.Marginal {
    val logPartition = sentences.map(_.logPartition).sum
  }

  class Inference(beliefsFactory: DocumentBeliefs.Factory,
                  grammar: LexGrammar[AnnotatedLabel, String],
                  reannotate: (BinarizedTree[AnnotatedLabel], Seq[String])=>BinarizedTree[(AnnotatedLabel, Int)],
                  featurizer: IndexedLexFeaturizer[AnnotatedLabel, String]) extends DocumentAnnotatingInference with ProjectableInference[ProcessedDocument, DocumentBeliefs] {

    type Marginal = DocLexParser.Marginal[ParseMarginal[AnnotatedLabel, String]]
    type ExpectedCounts = StandardExpectedCounts[Feature]
    def emptyCounts = StandardExpectedCounts.zero(featurizer.index)

    def baseAugment(doc: ProcessedDocument): DocumentBeliefs = {
      beliefsFactory(doc)
    }


    def marginal(doc: ProcessedDocument, aug: DocumentBeliefs): Marginal = {
      val trees = doc.treeInstances
      val lenses = for( (b, s) <- aug.sentences zip doc.sentences) yield adapt(s.words, b)

      val marginals = for (i <- 0 until trees.length) yield {
        AugmentedAnchoring(lenses(i), doc.sentences(i).sparsity).marginal
      }

      Marginal(marginals)
    }

    def projectGold(v: ProcessedDocument, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      project(v, m, oldAugment)
    }


    def goldMarginal(doc: ProcessedDocument, aug: DocumentBeliefs): Marginal = {
      val trees = doc.treeInstances
      val lenses = for( (b, s) <- aug.sentences zip doc.sentences) yield adapt(s.words, b)

      val marginals = for (i <- 0 until trees.length) yield {
        new TreeMarginal(AugmentedAnchoring(lenses(i), doc.sentences(i).sparsity), reannotate(trees(i).tree, trees(i).words))
      }

      Marginal(marginals)
    }

    def countsFromMarginal(doc: ProcessedDocument,
                           marg: Marginal,
                           counts: ExpectedCounts,
                           scale: Double): ExpectedCounts = {
      for( i <- (0 until marg.sentences.length)) {
        marg.sentences(i).expectedCounts(featurizer, counts, scale)
      }
      counts
    }


    def adapt(words: IndexedSeq[String], t: SentenceBeliefs): Anchoring[AnnotatedLabel, String] = {
      new Anchoring(grammar, words, t)
    }

    def project(v: ProcessedDocument, marg: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
      val sentences = for ( (old, (s, m)) <- oldAugment.sentences.zip(v.sentences zip marg.sentences)) yield {
        val info = new LexGovernorProjector(grammar).apply(m.anchoring.refined, m)
        val words = Array.tabulate(s.length) { w =>
          val oldW = old.wordBeliefs(w)
          oldW.copy(governor=oldW.governor.updated(info.wordGovernor(w)),
//            span=oldW.span.copy(beliefs=info.governedSpan(w)),
            tag=oldW.tag.updated(info.wordTag(w)),
            maximalLabel=oldW.maximalLabel.updated(info.maximalLabelType(w))
                    )

        }

        val spans = TriangularArray.tabulate(m.length+1) { (r, c) =>
          if(old.spanBeliefs(r, c) == null) null
          else {
            val span: SpanBeliefs = old.spanBeliefs(r, c)
            assert(!info.spanType(r,c).valuesIterator.exists(_.isInfinite), info.spanType(r,c))
            assert(info.spanType(r,c).sum != 0.0, info.spanType(r,c))
            span.copy(governor=span.governor.updated(info.spanGovernor(r, c)),
            label=span.label.updated(info.spanType(r,c))
            )
          }
        }

        old.copy(spans=spans, wordBeliefs=words)
      }
      new DocumentBeliefs(sentences)
    }


    // annotation methods
    def annotate(doc: ProcessedDocument, m: Marginal): ProcessedDocument = {
      val newSentences = for ( (s, marg) <- doc.sentences zip m.sentences) yield {
        // TODO: ugh
        val newTree = new MaxConstituentDecoder[AnnotatedLabel, String]().extractBestParse(marg.asInstanceOf[ChartMarginal[AnnotatedLabel, String]])
        s.copy(tree=newTree)
      }

      doc.copy(newSentences)

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

      val score1 = if(notCon1 < 1E-4) 0.0 else math.log(notCon1)
      val score2 = if(notCon2 < 1E-4) 0.0 else math.log(notCon2)

      score1 + score2
    }.sum



    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int): Double = {
      val head = anchoring.headIndex(ref)
      val dep = anchoring.depIndex(ref)
      val depScore = beliefs.wordBeliefs(dep).governor(head)
      if(beliefs.spans(begin, split) == null || beliefs.spans(split, end) == null) return Double.NegativeInfinity

      val score = if (lexGrammar.isRightRule(rule)) {
        val sGovScore = beliefs.spans(begin, split).governor(head)
        var notASpan = beliefs.spanBeliefs(begin, split).governor(length + 1)
        if(notASpan == 0.0) notASpan = 1.0
        val sMax = beliefs.wordBeliefs(dep).maximalLabel(grammar.leftChild(rule))
        if(depScore == 0.0 || sGovScore == 0.0 || sMax == 0.0) {
          Double.NegativeInfinity
        } else (
         anchoring.scoreBinaryRule(begin, split, end, rule, ref)
             + math.log(depScore * sGovScore / notASpan  *   sMax)
        )
      } else {// head on the right
        val sGovScore = beliefs.spans(split, end).governor(head)
        var notASpan = beliefs.spanBeliefs(split, end).governor(length + 1)
        if(notASpan < 1E-4) notASpan = 1.0
        val sMax = beliefs.wordBeliefs(dep).maximalLabel(grammar.rightChild(rule))
        if(depScore == 0.0 || sGovScore == 0.0 || sMax == 0.0) {
          Double.NegativeInfinity
        } else (
          anchoring.scoreBinaryRule(begin, split, end, rule, ref)
            + math.log(depScore * sGovScore / notASpan  *   sMax)
        )
      }
      score
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int): Double = {
      val parent = grammar.parent(rule)
      val sLabel = beliefs.spanBeliefs(begin, end).label(parent)
      var notASpan = beliefs.spanBeliefs(begin, end).label(notConstituent)
      if(sLabel == 0.0) {
        Double.NegativeInfinity
      } else {
        if(notASpan < 1E-4) notASpan = 1.0
        var baseScore = anchoring.scoreUnaryRule(begin, end, rule, ref)
        baseScore += math.log(sLabel / notASpan)
        if (begin == 0 && end == length) { // root, get the length

          val sSpanGov =  beliefs.spanBeliefs(begin, end).governor(length)
          var sNotSpan2 = beliefs.spanBeliefs(begin, end).governor(length + 1)
          if (sNotSpan2 < 1E-4) sNotSpan2 = 1.0

          val sMax = beliefs.wordBeliefs(ref).maximalLabel(parent)
          baseScore += math.log(beliefs.wordBeliefs(ref).governor(length) * sMax)
          baseScore += math.log(sSpanGov / sNotSpan2)
          //            * beliefs.wordBeliefs(ref).span(TriangularArray.index(begin,end))
        }

        if(begin == 0 && end == length) {
          baseScore += normalizingPiece
        }
        baseScore
      }
    }

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int): Double = {
      var baseScore = anchoring.scoreSpan(begin, end, label, ref)

      if(begin + 1 == end) {
        baseScore += math.log(beliefs.wordBeliefs(begin).tag(label))
      }

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
  }

}




