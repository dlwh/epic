package scalanlp.parser

import projections.{AnchoredRuleProjector, AnchoredRuleScorer}
import scalanlp.collection.mutable.TriangularArray
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Numerics

/**
 * CachingSpanScorerFactory essentially just rencodes a SpanScorer into a possibly more efficient representation.
 * @author dlwh
 */
class CachingSpanScorerFactory[L,W](chartBuilder: ChartBuilder[ParseChart,L,W], threshold: Double) extends SpanScorer.Factory[L,L,W] {
  val zero = new CKYChartBuilder[ParseChart.LogProbabilityParseChart, L,W](chartBuilder.root,
    new ZeroLexicon(chartBuilder.lexicon), Grammar.zero(chartBuilder.grammar),ParseChart.logProb)

  def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[L] = SpanScorer.identity, goldTagPolicy: GoldTagPolicy[L]):SpanScorer[L] = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores,unaryScores,_,binaryScores,_) = extractScores(s, oldScorer, goldTagPolicy)
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores)
  }

  /**
   * Projects an inside and outside chart to anchored rule posteriors.
   *
   * @param inside inside chart
   * @param outside outside chart
   * @param sentProb log probability of the root. probably a log partition
   * @param scorer: scorer used to produce this tree.
   * @param goldTags
   */
  def extractScores(s: Seq[W], scorer: SpanScorer[L], goldTags: GoldTagPolicy[L]):AnchoredRuleProjector.AnchoredData = {
    val inside = zero.buildInsideChart(s,scorer)
    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = zero.grammar.labelIndex.size
    val numProjectedRules = zero.grammar.index.size
    def projVector() = {
      new OldSparseVector(numProjectedLabels,Double.NegativeInfinity, 0);
    }

    def projRuleVector() = {
      new OldSparseVector(numProjectedRules,Double.NegativeInfinity, 0);
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if(arr(i) == null) {
        arr(i) = t;
      }
      arr(i);
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = TriangularArray.raw(inside.length+1,null:OldSparseVector)
    val unaryScores = TriangularArray.raw(inside.length+1,null:OldSparseVector);

    val logTotals = TriangularArray.raw(inside.length+1,null:OldSparseVector);
    val logTotalsUnaries = TriangularArray.raw(inside.length+1,null:OldSparseVector);

    val binaryScores = TriangularArray.raw[Array[OldSparseVector]](inside.length+1,null);
    for(begin <- 0 until inside.length; end <- (begin + 1) to inside.length) {
      val numSplits = end - begin;
      if(!inside.bot.enteredLabelIndexes(begin,end).isEmpty) // is there anything to put here?
        binaryScores(TriangularArray.index(begin,end)) = Array.fill(numSplits)(null:OldSparseVector)
    }

    import zero.grammar


    // fill in spans
    for(begin <- 0 until inside.length; end <- (begin + 1) to (inside.length);
        l <- inside.bot.enteredLabelIndexes(begin,end)) {
      val currentScore = scorer.scoreSpan(begin,end,l)
      if(currentScore > threshold || goldTags.isGoldTag(begin, end, l)) {
        getOrElseUpdate(lexicalScores,TriangularArray.index(begin,end),projVector())(l) = currentScore
      }
    }


    // the main loop, similar to cky
    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);

        // do binaries
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {
          for(split <- (begin+1) until end) {
            lazy val parentArray = if(binaryScores(index)(split-begin) eq null) {
              binaryScores(index)(split-begin) = projRuleVector()
              binaryScores(index)(split-begin)
            } else {
              binaryScores(index)(split-begin)
            }

            // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
            val rules = grammar.indexedBinaryRulesWithParent(parent)
            for(r <- rules) {
              val currentScore = scorer.scoreBinaryRule(begin,split,end,r)
              if(currentScore > Double.NegativeInfinity)
                parentArray(r) = currentScore
            }
          }

        }

        // do unaries. Similar to above
        for( parent <- inside.top.enteredLabelIndexes(begin,end)) {
          lazy val parentArray = if(unaryScores(index) eq null) {
            unaryScores(index) = projRuleVector()
            unaryScores(index)
          } else {
            unaryScores(index)
          }

          for(r <- grammar.indexedUnaryRulesWithParent(parent)) {
            val score =  scorer.scoreUnaryRule(begin,end,r)
            if(score > Double.NegativeInfinity) {
              val accScore = parentArray(r);
              parentArray(r) = Numerics.logSum(accScore,score);
            }
          }


        }


      }

    }
    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, logTotalsUnaries, binaryScores, logTotals);
  }

}