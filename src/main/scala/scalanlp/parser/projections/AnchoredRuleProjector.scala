package scalanlp.parser
package projections

import scalanlp.collection.mutable.TriangularArray
import scalala.library.Numerics
import scalanlp.trees.BinarizedTree
import scalanlp.tensor.sparse.OldSparseVector

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@SerialVersionUID(1L)
class AnchoredRuleProjector[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                   indexedProjections: GrammarProjections[C,L]) extends Serializable {





  /**
   * Projects an inside and outside chart to anchored rule posteriors.
   *
   * @param inside inside chart
   * @param outside outside chart
   * @param sentProb log probability of the root. probably a log partition
   * @param scorer: scorer used to produce this tree.
   * @param pruneLabel should return a threshold to determine if we need to prune. (prune if posterior <= threshold) See companion object for good choices.
   */
  def projectRulePosteriors(inside: ParseChart[L],
                            outside: ParseChart[L],
                            sentProb: Double,
                            scorer: SpanScorer[L]=SpanScorer.identity,
                            pruneLabel: SpanScorer[L]=AnchoredRuleProjector.noPruning[L]):AnchoredRuleProjector.AnchoredData = {


    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = indexedProjections.labels.coarseIndex.size;
    val numProjectedRules = indexedProjections.rules.coarseIndex.size;
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numProjectedLabels)(null);
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


    import parser.grammar;

    // fill in spans
    for(begin <- 0 until inside.length; end <- (begin + 1) to (inside.length);
        l <- inside.bot.enteredLabelIndexes(begin,end)) {
      val currentScore = inside.bot.labelScore(begin,end,l) + outside.bot.labelScore(begin,end,l) - sentProb;
      val pL = indexedProjections.labels.project(l)
      if(currentScore > pruneLabel.scoreSpan(begin,end,pL)) {
        getOrElseUpdate(lexicalScores,TriangularArray.index(begin,end),projVector())(pL) = 0
      }
    }


    // the main loop, similar to cky
    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);

        // do binaries
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.bot.labelScore(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var logTotal = Double.NegativeInfinity;

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
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              val pR = indexedProjections.rules.project(r)

              val ruleScore = grammar.ruleScore(r)

              val bScore = inside.top.labelScore(begin,split,b);
              val cScore = inside.top.labelScore(split, end, c)

              val currentScore = (bScore + cScore
                + parentScore
                + ruleScore
                + scorer.scoreBinaryRule(begin,split,end,r)  + scorer.scoreSpan(begin,end,parent)
                - sentProb);
              if(currentScore > pruneLabel.scoreBinaryRule(begin,split,end,indexedProjections.rules.project(r))) {
                val accScore = parentArray(pR)
                parentArray(pR) = Numerics.logSum(accScore,currentScore);
                logTotal = Numerics.logSum(logTotal,currentScore);
              }

            }
          }

          if(logTotal != Double.NegativeInfinity) {
            if(logTotals(index) eq null) {
              logTotals(index) = projVector;
            }
            logTotals(index)(pP) = Numerics.logSum(logTotals(index)(pP),logTotal);
          }
        }

        // do unaries. Similar to above
        for( parent <- inside.top.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.top.labelScore(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var logTotal = Double.NegativeInfinity;
          lazy val parentArray = if(unaryScores(index) eq null) {
            unaryScores(index) = projRuleVector()
            unaryScores(index)
          } else {
            unaryScores(index)
          }

          for(r <- parser.grammar.indexedUnaryRulesWithParent(parent)) {
            val c = parser.grammar.child(r)
            val pR = indexedProjections.rules.project(r)
            val ruleScore = parser.grammar.ruleScore(r)
            val score = ruleScore + inside.bot.labelScore(begin,end,c) + parentScore +
                    scorer.scoreUnaryRule(begin,end,r) - sentProb;
            if(score > pruneLabel.scoreUnaryRule(begin,end,indexedProjections.rules.project(r))) {
              val accScore = parentArray(pR);
              parentArray(pR) = Numerics.logSum(accScore,score);
              logTotal= Numerics.logSum(logTotal,score);
            }
          }

          if(logTotal != Double.NegativeInfinity) {
            if(logTotalsUnaries(index) eq null) {
              logTotalsUnaries(index) = projVector;
            }
            logTotalsUnaries(index)(pP) = Numerics.logSum(logTotalsUnaries(index)(pP),logTotal);
          }

        }


      }

    }
    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, logTotalsUnaries, binaryScores, logTotals);
  }
}


object AnchoredRuleProjector {

  /**
   * POJO for anchored rule counts. entries may be null.
   */
  case class AnchoredData(/** spanScore(trianuglarIndex)(label) = score of tag at position pos */
                          spanScores: Array[OldSparseVector],
                          /** unaryScores(triangularIndex)(rule) => score of unary from parent to child */
                          unaryScores: Array[OldSparseVector],
                          /** (triangularIndex)(parent) => same, but for unaries*/
                          logUnaryTotals: Array[OldSparseVector],
                          /** binaryScores(triangularIndex)(split)(rule) => score of unary from parent to child */
                          binaryScores: Array[Array[OldSparseVector]],
                          /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                          logBinaryTotals: Array[OldSparseVector]);

  def noPruning[L]: SpanScorer[L] = thresholdPruning(Double.NegativeInfinity);

  def thresholdPruning[L](thresh: Double):SpanScorer[L] = new SpanScorer[L] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = thresh;

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = thresh;

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = thresh;
  }

  /*
  def goldTreeForcing[L](tree: BinarizedTree[Int], pruner: SpanScorer[L] = thresholdPruning(-7)):SpanScorer[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)).+=(t.label);
      }
    }
    new SpanScorer[L] {
      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        if(gold(TriangularArray.index(begin,end))(tag) )Double.NegativeInfinity
        else pruner.scoreSpan(begin,end,tag);
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
        if(gold(TriangularArray.index(begin,end))(parent)  && gold(TriangularArray.index(begin,end))(child))
          Double.NegativeInfinity
        else pruner.scoreUnaryRule(begin,end,parent,child)
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
        if(gold(TriangularArray.index(begin,end))(parent)
                && gold(TriangularArray.index(begin,split))(leftChild)
                && gold(TriangularArray.index(split,end))(rightChild)) Double.NegativeInfinity
        else pruner.scoreBinaryRule(begin,split,end,parent,leftChild,rightChild)
      }
    }
  }
  */
}
