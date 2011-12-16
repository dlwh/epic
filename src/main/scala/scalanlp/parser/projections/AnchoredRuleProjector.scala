package scalanlp.parser
package projections

import scalanlp.collection.mutable.TriangularArray
import scalala.library.Numerics
import scalanlp.trees.BinarizedTree
import scalanlp.tensor.sparse.OldSparseVector
import sun.misc.Unsafe

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@SerialVersionUID(2L)
class AnchoredRuleProjector[C,L,W](coarseGrammar: Grammar[C],
                                   parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
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
                            pruneLabel: SpanScorer[C]=AnchoredRuleProjector.noPruning[C]):AnchoredRuleProjector.AnchoredData = {


    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numProjectedLabels = indexedProjections.labels.coarseIndex.size;
    val numProjectedRules = indexedProjections.rules.coarseIndex.size;
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numProjectedLabels)(null);
    def projVector() = {
      new OldSparseVector(numProjectedLabels,0.0, 0);
    }

    def projRuleVector() = {
      new OldSparseVector(numProjectedRules,0.0, 0);
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

    val totals = TriangularArray.raw(inside.length+1,null:OldSparseVector);
    val totalsUnaries = TriangularArray.raw(inside.length+1,null:OldSparseVector);

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
        var pruned = 0
        var totalP = 0

        // do binaries
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.bot.labelScore(begin,end,parent)+ scorer.scoreSpan(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0

          if(parentScore + inside.bot.labelScore(begin,end,parent) - sentProb > pruneLabel.scoreSpan(begin,end,parent)) {
            val rules = grammar.indexedBinaryRulesWithParent(parent)
            var ruleIndex = 0
            while(ruleIndex < rules.length) {
              val r = rules(ruleIndex)
              val b = grammar.leftChild(r)
              val c = grammar.rightChild(r)
              val ruleScore = grammar.ruleScore(r)
              val pR = indexedProjections.rules.project(r)
              ruleIndex += 1

              for(split <- inside.top.feasibleSpan(begin, end, b, c)) {
                // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
                val bScore = inside.top.labelScore(begin,split,b)
                val cScore = inside.top.labelScore(split, end, c)

                if(bScore != Double.NegativeInfinity && cScore != Double.NegativeInfinity) {
                  val currentScore = (bScore + cScore
                    + parentScore
                    + ruleScore
                    + scorer.scoreBinaryRule(begin,split,end,r)
                    - sentProb);
                  if(currentScore > pruneLabel.scoreBinaryRule(begin,split,end,pR)) {
                    val parentArray = if(binaryScores(index)(split-begin) eq null) {
                      binaryScores(index)(split-begin) = projRuleVector()
                      binaryScores(index)(split-begin)
                    } else {
                      binaryScores(index)(split-begin)
                    }
                    val count = math.exp(currentScore)
                    parentArray(pR) += count
                    total += count
                  }
                }

              }
            }
          }

          if(total != 0.0) {
            if(totals(index) eq null) {
              totals(index) = projVector;
            }
            totals(index)(pP) += total
          }
        }
        if(totalP != 0.0)
          println(pruned * 1.0 / totalP)


        // do unaries. Similar to above
        for( parent <- inside.top.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.top.labelScore(begin,end,parent);
          val pP = indexedProjections.labels.project(parent);

          var total = 0.0
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
              val count = math.exp(score)
              parentArray(pR) += count
              total += count
            }
          }

          if(total != 0.0) {
            if(totalsUnaries(index) eq null) {
              totalsUnaries(index) = projVector;
            }
            totalsUnaries(index)(pP) += total
          }

        }


      }

    }
    new AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totals);
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
                          unaryTotals: Array[OldSparseVector],
                          /** binaryScores(triangularIndex)(split)(rule) => score of unary from parent to child */
                          binaryScores: Array[Array[OldSparseVector]],
                          /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                          binaryTotals: Array[OldSparseVector]);

  def noPruning[L]: SpanScorer[L] = thresholdPruning(Double.NegativeInfinity);

  def thresholdPruning[L](thresh: Double):SpanScorer[L] = new SpanScorer[L] {
    def scoreSpan(begin: Int, end: Int, tag: Int) = thresh;

    def scoreUnaryRule(begin: Int, end: Int, rule: Int) = Double.NegativeInfinity

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = Double.NegativeInfinity
  }

  def goldTreeForcing[L](tree: BinarizedTree[Int]):SpanScorer[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)) +=(t.label);
      }
    }
    new SpanScorer[L] {
      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        if(gold(TriangularArray.index(begin,end))(tag)) Double.NegativeInfinity
        else 0.0
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
    }
  }

 def candidateTreeForcing[L](tree: BinarizedTree[Seq[Int]]):SpanScorer[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)) ++= t.label
      }
    }
    new SpanScorer[L] {
      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        if(gold(TriangularArray.index(begin,end))(tag)) Double.NegativeInfinity
        else 0.0
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0
    }
  }
}
