package scalanlp.parser
package projections

import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.TriangularArray
import scalanlp.math.Numerics
import scalanlp.trees.BinarizedTree

/**
 * Used for computed the expected number of anchored rules that occur at each span/split.
 * @author dlwh
 */
@serializable
@SerialVersionUID(1L)
class AnchoredRuleProjector[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W],
                                   indexedProjections: ProjectionIndexer[C,L]) {





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
    val numProjectedLabels = indexedProjections.coarseIndex.size;
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numProjectedLabels)(null);
    def projVector() = {
      val vec = new SparseVector(numProjectedLabels,0);
      vec.default = Double.NegativeInfinity;
      vec
    }

    def getOrElseUpdate[T<:AnyRef](arr: Array[T], i: Int, t : =>T) = {
      if(arr(i) == null) {
        arr(i) = t;
      }
      arr(i);
    }

    // The data, and initialization. most things init'd to null
    val lexicalScores = Array.fill(inside.length)(projVector())
    val unaryScores = TriangularArray.raw(inside.length+1,null:Array[SparseVector]);

    val logTotals = TriangularArray.raw(inside.length+1,null:SparseVector);
    val logTotalsUnaries = TriangularArray.raw(inside.length+1,null:SparseVector);

    val binaryScores = TriangularArray.raw[Array[Array[Array[SparseVector]]]](inside.length+1,null);
    for(begin <- 0 until inside.length; end <- (begin + 1) to inside.length) {
      val numSplits = end - begin;
      if(!inside.bot.enteredLabelIndexes(begin,end).isEmpty) // is there anything to put here?
        binaryScores(TriangularArray.index(begin,end)) = Array.fill(numSplits)(projFill[Array[SparseVector]]);
    }


    import parser.grammar;

    // fill in lexicals
    for(begin <- 0 until inside.length; end = begin + 1;
        l <- inside.bot.enteredLabelIndexes(begin,end)) {
      val accScore = lexicalScores(begin)(indexedProjections.project(l));
      val currentScore = inside.bot.labelScore(begin,end,l) + outside.bot.labelScore(begin,end,l) - sentProb;
      val pL = indexedProjections.project(l)
      if(currentScore > pruneLabel.scoreLexical(begin,end,pL))
        lexicalScores(begin)(pL) = 0
    }


    // the main loop, similar to cky
    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);

        // do binaries
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.bot.labelScore(begin,end,parent);
          val pP = indexedProjections.project(parent);

          var logTotal = Double.NegativeInfinity;

          for(split <- (begin+1) until end) {
            lazy val parentArray = if(binaryScores(index)(split-begin)(pP) eq null) {
              binaryScores(index)(split-begin)(pP) = projFill[SparseVector]();
              binaryScores(index)(split-begin)(pP)
            } else {
              binaryScores(index)(split-begin)(pP);
            }

            // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
            var ruleIndex = 0;
            val rules = grammar.binaryRulesByIndexedParent(parent)
            val numRules = rules.activeLength;
            while(ruleIndex < numRules) {
              val b = rules.indexAt(ruleIndex);
              val cRules = rules.valueAt(ruleIndex);
              ruleIndex += 1;
              val bScore = inside.top.labelScore(begin,split,b);
              if(bScore != Double.NegativeInfinity) {
                val pB = indexedProjections.project(b);
                var i = 0
                while(i < cRules.used) {
                  val c = cRules.index(i);
                  val ruleScore = cRules.data(i)
                  val pC = indexedProjections.project(c);
                  val currentScore = (bScore + inside.top.labelScore(split,end,c)
                          + parentScore + ruleScore + scorer.scoreBinaryRule(begin,split,end,parent,b,c) - sentProb);
                  if(currentScore > pruneLabel.scoreBinaryRule(begin,split,end,pP,pB,pC)) {
                    val accScore = getOrElseUpdate(parentArray,pB,projVector())(pC);
                    parentArray(pB)(pC) = Numerics.logSum(accScore,currentScore);
                    logTotal = Numerics.logSum(logTotal,currentScore);
                  }
                  i+=1;
                }
              }
            }


          }

          if(logTotal != Double.NegativeInfinity) {
            if(logTotals(index) eq null) {
              logTotals(index) = projVector;
            }
            logTotals(index)(pP) = logTotal;
          }
        }

        // do unaries. Similar to above
        for( parent <- inside.top.enteredLabelIndexes(begin,end)) {
          val parentScore = outside.top.labelScore(begin,end,parent);
          val pP = indexedProjections.project(parent);

          var logTotal = Double.NegativeInfinity;
          lazy val parentArray = if(unaryScores(index) eq null) {
            unaryScores(index) = projFill[SparseVector];
            getOrElseUpdate(unaryScores(index), pP, projVector());
          } else {
            getOrElseUpdate(unaryScores(index), pP,projVector())
          }

          var ruleIndex = 0;
          val rules = parser.grammar.unaryRulesByIndexedParent(parent);
          while(ruleIndex < rules.used) {
            val c = rules.index(ruleIndex);
            val ruleScore = rules.data(ruleIndex);
            ruleIndex += 1;
            val score = ruleScore + inside.bot.labelScore(begin,end,c) + parentScore +
                    scorer.scoreUnaryRule(begin,end,parent,c) - sentProb;
            val pC = indexedProjections.project(c);
            if(score > pruneLabel.scoreUnaryRule(begin,end,pP,pC)) {
              val accScore = parentArray(pC);
              parentArray(pC) = Numerics.logSum(accScore,score);
              logTotal= Numerics.logSum(logTotal,score);
            }
          }

          if(logTotal != Double.NegativeInfinity) {
            if(logTotalsUnaries(index) eq null) {
              logTotalsUnaries(index) = projVector;
            }
            logTotalsUnaries(index)(pP) = logTotal;
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
  case class AnchoredData(/** lexicalScore(pos)(label) = score of tag at position pos */
                          lexicalScores: Array[SparseVector],
                          /** unaryScores(triangularIndex)(parent)(child) => score of unary from parent to child */
                          unaryScores: Array[Array[SparseVector]],
                          /** (triangularIndex)(parent) => same, but for unaries*/
                          logUnaryTotals: Array[SparseVector],
                          /** binaryScores(triangularIndex)(split)(parent)(lchild)(rchild) => score of unary from parent to child */
                          binaryScores: Array[Array[Array[Array[SparseVector]]]],
                          /** (triangularIndex)(parent) => sum of all binary rules at parent. */
                          logBinaryTotals: Array[SparseVector]);

  def noPruning[L]: SpanScorer[L] = thresholdPruning(Double.NegativeInfinity);

  def thresholdPruning[L](thresh: Double):SpanScorer[L] = new SpanScorer[L] {
    def scoreLexical(begin: Int, end: Int, tag: Int) = thresh;

    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = thresh;

    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = thresh;
  }

  def goldTreeForcing[L](tree: BinarizedTree[Int], pruner: SpanScorer[L] = thresholdPruning(-7)):SpanScorer[L] ={
    val gold = TriangularArray.raw(tree.span.end+1,collection.mutable.BitSet());
    if(tree != null) {
      for( t <- tree.allChildren) {
        gold(TriangularArray.index(t.span.start,t.span.end)).+=(t.label);
      }
    }
    new SpanScorer[L] {
      def scoreLexical(begin: Int, end: Int, tag: Int) = {
        if(gold(TriangularArray.index(begin,end))(tag) )Double.NegativeInfinity
        else pruner.scoreLexical(begin,end,tag);
      }

      def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
        if(gold(TriangularArray.index(begin,end))(parent)  && gold(TriangularArray.index(begin,end))(child))
          Double.NegativeInfinity
        else pruner.scoreUnaryRule(begin,end,parent,child)
      }

      def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
        if(gold(TriangularArray.index(begin,end))(parent)
                && gold(TriangularArray.index(begin,split))(leftChild)
                && gold(TriangularArray.index(split,end))(rightChild)) Double.NegativeInfinity
        else pruner.scoreBinaryRule(begin,split,end,parent,leftChild,rightChild)
      }
    }
  }
}
