package scalanlp.parser

import projections.AnchoredRuleScorer
import scalala.tensor.sparse.SparseVector
import scalanlp.collection.mutable.TriangularArray

/**
 *
 * @author dlwh
 */
class CachingSpanScorerFactory[L,W](chartBuilder: ChartBuilder[ParseChart,L,W]) extends SpanScorer.Factory[L,L,W] {
  val zero = new CKYChartBuilder[ParseChart.LogProbabilityParseChart, L,W](chartBuilder.root,
    new ZeroLexicon(chartBuilder.lexicon), new ZeroGrammar(chartBuilder.grammar),ParseChart.logProb);


  def mkSpanScorer(s: Seq[W], oldScorer: SpanScorer[L] = SpanScorer.identity):SpanScorer[L] = {
    val zeroSparseVector = new SparseVector(chartBuilder.grammar.index.size,0)
    zeroSparseVector.default = 0.0;
    val logTotals = TriangularArray.raw(s.length+1,zeroSparseVector);
    val (lexicalScores,unaryScores,binaryScores) = extractScores(s, oldScorer);
    new AnchoredRuleScorer(lexicalScores, unaryScores, binaryScores, logTotals, logTotals);
  }

  private def extractScores(s: Seq[W], scorer: SpanScorer[L]) = {
    val inside = zero.buildInsideChart(s,scorer);

    // preliminaries: we're not going to fill in everything: some things will be null.
    // all of this is how to deal with it.
    val numLabels = zero.grammar.index.size;
    def projFill[T>:Null<:AnyRef:ClassManifest]() = Array.fill[T](numLabels)(null);
    def projVector() = {
      val vec = new SparseVector(numLabels,0);
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

    val binaryScores = TriangularArray.raw[Array[Array[Array[SparseVector]]]](inside.length+1,null);
    for(begin <- 0 until inside.length; end <- (begin + 1) to inside.length) {
      val numSplits = end - begin;
      if(!inside.bot.enteredLabelIndexes(begin,end).isEmpty) // is there anything to put here?
        binaryScores(TriangularArray.index(begin,end)) = Array.fill(numSplits)(projFill[Array[SparseVector]]);
    }


    // fill in lexicals
    for(begin <- 0 until inside.length; end = begin + 1;
        l <- inside.bot.enteredLabelIndexes(begin,end)) {
      val currentScore = scorer.scoreLexical(begin,end,l);
      if(currentScore != Double.NegativeInfinity)
        lexicalScores(begin)(l) = currentScore;
    }
    // the main loop, similar to cky
    for(diff <- 1 to inside.length) {
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val index = TriangularArray.index(begin,end);

        // do binaries
        for( parent <- inside.bot.enteredLabelIndexes(begin,end)) {

          for(split <- (begin+1) until end) {
            lazy val parentArray = if(binaryScores(index)(split-begin)(parent) eq null) {
              binaryScores(index)(split-begin)(parent) = projFill[SparseVector]();
              binaryScores(index)(split-begin)(parent)
            } else {
              binaryScores(index)(split-begin)(parent);
            }

            // P(sAt->sBu uCt | sAt) \propto \sum_{latent} O(A-x,s,t) r(A-x ->B-y C-z) I(B-y,s,u) I(C-z, s, u)
            for(b <- inside.top.enteredLabelIndexes(begin,split)) {
              val cRules = zero.grammar.binaryRulesByIndexedParent(parent)(b);
              var i = 0
              while(i < cRules.used) {
                val c = cRules.index(i);
                val ruleScore = cRules.data(i)
                val currentScore = scorer.scoreBinaryRule(begin,split,end,parent,b,c);
                if(currentScore != Double.NegativeInfinity) {
                  val accScore = getOrElseUpdate(parentArray,b,projVector())(c);
                  parentArray(b)(c) = currentScore;
                }
                i+=1;
              }
            }
          }
        }

        // do unaries. Similar to above
        for( parent <- inside.top.enteredLabelIndexes(begin,end)) {
          lazy val parentArray = if(unaryScores(index) eq null) {
            unaryScores(index) = projFill[SparseVector];
            getOrElseUpdate(unaryScores(index), parent, projVector());
          } else {
            getOrElseUpdate(unaryScores(index), parent,projVector())
          }

          for( (c,ruleScore) <- zero.grammar.unaryRulesByIndexedParent(parent)) {
            val score =  scorer.scoreUnaryRule(begin,end,parent,c);
            if(score != Double.NegativeInfinity) {
              parentArray(c) = score;
            }
          }
        }
      }
    }

    (lexicalScores,unaryScores,binaryScores)
  }
}