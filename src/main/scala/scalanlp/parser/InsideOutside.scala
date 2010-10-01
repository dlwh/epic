package scalanlp.parser

import scalanlp.collection.mutable.SparseArray
import scalala.tensor.counters.LogCounters.LogDoubleCounter
import scalala.tensor.Vector;
import scalala.tensor.sparse.SparseVector;
import scalanlp.math.Numerics.logSum;

import ParseChart._;
import ChartParser._;

/**
 * 
 * @author dlwh
 */
class InsideOutside[L,W](parser: ChartParser[LogProbabilityParseChart,L,W]) {
  def this(root: L, g: Grammar[L], lexicon: Lexicon[L,W])  = {
    this(new CKYParser[ParseChart.LogProbabilityParseChart,L,W](root,lexicon,g,logProb));
  }

  def grammar = parser.grammar;
  def lexicon = parser.lexicon;
  def root = parser.root;


  def expectedCounts(words: Seq[W], validSpan:ChartParser.SpanFilter =ChartParser.defaultFilterBoxed):ExpectedCounts = {
    val inside = parser.buildInsideChart(words);
    val outside = parser.buildOutsideChart(inside);
    val totalProb = inside.labelScore(0, words.length, root);


    val wordCounts = grammar.fillSparseArray(LogDoubleCounter[W]());
    // handle lexical productions:
    for(i <- 0 until words.length) {
      val w = words(i);
      for(l <- inside.enteredLabelIndexes(i,i+1) if isTag(l)) {
        val iScore = inside.labelScore(i,i+1,l);
        val oScore = outside.labelScore(i,i+1,l);
        wordCounts(l)(w) =  (iScore + oScore) - totalProb;
      }
    }

    val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity)));
    // handle binary rules
    for {
      span <- 2 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      (b,binaryRules) <- grammar.allBinaryRules;
      if inside.canStartHere(begin, end, b);
      (c,parentVector) <- binaryRules;
      split <- inside.feasibleSpan(begin, end, b, c)
    } {
      val bScore = inside.labelScore(begin, split,b);
      if (!bScore.isInfinite) {
        val cScore = inside.labelScore(split, end, c)
        if (!cScore.isInfinite) {
          var i = 0;
          while(i < parentVector.used) {
            val a = parentVector.index(i);
            val rScore = parentVector.data(i);
            val aScore = outside.labelScore(begin,end,a);
            i += 1;
            if((validSpan eq defaultFilterBoxed) || validSpan(begin,end,a)) {
              val prob = bScore + cScore + aScore + rScore - totalProb;
              binaryRuleCounts(a)(b)(c) = logSum(binaryRuleCounts(a)(b)(c),prob);
            }
          }
        }
      }
    }

    val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity));
    for {
      span <- 1 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      b <- inside.enteredLabelIndexes(begin, end)
    } {
      val bScore = inside.labelScore(begin, end,b);
      if (!bScore.isInfinite) {
        val parentVector = grammar.unaryRulesByIndexedChild(b);
        var i = 0;
        while(i < parentVector.used) {
          val a = parentVector.index(i);
          val rScore = parentVector.data(i);
          val aScore = outside.labelScore(begin,end,a);
          i += 1;
          if((validSpan eq defaultFilterBoxed) || validSpan(begin,end,a)) {
            val prob = bScore + aScore + rScore - totalProb;
            unaryRuleCounts(a)(b) = logSum(unaryRuleCounts(a)(b),prob);
          }
        }
      }
    }
    ExpectedCounts(binaryRuleCounts, unaryRuleCounts, wordCounts, totalProb);
  }


  final case class ExpectedCounts(
    binaryRuleCounts: SparseArray[SparseArray[Vector]], // parent -> lchild -> rchild -> counts;
    unaryRuleCounts: SparseArray[Vector], // parent -> child -> counts;
    wordCounts: SparseArray[LogDoubleCounter[W]], // parent -> word -> counts;
    var logProb: Double
  ) {
    def +=(c: ExpectedCounts) = {
      val ExpectedCounts(bCounts,uCounts,wCounts,tProb) = c;

      for( (k1,c) <- bCounts;
          (k2,vec) <- c) {
        logAdd(binaryRuleCounts(k1)(k2),vec);
      }

      for( (k,vec) <- uCounts) {
        logAdd(unaryRuleCounts(k),vec);
      }

      for( (k,vec) <- wCounts) {
        logAdd(wordCounts(k),vec);
      }

      logProb += tProb;
      this;
    }
  }


  private val isTag = new collection.mutable.BitSet();
  lexicon.tags.foreach {l => isTag += grammar.index(l)};

  def logAdd(v: Vector, v2: Vector) {
    for( (i,w) <- v2.activeElements) {
      v(i) = logSum(v(i),w);
    }
  }

  def logAdd[W](v: LogDoubleCounter[W], v2: LogDoubleCounter[W]) {
    for( (i,w) <- v2.iterator) {
      v(i) = logSum(v(i),w);
    }
  }
}