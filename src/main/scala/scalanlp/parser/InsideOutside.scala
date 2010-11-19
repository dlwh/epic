package scalanlp.parser

import scalanlp.collection.mutable.SparseArray
import scalala.tensor.counters.Counters.DoubleCounter
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalala.tensor.Vector

import ParseChart._;
import ChartBuilder._;
import InsideOutside._;

import math.exp;

/**
 * 
 * @author dlwh
 */
class InsideOutside[L,W](val parser: ChartBuilder[LogProbabilityParseChart,L,W]) {
  def this(root: L, g: Grammar[L], lexicon: Lexicon[L,W])  = {
    this(new CKYChartBuilder[ParseChart.LogProbabilityParseChart,L,W](root,lexicon,g,logProb));
  }

  def grammar = parser.grammar;
  def lexicon = parser.lexicon;
  def root = parser.root;

  def expectedCounts(words: Seq[W], validSpan: SpanScorer =defaultScorer):ExpectedCounts[W] = {
    val inside = parser.buildInsideChart(words, validSpan);
    val outside = parser.buildOutsideChart(inside, validSpan);
    val totalProb = inside.labelScore(0, words.length, root);

    val wordCounts = computeWordCounts(words, inside, outside, totalProb)
    val binaryRuleCounts = computeBinaryCounts(words, inside, outside, validSpan, totalProb)
    val unaryRuleCounts = computeUnaryCounts(words, inside, outside, validSpan, totalProb)

    ExpectedCounts(binaryRuleCounts, unaryRuleCounts, wordCounts, totalProb);
  }

  private def computeWordCounts(words: scala.Seq[W],
                        inside: LogProbabilityParseChart[L],
                        outside: LogProbabilityParseChart[L],
                        totalProb: Double): SparseArray[DoubleCounter[W]] = {
    val wordCounts = grammar.fillSparseArray(DoubleCounter[W]());
    // handle lexical productions:
    for (i <- 0 until words.length) {
      val w = words(i);
      for (l <- inside.enteredLabelIndexes(i, i + 1) if isTag(l)) {
        val iScore = inside.labelScore(i, i + 1, l);
        val oScore = outside.labelScore(i, i + 1, l);
        wordCounts.getOrElseUpdate(l)(w) += exp(iScore + oScore - totalProb);
      }
    }
    wordCounts
  }

  private def computeBinaryCounts(words: scala.Seq[W],
                                  inside: LogProbabilityParseChart[L],
                                  outside: LogProbabilityParseChart[L],
                                  validSpan: SpanScorer, totalProb: Double) = {
    val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(0.0)));
    // handle binary rules
    for{
      span <- 2 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      (b, binaryRules) <- grammar.allBinaryRules
      if inside.canStartHere(begin, end, b);
      (c, parentVector) <- binaryRules;
      split <- inside.feasibleSpan(begin, end, b, c)
    } {
      val bScore = inside.labelScore(begin, split, b);
      if (!bScore.isInfinite) {
        val cScore = inside.labelScore(split, end, c)
        if (!cScore.isInfinite) {
          var i = 0;
          while (i < parentVector.used) {
            val a = parentVector.index(i);
            val rScore = parentVector.data(i);
            val aScore = outside.labelScore(begin, end, a);
            i += 1;
            if (!aScore.isInfinite) {
              val prob = bScore + cScore + aScore + rScore + validSpan.scoreBinaryRule(begin,split, end,a,b,c)  - totalProb;
              binaryRuleCounts.getOrElseUpdate(a).getOrElseUpdate(b)(c) += exp(prob);
              assert(binaryRuleCounts(a)(b)(c) >= exp(prob),binaryRuleCounts(a)(b)(c) + " " + exp(prob));
            }
          }
        }
      }
    }
    binaryRuleCounts
  }

  private def computeUnaryCounts(words: scala.Seq[W],
                                 inside: LogProbabilityParseChart[L],
                                 outside: LogProbabilityParseChart[L],
                                 validSpan: SpanScorer,
                                 totalProb: Double): SparseArray[Vector] = {
    val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(0.0));
    for{
      span <- 1 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      b <- inside.enteredLabelIndexes(begin, end)
    } {
      val bScore = inside.labelScore(begin, end, b);
      if (!bScore.isInfinite) {
        val parentVector = grammar.unaryRulesByIndexedChild(b);
        var i = 0;
        while (i < parentVector.used) {
          val a = parentVector.index(i);
          val rScore = parentVector.data(i);
          val aScore = outside.labelScore(begin, end, a);
          val prob = bScore + aScore + rScore + validSpan.scoreUnaryRule(begin,end,a,b) - totalProb;
          unaryRuleCounts.getOrElseUpdate(a)(b) += exp(prob);
          i += 1;
        }
      }
    }
    unaryRuleCounts
  }


  private val isTag = new collection.mutable.BitSet();
  lexicon.tags.foreach {l => isTag += grammar.index(l)};
}

object InsideOutside {

  final case class ExpectedCounts[W](
    binaryRuleCounts: SparseArray[SparseArray[Vector]], // parent -> lchild -> rchild -> counts;
    unaryRuleCounts: SparseArray[Vector], // parent -> child -> counts;
    wordCounts: SparseArray[DoubleCounter[W]], // parent -> word -> counts;
    var logProb: Double
  ) {

    def this(g: Grammar[_]) = this(g.fillSparseArray(g.fillSparseArray(g.mkVector(0.0))),
                                   g.fillSparseArray(g.mkVector(0.0)),
                                   g.fillSparseArray(DoubleCounter[W]()), 0.0);

    def decode[L](g: Grammar[L]) = (decodeRules(g,binaryRuleCounts,unaryRuleCounts),decodeWords(g,wordCounts));

    def +=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,uCounts,wCounts,tProb) = c;

      for( (k1,c) <- bCounts;
          (k2,vec) <- c) {
        binaryRuleCounts.getOrElseUpdate(k1).getOrElseUpdate(k2) += vec;
      }

      for( (k,vec) <- uCounts) {
        unaryRuleCounts.getOrElseUpdate(k) += vec;
      }

      for( (k,vec) <- wCounts) {
        wordCounts.getOrElseUpdate(k) += vec;
      }

      logProb += tProb;
      this;
    }

    def -=(c: ExpectedCounts[W]) = {
      val ExpectedCounts(bCounts,uCounts,wCounts,tProb) = c;

      for( (k1,c) <- bCounts;
          (k2,vec) <- c) {
        binaryRuleCounts.getOrElseUpdate(k1).getOrElseUpdate(k2) -= vec;
      }

      for( (k,vec) <- uCounts) {
        unaryRuleCounts.getOrElseUpdate(k) -= vec;
      }

      for( (k,vec) <- wCounts) {
        wordCounts.getOrElseUpdate(k) -= vec;
      }

      logProb -= tProb;
      this;
    }

  }

    def decodeRules[L](g: Grammar[L],
                     binaryRuleCounts: SparseArray[SparseArray[Vector]],
                     unaryRuleCounts: SparseArray[Vector]) = {
    val ctr = PairedDoubleCounter[L,Rule[L]]();

    for( (pIndex,arr1) <- binaryRuleCounts.iterator;
        p = g.index.get(pIndex);
        (lIndex,arr2) <- arr1.iterator;
        l = g.index.get(lIndex);
        (rIndex,v) <- arr2.activeElements;
        r = g.index.get(rIndex)
    ) {
      ctr(p,BinaryRule(p,l,r)) = v;
    }

    for( (pIndex,arr1) <- unaryRuleCounts.iterator;
        p = g.index.get(pIndex);
        (cIndex,v) <- arr1.activeElements;
        c = g.index.get(cIndex)
    ) {
      ctr(p,UnaryRule(p,c)) = v;
    }

    ctr;
  }

  def decodeWords[L,W](g: Grammar[L], wordCounts: SparseArray[DoubleCounter[W]]) = {
    val ctr = PairedDoubleCounter[L,W]();
    for( (i,c) <- wordCounts) {
      ctr(g.index.get(i)) := c;
    }
    ctr;
  }



}