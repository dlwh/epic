package scalanlp.parser

import scalanlp.collection.mutable.SparseArrayMap
import ParseChart._;

import InsideOutside._;

import math.exp
import scalala.tensor.::
import scalala.tensor.mutable.{Counter, Counter2, Vector}

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

  def expectedCounts(words: Seq[W], validSpan: SpanScorer[L] =SpanScorer.identity):ExpectedCounts[W] = {
    val inside = parser.buildInsideChart(words, validSpan);
    val totalProb = inside.top.labelScore(0, words.length, root);
    val outside = parser.buildOutsideChart(inside, validSpan);

    expectedCounts(words,inside,outside, totalProb, validSpan)
  }

  def expectedCounts(words: Seq[W],
                     inside: LogProbabilityParseChart[L],
                     outside: LogProbabilityParseChart[L],
                     totalProb: Double, validSpan: SpanScorer[L]) = {
    val wordCounts = computeWordCounts(words, inside, outside, validSpan, totalProb)
    val binaryRuleCounts = computeBinaryCounts(words, inside, outside, validSpan, totalProb)
    val unaryRuleCounts = computeUnaryCounts(words, inside, outside, validSpan, totalProb)

    ExpectedCounts(binaryRuleCounts, unaryRuleCounts, wordCounts, totalProb);
  }

  private def computeWordCounts(words: scala.Seq[W],
                                inside: LogProbabilityParseChart[L],
                                outside: LogProbabilityParseChart[L],
                                validSpan: SpanScorer[L],
                                totalProb: Double): SparseArrayMap[Counter[W, Double]] = {
    val wordCounts = grammar.fillSparseArrayMap(Counter[W, Double]());
    // handle lexical productions:
    for (i <- 0 until words.length) {
      val w = words(i);
      for (l <- inside.bot.enteredLabelIndexes(i, i + 1) if isTag(l)) {
        val iScore = inside.bot.labelScore(i, i + 1, l);
        val oScore = outside.bot.labelScore(i, i + 1, l);
        wordCounts.getOrElseUpdate(l)(w) += exp(iScore + oScore - totalProb);
      }
    }
    wordCounts
  }

  private def computeBinaryCounts(words: scala.Seq[W],
                                  inside: LogProbabilityParseChart[L],
                                  outside: LogProbabilityParseChart[L],
                                  validSpan: SpanScorer[L], totalProb: Double) = {
    val binaryRuleCounts = grammar.fillSparseArrayMap(grammar.fillSparseArrayMap(grammar.mkVector()));
    // handle binary rules
    for{
      span <- 2 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      (b, binaryRules) <- grammar.allBinaryRules;
      //if inside.canStartHere(begin, end, b);
      (c, parentVector) <- binaryRules;
      split <- inside.top.feasibleSpan(begin, end, b, c)
    } {
      val bScore = inside.top.labelScore(begin, split, b);
      if (!bScore.isInfinite) {
        val cScore = inside.top.labelScore(split, end, c)
        if (!cScore.isInfinite) {
          var i = 0;
          while (i < parentVector.activeSize) {
            val a = parentVector.indexAt(i);
            val rScore = parentVector.valueAt(i) + validSpan.scoreBinaryRule(begin,split, end,a,b,c);
            val aScore = outside.bot.labelScore(begin, end, a);
            i += 1;
            if (!aScore.isInfinite) {
              val prob = exp(bScore + cScore + aScore + rScore - totalProb);
              if(prob.isInfinite || prob.isNaN)
                error(prob + " in " + words + " a: " + aScore + " b" + bScore  + " c" + cScore  + " r" + rScore  + " sp" + validSpan.scoreBinaryRule(begin,split,end,a,b,c) + " t" + totalProb);
              binaryRuleCounts.getOrElseUpdate(a).getOrElseUpdate(b)(c) += prob;
              assert(binaryRuleCounts(a)(b)(c) >= prob,binaryRuleCounts(a)(b)(c) + " " + prob);
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
                                 validSpan: SpanScorer[L],
                                 totalProb: Double): SparseArrayMap[Vector[Double]] = {
    val unaryRuleCounts = grammar.fillSparseArrayMap(grammar.mkVector());
    for{
      span <- 1 to words.length;
      begin <- 0 to (words.length - span);
      end = begin + span
      b <- inside.bot.enteredLabelIndexes(begin, end)
    } {
      val bScore = inside.bot.labelScore(begin, end, b);
      if (!bScore.isInfinite) {
        val parentVector = grammar.unaryRulesByIndexedChild(b);
        var i = 0;
        while (i < parentVector.activeSize) {
          val a = parentVector.indexAt(i);
          val aScore = outside.top.labelScore(begin, end, a);
          if(!aScore.isInfinite) {
            val rScore = parentVector.valueAt(i) + validSpan.scoreUnaryRule(begin,end,a,b);
            val prob = exp(bScore + aScore + rScore - totalProb);
            if(prob.isInfinite || prob.isNaN)
              error("nan in " + words + " for unary " + inside.grammar.index.get(a) + "->" + inside.grammar.index.get(b) + ": " + prob + " a: " + aScore + " " + "b: " + bScore + " sp: " + validSpan.scoreUnaryRule(begin,end,a,b)  + "tot: " + totalProb);
            unaryRuleCounts.getOrElseUpdate(a)(b) += prob;
          }
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
    binaryRuleCounts: SparseArrayMap[SparseArrayMap[Vector[Double]]], // parent -> lchild -> rchild -> counts;
    unaryRuleCounts: SparseArrayMap[Vector[Double]], // parent -> child -> counts;
    wordCounts: SparseArrayMap[Counter[W,Double]], // parent -> word -> counts;
    var logProb: Double
  ) {

    def this(g: Grammar[_]) = this(g.fillSparseArrayMap(g.fillSparseArrayMap(g.mkVector())),
                                   g.fillSparseArrayMap(g.mkVector()),
                                   g.fillSparseArrayMap(Counter[W,Double]()), 0.0);

    def decode[L](g: Grammar[L]) = (decodeRules(g,binaryRuleCounts), decodeUnaries(g,unaryRuleCounts),decodeWords(g,wordCounts));

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
                     binaryRuleCounts: SparseArrayMap[SparseArrayMap[Vector[Double]]]) = {
    val ctr = Counter2[L,BinaryRule[L],Double]();

    for( (pIndex,arr1) <- binaryRuleCounts.iterator;
         p = g.index.get(pIndex);
         (lIndex,arr2) <- arr1.iterator;
         l = g.index.get(lIndex);
         (rIndex,v) <- arr2.nonzero.pairs.iterator;
         r = g.index.get(rIndex)
    ) {
      ctr(p,BinaryRule(p,l,r)) = v;
    }

    ctr
  }

  def decodeUnaries[L](g: Grammar[L],
            unaryRuleCounts: SparseArrayMap[Vector[Double]]) = {
    val ctr = Counter2[L,UnaryRule[L],Double]();
    for( (pIndex,arr1) <- unaryRuleCounts.iterator;
        p = g.index.get(pIndex);
        (cIndex,v) <- arr1.nonzero.pairs.iterator;
        c = g.index.get(cIndex)
    ) {
      ctr(p,UnaryRule(p,c)) = v;
    }

    ctr;
  }

  def decodeWords[L,W](g: Grammar[L], wordCounts: SparseArrayMap[Counter[W,Double]]) = {
    val ctr = Counter2[L,W,Double]();
    for( (i,c) <- wordCounts) {
      ctr(g.index.get(i), ::) := c;
    }
    ctr;
  }



}