package scalanlp.parser

import projections.ProjectionIndexer
import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.graphs.{DotRenderer, Distance}

import scalala.tensor.counters.Counters;
import scalanlp.util.Implicits._
import scalanlp.collection.mutable.TriangularArray;
import scalanlp.math.Numerics.logSum;

/**
 * A ChartDecoder can turn an inside chart (and optionally an outside chart) from some
 * parse chart over symbols F into a tree over symbols C
 *
 * @author dlwh
 */
@serializable
trait ChartDecoder[C,F] {
  def extractBestParse(root:F, grammar: Grammar[F],
                       inside: ParseChart[F],
                       outside: =>ParseChart[F],
                       spanScorer: SpanScorer = SpanScorer.identity):BinarizedTree[C];

}

/**
 * Tries to extract a tree that maximizes log score.
 */
@serializable
@SerialVersionUID(1)
class ViterbiDecoder[C,F](val indexedProjections: ProjectionIndexer[C,F]) extends ChartDecoder[C,F] {

  override def extractBestParse(root: F, grammar: Grammar[F],
                                inside: ParseChart[F],
                                outside: =>ParseChart[F],
                                spanScorer: SpanScorer = SpanScorer.identity):BinarizedTree[C] = {

    def buildTreeUnary(start: Int, end:Int, root: Int):BinarizedTree[C] = {
      var maxScore = Double.NegativeInfinity;
      var maxChild = -1;
      for {
        (b,ruleScore) <- grammar.unaryRulesByIndexedParent(root)
      } {
        val score = ruleScore + inside.bot(start,end,b) + spanScorer.scoreUnaryRule(start,end,root,b);
        if(score > maxScore) {
          maxScore = score;
          maxChild = b;
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(start,end).map { case (i,v) => (grammar.index.get(i),v)}.toList)
        error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
      }
      val child = buildTree(start,end,maxChild);
      UnaryTree(indexedProjections.coarseSymbol(root),child)(Span(start,end));
    }

    def buildTree(start: Int, end: Int, root: Int):BinarizedTree[C] = {
      var maxScore = Double.NegativeInfinity;
      var maxLeft = -1;
      var maxRight = -1;
      var maxSplit = -1;
      if(start +1 == end) {
        return NullaryTree(indexedProjections.coarseSymbol(root))(Span(start,end));
      }

      for {
        (b,rchild) <- grammar.allBinaryRules;
        (c,parentScores) <- rchild
        split <- inside.top.feasibleSpan(start, end, b, c)
      } {
        val ruleScore = parentScores(root);
        val score = ruleScore + inside.top.labelScore(start,split,b) +
                inside.top.labelScore(split,end,c) + spanScorer.scoreBinaryRule(start,split,end,root,b,c);
        if(score > maxScore) {
          maxScore = score;
          maxLeft = b;
          maxRight = c;
          maxSplit = split;
        }
      }

      if(maxScore == Double.NegativeInfinity) {
        println("entered things: " + inside.bot.enteredLabelScores(start,end).map { case (i,v) => (grammar.index.get(i),v)}.toList)
        error("Couldn't find a tree!" + start + " " + end + " " + grammar.index.get(root));
      } else {
        val lchild = buildTreeUnary(start,maxSplit,maxLeft);
        val rchild = buildTreeUnary(maxSplit,end,maxRight);
        BinaryTree(indexedProjections.coarseSymbol(root),lchild,rchild)(Span(start,end));
      }


    }

    buildTreeUnary(0,inside.length, grammar.index(root));
  }
}

@serializable
@SerialVersionUID(1)
class SimpleViterbiDecoder[L](grammar: Grammar[L]) extends
    ViterbiDecoder[L,L](new ProjectionIndexer(grammar.index,grammar.index,identity[L]));


object SimpleViterbiDecoder {
  def apply[L](g: Grammar[L]):SimpleViterbiDecoder[L] = new SimpleViterbiDecoder[L](g);
}

/**
 * Tries to extract a tree that maximizes rule sum in the coarse grammar
trait MaxRuleSumDecoder[C,F] extends ChartDecoder[C,F] {

  protected def indexedProjections: ProjectionIndexer[C,F]
  protected def coarseGrammar: Grammar[C] // just for what rules are possible.

  private case class MaxRuleData(maxScore: Array[Array[Double]], // begin,end -> parent -> score for parent
                                 maxLeftChild: Array[Array[Int]],// begin,end -> parent -> left child
                                 maxRightChild: Array[Array[Int]],
                                 maxUnaryChild: Array[Array[Int]],
                                 maxSplit: Array[Array[Int]]); // split point, or -1 if a unary, or -2 if preterminal

  override def extractBestParse(root: F, grammar: Grammar[F],
                                inside: ParseChart[F], outside: =>ParseChart[F],
                                spanScorer: SpanScorer = SpanScorer.identity):BinarizedTree[C] = {
    val data = buildMaxCCharts(root, grammar, inside, outside, spanScorer);
    import data._;
    def buildTree(begin: Int, end: Int, root: Int, allowUnary:Boolean =true):BinarizedTree[L] = {
      val lroot = grammar.index.get(root);
      val arrayIndex = TriangularArray.index(begin,end);
      if(allowUnary && maxUnaryChild(arrayIndex)(root) > -1) {
        val child = buildTree(begin,end,maxUnaryChild(arrayIndex)(root), false);
        UnaryTree(lroot,child)(Span(begin,end));
      } else if(maxSplit(arrayIndex)(root) >= 0) {
        val split = maxSplit(arrayIndex)(root);
        val leftChild = buildTree(begin,split,maxLeftChild(arrayIndex)(root));
        val rightChild = buildTree(split,end,maxRightChild(arrayIndex)(root));
        BinaryTree(lroot,leftChild,rightChild)(Span(begin,end));
      } else if(maxSplit(arrayIndex)(root) == -2) { // lexical
        NullaryTree(lroot)(Span(begin,end));
      } else {
        error("Couldn't construct over " + lroot + " for span " + begin + " " + end);
      }
    }

    buildTree(0,inside.length, inside.grammar.index(root));
  }

  private def buildMaxCCharts(root: F, grammar: Grammar[F], inside: ParseChart[F], outside: ParseChart[F], spanScorer:SpanScorer) = {
    val partition = inside.labelScore(0,inside.length,root);
    val coarseEncoder = indexedProjections.coarseEncoder;

    val maxScore = TriangularArray.raw(inside.length,coarseEncoder.fillArray(Double.NegativeInfinity));
    val maxLeftChild = TriangularArray.raw(inside.length,coarseEncoder.fillArray(-1));
    val maxRightChild = TriangularArray.raw(inside.length,coarseEncoder.fillArray(-1));
    val maxUnaryChild = TriangularArray.raw(inside.length,coarseEncoder.fillArray(-1));
    // if maxSplit == -2, then it's a preterminal
    val maxSplit = TriangularArray.raw(inside.length,coarseEncoder.fillArray(-1));


    for(diff <- 1 to inside.length) {
      if(diff == 1) {
        for(begin <- 0 until inside.length) {
          val end = begin + 1;
          val arrayIndex = TriangularArray.index(begin,end);
          for( (a,aInside) <- inside.enteredLabelScores(begin,end) if grammar.isPreterminal(a)) {
            val aOutside = outside.labelScore(begin,end,a);
            val score = aOutside + aInside - partition + spanScorer.scoreLexical(begin,end,a);
            if(score != Double.NegativeInfinity) {
              val proj = indexedProjections.project(a);
              maxSplit(arrayIndex)(proj) = -2;
              maxScore(arrayIndex)(proj) = logSum(maxScore(arrayIndex)(proj),score);
            }
          }
        }
      }

      // a -> b -> c -> split -> score
      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val arrayIndex = TriangularArray.index(begin,end);
        // find binary rule at each point that is best for each label
        for{ split <- (begin+1) until end
             (pB,coarseRules) <- coarseGrammar.allBinaryRules
             if maxScore(TriangularArray.index(begin,split))(pB) != Double.NegativeInfinity
             (pC,coarseParents) <- coarseRules
             if maxScore(TriangularArray.index(split,end))(pC) != Double.NegativeInfinity
             pA <- coarseParents.activeKeys
        } {
          var score = Double.NegativeInfinity;
          for {
            a <- indexedProjections.refinementsOf(pA);
            outsideScore = outside.labelScore(begin, end, a) if outsideScore != Double.NegativeInfinity
            b <- indexedProjections.refinementsOf(pB)
            c <- indexedProjections.refinementsOf(pC)
            ruleScore = grammar.binaryRuleScore(a,b,c) if ruleScore != Double.NegativeInfinity
          } {
            val ruleCount = (outsideScore + ruleScore + spanScorer.scoreBinaryRule(begin,split,end,a,b,c)
                    + inside.labelScore(begin,split,b) + inside.labelScore(split,end,c) - partition);
            score = logSum(score,ruleCount);

          }

          val bScore = maxScore(TriangularArray.index(begin, split))(pB)
          val cScore = maxScore(TriangularArray.index(split, end))(pC)
          if (bScore != Double.NegativeInfinity && cScore != Double.NegativeInfinity) {
            val totalScore = logSum(score, bScore, cScore);
            if(totalScore > maxScore(arrayIndex)(pA)) {
              maxScore(arrayIndex)(pA) = totalScore;
              maxLeftChild(arrayIndex)(pA) = pB;
              maxRightChild(arrayIndex)(pA) = pB;
              maxSplit(arrayIndex)(pA) = split;
            }
          }
        }

        // TODO: allow more than 1 unary
        for{
          (pB,coarseRules) <- coarseGrammar.allUnaryRules
          if maxScore(TriangularArray.index(begin,end))(pB) != Double.NegativeInfinity
          pA <- coarseRules.activeKeys
        } {
         var score = Double.NegativeInfinity;
         for {
           a <- indexedProjections.refinementsOf(pA);
           outsideScore = outside.labelScore(begin, end, a) if outsideScore != Double.NegativeInfinity
           b <- indexedProjections.refinementsOf(pB)
           ruleScore = grammar.unaryRuleScore(a,b) if ruleScore != Double.NegativeInfinity
         } {

         }

           if(maxSplit(arrayIndex)(c) == -2) { // lexical tag
             val cInside = inside.labelScore(begin,end,c);
             val ruleCount = outsideScore + uScore + cInside - partition;
             val score = ruleCount;
             if(score > maxScore(arrayIndex)(a)) {
               maxScore(arrayIndex)(a) = score;
               maxUnaryChild(arrayIndex)(a) = c;
             }
           }



         }

      }
    }

    MaxRuleData(maxScore,maxLeftChild,maxRightChild,maxUnaryChild,maxSplit);
  }
}

object MaxRuleTrainer extends ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {
    import scalanlp.math.Semiring.Viterbi.doubleIsViterbi;
    val base = GenerativeParser.fromTrees(trainTrees).builder.withCharts(ParseChart.logProb);
    import base.grammar;

    val parser = new Parser[String,String] with MaxRuleSumDecoder[String] {
      def scores(o: Seq[String]) = try {
        val inside = base.buildInsideChart(o);
        lazy val outside = base.buildOutsideChart(inside);
        val tree = extractBestParse(base.root, grammar, inside, outside);
        Counters.aggregate(tree -> 1.0);
      } catch {
        case e => throw new RuntimeException("Couldn't parse " + o, e);
      }
    }
    Iterator.single(("MaxRule",parser));
  }
}
 */

