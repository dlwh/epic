package scalanlp.parser

import scalanlp.trees._
import scalanlp.config.Configuration
import scalanlp.graphs.{DotRenderer, Distance}

import scalala.tensor.counters.Counters;
import scalanlp.util.Implicits._
import scalanlp.collection.mutable.TriangularArray;
import scalanlp.math.Numerics.logSum;

/**
 *  A ParseDecoder can turn an inside chart (and optionally an outside chart) into a tree
 *
 * @author dlwh
 */
trait ParseDecoder[L] {
  def extractBestParse(root:L, inside: ParseChart[L], outside: =>ParseChart[L]):BinarizedTree[L];
}

/**
 * Tries to extract a tree that maximizes log score.
 */
trait ViterbiDecoder[L] extends ParseDecoder[L] {
  override def extractBestParse(root: L, inside: ParseChart[L], outside: =>ParseChart[L]):BinarizedTree[L] = {
    import inside.{grammar,labelScore};

    def buildTree(start: Int, end: Int, root: Int):BinarizedTree[L] = {
      val scoreToFind = labelScore(start,end,root);
      var closest = Double.NaN;
      for {
        (b,rchild) <- grammar.allBinaryRules;
        (c,parentScores) <- rchild
        split <- inside.feasibleSpan(start, end, b, c)
      } {
        val ruleScore = parentScores(root);
        val score = ruleScore + labelScore(start,split,b) + labelScore(split,end,c);
        if(score closeTo scoreToFind) {
          val left = buildTree(start,split,b);
          val right = buildTree(split,end,c);
          return BinaryTree(grammar.index.get(root),left,right)(Span(start,end));
        } else if(closest.isNaN || (score - scoreToFind).abs < (scoreToFind - closest).abs) {
          closest = score;
        }
      }
      for {
        (b,ruleScore) <- grammar.unaryRulesByIndexedParent(root)
      } {
        val score = ruleScore + labelScore(start,end,b);
        if(score closeTo scoreToFind) {
          val child = buildTree(start,end,b);
          return UnaryTree(grammar.index.get(root),child)(Span(start,end));
        } else if(closest.isNaN || (score - scoreToFind).abs < (scoreToFind - closest).abs) {
          closest = score;
        }
      }
      if(start +1 == end) // lexical
        NullaryTree(grammar.index.get(root))(Span(start,end));
      else error("Couldn't find a tree!" + closest + " " + start + " " + end + " " + grammar.index.get(root));
    }

    buildTree(0,inside.length, grammar.index(root));
  }
}

/**
 * Tries to extract a tree that maximizes labeled recall
 */
trait MaxRuleSumDecoder[L] extends ParseDecoder[L] {

  protected def unaryClosure: UnaryRuleClosure;

  private case class MaxRuleData(maxScore: Array[Array[Double]], // begin -> end -> parent -> score for parent
                                 maxLeftChild: Array[Array[Int]],// begin -> end -> parent -> left child
                                 maxRightChild: Array[Array[Int]],
                                 maxUnaryChild: Array[Array[Int]],
                                 maxSplit: Array[Array[Int]]); // split point, or -1 if a unary, or -2 if preterminal

  override def extractBestParse(root: L, inside: ParseChart[L], xoutside: =>ParseChart[L]):BinarizedTree[L] = {
    import inside.{grammar,labelScore};
    val data = buildMaxCCharts(root, inside, xoutside);
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

  private def buildMaxCCharts(root: L, inside: ParseChart[L], outside: ParseChart[L]) = {
    val partition = inside.labelScore(0,inside.length,root);
    import inside.grammar;


    val maxScore = TriangularArray.raw(inside.length,grammar.fillArray(Double.NegativeInfinity));
    val maxLeftChild = TriangularArray.raw(inside.length,grammar.fillArray(-1));
    val maxRightChild = TriangularArray.raw(inside.length,grammar.fillArray(-1));
    val maxUnaryChild = TriangularArray.raw(inside.length,grammar.fillArray(-1));
    // if maxSplit == -2, then it's a preterminal
    val maxSplit = TriangularArray.raw(inside.length,grammar.fillArray(-1));

    for(diff <- 1 to inside.length) {
      if(diff == 1) {
        for(begin <- 0 until inside.length) {
          val end = begin + 1;
          val arrayIndex = TriangularArray.index(begin,end);
          for( (a,aInside) <- inside.enteredLabelScores(begin,end) if inside.grammar.isPreterminal(a)) {
            val aOutside = outside.labelScore(begin,end,a);
            val score = aOutside + aInside - partition;
            if(score != Double.NegativeInfinity) {
              maxSplit(arrayIndex)(a) = -2;
              maxScore(arrayIndex)(a) = score;
            }
          }
        }
      }

      for(begin <- 0 until (inside.length - diff + 1)) {
        val end = begin + diff;
        val arrayIndex = TriangularArray.index(begin,end);
        // find binary rule at each point that is best for each label
        for( (a,outsideScore) <- outside.enteredLabelScores(begin, end);
             (b,cRules) <- grammar.binaryRulesByIndexedParent(a);
             (c,ruleScore) <- cRules.activeElements;
             split <- inside.feasibleSpan(begin,end, b, c)) {
          val ruleCount = (outsideScore + ruleScore
                  + inside.labelScore(begin,split,b) + inside.labelScore(split,end,c) - partition);
          val bScore = maxScore(TriangularArray.index(begin, split))(b)
          val cScore = maxScore(TriangularArray.index(split, end))(c)
          if (bScore != Double.NegativeInfinity && cScore != Double.NegativeInfinity) {
            val score = logSum(ruleCount, bScore, cScore);
            if(score > maxScore(arrayIndex)(a)) {
              maxScore(arrayIndex)(a) = score;
              maxLeftChild(arrayIndex)(a) = b;
              maxRightChild(arrayIndex)(a) = c;
              maxSplit(arrayIndex)(a) = split;
            }
          }

        }

        // TODO: allow more than 1 unary
        for( (a,outsideScore) <- outside.enteredLabelScores(begin, end);
             (c,uScore) <- unaryClosure.closeFromParent(a).activeElements if a != c
             if maxScore(arrayIndex)(c) != Double.NegativeInfinity) {
          // idea: a ->* c -> [cb, cc] , with some split point.
          // recover the split point and cb, cc from above
          // since these guys maximize the scores we want.
          if(maxLeftChild(arrayIndex)(c) != -1) {
            val cb = maxLeftChild(arrayIndex)(c);
            val cc = maxRightChild(arrayIndex)(c);
            val split = maxSplit(arrayIndex)(c);

            val childBinaryRuleScore = grammar.binaryRuleScore(c,cb,cc);
            val cbInside = inside.labelScore(begin,split,cb);
            val ccInside = inside.labelScore(split,end,cc);

            // outside(a) * score(a->c) * score(c -> cb cc) * inside(cb) * inside(cc) / partition
            val ruleCount = outsideScore + uScore + childBinaryRuleScore + cbInside + ccInside - partition;
            val cbScore = maxScore(TriangularArray.index(begin, split))(cb);
            val ccScore = maxScore(TriangularArray.index(split, end))(cc);
            if (cbScore != Double.NegativeInfinity && ccScore != Double.NegativeInfinity) {
              val score = logSum(ruleCount,cbScore,ccScore);
              if(score > maxScore(arrayIndex)(a)) {
                maxScore(arrayIndex)(a) = score;
                maxUnaryChild(arrayIndex)(a) = c;
              }
            }
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
    val base = GenerativeParser.fromTrees(trainTrees).withCharts(ParseChart.logProb);
    import base.grammar;

    val parser = new Parser[String,String] with MaxRuleSumDecoder[String] {
      val unaryClosure = UnaryRuleClosure(base.grammar);
      def scores(o: Seq[String]) = try {
        val inside = base.buildInsideChart(o);
        lazy val outside = base.buildOutsideChart(inside);
        val tree = extractBestParse(base.root, inside, outside);
        Counters.aggregate(tree -> 1.0);
      } catch {
        case e => throw new RuntimeException("Couldn't parse " + o, e);
      }
    }
    Iterator.single(("MaxRule",parser));
  }
}