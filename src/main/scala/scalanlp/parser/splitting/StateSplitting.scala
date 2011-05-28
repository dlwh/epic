package scalanlp.parser.splitting
/*
 Copyright 2010 David Hall

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/


import scalanlp.config.Configuration
import scalala.tensor.counters.Counters._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.parser.ParserParams.NoParams;
import scalala.tensor.counters.LogCounters;
import scalanlp.math.Numerics.logSum;
import scalanlp.parser._;
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.UnaryTree
import scalanlp.parser.projections._;
import scalanlp.concurrent.ParallelOps._;

import math.exp

import InsideOutside._;

object StateSplitting {

  def insideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity) = {
    val chart = ParseChart.logProb(grammar, s.length);
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };

    indexedTree.postorder.foreach {
      case t : NullaryTree[Seq[Int]] =>
        // fill in POS tags:
        assert(t.span.length == 1);
        val word = s(t.span.start);
        var foundOne = false;
        for( a <- t.label.iterator;
            wScore = lexicon.wordScore(grammar.index.get(a),word) + scorer.scoreLexical(t.span.start,t.span.end,a);
            if !wScore.isInfinite) {
          chart.bot.enter(t.span.start,t.span.end,a,wScore);
          foundOne = true;
        }
        if(!foundOne) {
          val msg = t.label.map { a =>
            lexicon.tagScores(word) + " " +
            grammar.index.get(a) + " " + lexicon.wordScore(grammar.index.get(a),word) + " " + scorer.scoreLexical(t.span.start,t.span.start+1,a);
          }
          error("Trouble with lexical " + t.render(s) + " " + msg.mkString(", "))
        }
      case t : UnaryTree[Seq[Int]] =>
        var foundOne = false;
        for ( a <- t.label) {
          val rules = grammar.unaryRulesByIndexedParent(a);
          for(c <- t.child.label) {
            val ruleScore = (rules(c)
              + chart.bot(t.span.start,t.span.end,c)
              + scorer.scoreUnaryRule(t.span.start, t.span.end, a, c));
            if(!ruleScore.isInfinite) {
              foundOne = true;
            }
            chart.top.enter(t.span.start, t.span.end, a,ruleScore);
          }
        }

        if(!foundOne) {
          val msg = t.label.map { a => t.child.label.map { c =>
            grammar.index.get(a) + "->" + grammar.index.get(c) + " " +
             grammar.unaryRulesByIndexedParent(a)(c) + " " +  chart.bot(t.span.start,t.span.end,c) + " " + scorer.scoreUnaryRule(t.span.start, t.span.end, a, c);
          }}
          error("Trouble with unary " + t.render(s) + " " + msg.mkString(", "))
        }
      case t@BinaryTree(_,lchild,rchild) =>
        var foundOne = false;
        val begin = t.span.start;
        val split = t.leftChild.span.end;
        val end = t.span.end;
        for {
          a <- t.label
        } {
          val arr = new Array[Double](lchild.label.length * rchild.label.length);
          var i = 0;
          for { b <- lchild.label } {
            val rules = grammar.binaryRulesByIndexedLeftChild(b);
            for { c <- rchild.label } {
              arr(i) = rules(c)(a) + chart.top(begin,split,b) + chart.top(split,end,c) + scorer.scoreBinaryRule(begin,split,end,a,b,c);
              i += 1;
            }
          };
          chart.bot.enter(begin,end,a,logSum(arr));
          if(!chart.bot(begin,end,a).isInfinite) foundOne = true;
       }

        if(!foundOne) {
          val msg = t.label.map { a => t.leftChild.label.map { b => t.rightChild.label.map { c =>
            grammar.index.get(a) + "->" + grammar.index.get(b) + " " + grammar.index.get(c) + " br: " +
                 grammar.binaryRulesByIndexedLeftChild(b)(c)(a) +  " lc:"+ chart.top(begin,split,b) +  " rc: " + chart.top(split,end,c) +
                " scorer: " + scorer.scoreBinaryRule(begin,split,end,a,b,c);
          }}}
          error("Trouble with binary " + t.render(s) + " " + msg.mkString(", "))
        }
      case _ => error("bad tree!");
    }

    chart;
  }

  def outsideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], insideScores: ParseChart.LogProbabilityParseChart[L], scorer: SpanScorer[L]= SpanScorer.identity) = {
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };
    val chart = ParseChart.logProb(grammar, s.length);
    // Root gets score 0
    for(l <- indexedTree.label) chart.top.enter(0,s.length,l,0.0);

    // Set the outside score of each child
    indexedTree.preorder.foreach {
      case t @ BinaryTree(_,lchild,rchild) =>
        for {
          p <- t.label.iterator
          pScore = chart.bot(t.span.start,t.span.end,p);
          l <- lchild.label.iterator
          lScore = insideScores.top(lchild.span.start,lchild.span.end,l);
          lRules = grammar.binaryRulesByIndexedLeftChild(l);
          r <- rchild.label.iterator
          rScore = insideScores.top(rchild.span.start,rchild.span.end,r)
        } {
          val rS = lRules(r)(p) + scorer.scoreBinaryRule(t.span.start,lchild.span.end,t.span.end,p,l,r);
          chart.top.enter(lchild.span.start,lchild.span.end,l, pScore + rScore + rS);
          chart.top.enter(rchild.span.start,rchild.span.end,r, pScore + lScore + rS);
        }
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_,child) =>
        for {
          p <- t.label
          pScore = chart.top(child.span.start,child.span.end,p);
          c <- child.label
        } {
          val rScore = grammar.unaryRulesByIndexedChild(c)(p)+ scorer.scoreUnaryRule(t.span.start,t.span.end,p,c);
          chart.bot.enter(child.span.start,child.span.end,c,pScore + rScore);
        }

    }

    chart
  }


  def expectedCounts[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity[L]) = {
    val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(0.0)));
    val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(0.0));
    val wordCounts = grammar.fillSparseArray(DoubleCounter[W]());

    val safeScorer = scorer;
    /*new SpanScorer {
      @inline private def I(score: Double) = if(score > Double.NegativeInfinity) score else -100.0;

      def scoreLexical(begin: Int, end: Int, tag: Int) = I(scorer.scoreLexical(begin,end,tag))

      def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = I(scorer.scoreUnaryRule(begin,end,parent,child));

      def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
        I(scorer.scoreBinaryRule(begin, split, end, parent, leftChild, rightChild))
      }
    } */

    val iScores = insideScores(grammar,lexicon,tree,s, safeScorer);
    val oScores = outsideScores(grammar,lexicon, tree,s,iScores, safeScorer);
    val indexedTree:Tree[Seq[Int]] = tree.map { _.map(grammar.index) };

    // normalizer
    val totalProb = logSum(tree.label.map(grammar.index).map(iScores.top(0,s.length,_)));
    if(totalProb.isInfinite || totalProb.isNaN)
      error("NAn or infinite" + totalProb + " " + tree.render(s));

    indexedTree.allChildren foreach {
      case t: NullaryTree[Seq[Int]] =>
        for( l <- t.label) {
          val iS = iScores.bot(t.span.start,t.span.end,l);
          val oS = oScores.bot(t.span.start,t.span.end,l)
          val ruleScore = (iS + oS - totalProb);
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore).isInfinite);
          wordCounts.getOrElseUpdate(l)(s(t.span.start)) +=  exp(ruleScore);
        }
      case t@UnaryTree(_,child) =>
        for {
          p <- t.label.iterator;
          opScore = oScores.top(t.span.start,t.span.end,p);
          c <- child.label.iterator
          icScore = iScores.bot(child.span.start,child.span.end,c)
        } {
          val ruleScore = opScore + icScore + grammar.unaryRulesByIndexedChild(c)(p) - totalProb;
          val span = safeScorer.scoreUnaryRule(t.span.start,t.span.end,p,c);
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore + span).isInfinite);
          unaryRuleCounts.getOrElseUpdate(p)(c) += exp(ruleScore + span);
        }
      case t@ BinaryTree(_,lc,rc) =>
        for {
          p <- t.label iterator;
          opScore = oScores.bot(t.span.start,t.span.end,p);
          l <- lc.label.iterator
          val lRules = grammar.binaryRulesByIndexedLeftChild(l);
          ilScore = iScores.top(lc.span.start,lc.span.end,l);
          r <- rc.label.iterator
        } {
          val irScore = iScores.top(rc.span.start,rc.span.end,r)
          val ruleScore = opScore + irScore + ilScore + lRules(r)(p) - totalProb;
          val span = safeScorer.scoreBinaryRule(t.span.start,rc.span.start,rc.span.end,p,l,r);
          assert(!ruleScore.isNaN);
          //assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore + span).isInfinite, ruleScore + " " + span + " " + (ruleScore + span) + " " + totalProb);
          binaryRuleCounts.getOrElseUpdate(p).getOrElseUpdate(l)(r) += exp(ruleScore + span);
          assert(binaryRuleCounts(p)(l)(r) >=exp(ruleScore + span));
        }
    }

    //println("Problems: " + numProblems * 1.0 / numTotal);
    ExpectedCounts(binaryRuleCounts,unaryRuleCounts,wordCounts,totalProb);
  }

  def splitCounts[L,L2,W](data: SplittingData[L,W],
                          splitter: L=>Seq[L2],
                          randomNoise: Double = 0.1):SplittingData[L2,W] = {

    val SplittingData(binaries,unaries,wordCounts) = data;

    val splitRules = PairedDoubleCounter[L2,BinaryRule[L2]]();
    for {
      ((_,baseR),count) <- binaries;
      r <- splitRule(baseR,splitter)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitRules(r.parent,r) = count * x;
    }

    val splitUnaries = PairedDoubleCounter[L2,UnaryRule[L2]]();
    for {
      ((_,baseR),count) <- unaries;
      r <- splitRule(baseR,splitter)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitUnaries(r.parent,r) = count * x;
    }

    val splitWords = PairedDoubleCounter[L2,W]();
    for {
      ((l,w),count) <- wordCounts
      r <- splitter(l)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitWords(r,w) = count * x;
    }

    SplittingData(splitRules,splitUnaries,splitWords);
  }

  private def splitRule[L,L2,W](r: UnaryRule[L], splitter: L=>Seq[L2]):Iterator[UnaryRule[L2]] = r match {
    case UnaryRule(p,c) => for { pp <- splitter(p).iterator ; cc <- splitter(c).iterator} yield UnaryRule(pp,cc);
  }

  private def splitRule[L,L2,W](r: BinaryRule[L], splitter: L=>Seq[L2]):Iterator[BinaryRule[L2]] = r match {
    case BinaryRule(p,l,r) =>
      for { pp <- splitter(p).iterator ; ll <- splitter(l).iterator; rr <- splitter(r).iterator } yield BinaryRule(pp,ll,rr);
  }

  def splitCycle[L,W](data: SplittingData[L,W],
                      trees: SplitTreebank[L,W],
                      splitter: L=>Seq[L],
                      randomNoise: Double =0.1,convergence:Double=1E-4):
                      (SplittingData[L,W],Double) = {
    val splitData = splitCounts(data,splitter,randomNoise);

    val stateIterator = Iterator.iterate( (splitData,Double.MaxValue,convergence.abs*100)) { case state =>
      val (SplittingData(splitRules,splitUnRules, splitWords),lastLogProb,_) = state;
      assert(splitRules.total != 0)
      assert(splitUnRules.total != 0)
      assert(splitWords.total != 0)
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(splitRules), LogCounters.logNormalizeRows(splitUnRules));
      val lexicon = new UnsmoothedLexicon(LogCounters.logNormalizeRows(splitWords));
      val accum = trees.par.mapReduce ( {case TreeInstance(_,t,s,_) =>  expectedCounts[L,W](grammar,lexicon,t,s)}, (_:ExpectedCounts[W]) += (_:ExpectedCounts[W]) )
      val logProb = accum.logProb;

      val (finalRules,finalUnaries,finalWords) = accum.decode(grammar);

      for( (k,ctr) <- finalWords.rows) {
        println(k + " " + ctr.maxk(30));
      }

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (SplittingData(finalRules,finalUnaries,finalWords),logProb,improvement);
    }
    stateIterator.drop(10).dropWhile(_._3 > convergence).map { case (data,logP,_) => (data,logP) }.next
  }

  def splitGrammar[L,W](data: SplittingData[L,W],
                        trees: Treebank[L,W],
                        root: L,
                        nSplits:Int =3,
                        randomNoise: Double=0.1,
                        convergence: Double = 1E-4): Iterator[(SplittingData[(L,Int),W],Double)] = {
    def split(s: (L,Int) ) = {
      val (l,state) = s;
      if (l == root) Seq(s) else Seq( (l,state*2),(l,state*2+1));
    }

    val myTrees = trees.map { case TreeInstance(id,tree,sent, _) => TreeInstance(id,tree.map { (_,0) },sent) }
    val SplittingData(binaries,unaries,wordCounts) = data;

    val myRuleCounts = PairedDoubleCounter[(L,Int),BinaryRule[(L,Int)]]();
    for ( (l,rule,w) <- binaries.triples) rule match {
      case BinaryRule(par,lc,rc) =>
        myRuleCounts( (l,0), BinaryRule( (l,0), (lc,0), (rc,0))) = w;
    }
    val myUnaries = PairedDoubleCounter[(L,Int),UnaryRule[(L,Int)]]();
    for ( (l,rule,w) <- unaries.triples) rule match {
      case UnaryRule(par,child) =>
      myUnaries( (l,0), UnaryRule( (l,0), (child,0))) = w;
    }

    val myWordCounts = PairedDoubleCounter[(L,Int),W]();
    for ( (l,word,w) <- wordCounts.triples) {
      myWordCounts( (l,0), word) = w;
    };

    val splittingData = SplittingData(myRuleCounts, myUnaries, myWordCounts)
    splitGrammar(splittingData,myTrees,split _, nSplits, randomNoise, convergence);
  }


  def splitStringGrammar(data: SplittingData[String,String],
                   trees: Treebank[String,String],
                   nSplits:Int =3,
                   randomNoise: Double=0.1,
                   convergence: Double = 1E-4): Iterator[(SplittingData[String,String],Double)] = {
    def split(s: String ) = {
      if (s == "") Seq(s) else if(s.contains("---")) Seq( s+"0",s+"1") else Seq(s+"---0",s+"---1");
    }

    splitGrammar(data, trees,split _, nSplits, randomNoise, convergence);
  }

  case class SplittingData[L,W](binaryRules: PairedDoubleCounter[L,BinaryRule[L]],
                                unaryRules: PairedDoubleCounter[L,UnaryRule[L]],
                                tagCounts: PairedDoubleCounter[L,W]);
  type Treebank[L,W] = IndexedSeq[TreeInstance[L,W]];
  type SplitTreebank[L,W] = IndexedSeq[TreeInstance[Seq[L],W]];

  def splitGrammar[L,W](initData: SplittingData[L,W],
                      unsplitTrees: Treebank[L,W],
                      splitter: L=>Seq[L], nSplits: Int,
                      randomNoise: Double,  convergence: Double): Iterator[(SplittingData[L,W],Double)]  = {
    val oneCycle = splitCycle(_:SplittingData[L,W],_:SplitTreebank[L,W],splitter,randomNoise,convergence);

    def splitTree(tree: BinarizedTree[L]):BinarizedTree[Seq[L]] =  tree.map(splitter);
    def resplitTree(tree: BinarizedTree[Seq[L]]):BinarizedTree[Seq[L]] =  tree.map(_ flatMap splitter);
    def splitHelper(pair: TreeInstance[L,W], splitFun: BinarizedTree[L]=>BinarizedTree[Seq[L]]) = (
      TreeInstance(pair.id,splitFun(pair.tree),pair.words)
    );

    val initState = (initData,Double.NegativeInfinity, splitTree _)
    val iterations = Iterator.iterate( initState ) { state =>
      val (data,logProb,splitFun) = state;
      val myTrees = unsplitTrees map { splitHelper(_,splitFun) } toIndexedSeq;
      val (newData,newLP) = oneCycle(data,myTrees);
      (newData,newLP,splitFun andThen resplitTree);
    }

    for( (data,logProb,_) <- iterations) yield (data,logProb);

  }

}

object StateSplittingTrainer extends ParserTrainer with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params) = {

    println("Extracting counts");
    val (initialLex,initialProductions,initUnR) = GenerativeParser.extractCounts(trainTrees)

    val coarseGrammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initialProductions),
      LogCounters.logNormalizeRows(initUnR));
    println("Splitting Grammar");
    import StateSplitting._;
    val data = SplittingData(initialProductions, initUnR, initialLex)
    for( (SplittingData(finalProd,finalUn,finalLex),logProb) <-  splitGrammar(data,trainTrees,"")) yield {
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(finalProd),LogCounters.logNormalizeRows(finalUn));
      val lex = new SimpleLexicon(finalLex);
      val builder = CKYChartBuilder(("",0),lex,grammar);
      def proj(l: (String,Int)) = l._1;
      val parser = ProjectingParser(builder,coarseGrammar.index,proj);
      ("split",parser);
    }
  }
}