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


import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.collection.mutable.TriangularArray
import scalanlp.config.Configuration
import scalala.tensor.counters.Counters._;
import scalala.tensor.counters.LogCounters;
import scalanlp.math.Numerics.logSum;
import scalanlp.parser.ParserTrainer
import scalanlp.parser._;
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees
import scalanlp.trees.UnaryTree
import scalanlp.parser.projections._;

import math.exp

import InsideOutside._;

object StateSplitting {

  def insideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W]) = {
    val score = new TriangularArray(s.length+1, grammar.mkVector(Double.NegativeInfinity));
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };

    indexedTree.postorder.foreach {
      case t : NullaryTree[Seq[Int]] =>
        // fill in POS tags:
        assert(t.span.length == 1);
        val word = s(t.span.start);
        for( a <- t.label.iterator;
            wScore = lexicon.wordScore(grammar.index.get(a),word)
            if !wScore.isInfinite) {
          score(t.span.start,t.span.start+1)(a) = wScore;
        }
      case t : UnaryTree[Seq[Int]] =>
        val spanScore = score(t.span.start,t.span.end);
        for {
          c <- t.child.label
          rules = grammar.unaryRulesByIndexedChild(c);
          a <- t.label
        } {
          val ruleScore = rules(a) + spanScore(c);
          spanScore(a) = logSum(spanScore(a), ruleScore);
        }
      case t@BinaryTree(_,lchild,rchild) =>
        val spanScore = score(t.span.start,t.span.end);
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
              arr(i) = rules(c)(a) + score(begin,split)(b) + score(split,end)(c);
              i += 1;
            }
          };
          spanScore(a) = logSum(arr);
       }
      case _ => error("bad tree!");
    }

    score;
  }

  def outsideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], insideScores: TriangularArray[Vector]) = {
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };
    val score = new TriangularArray(s.length+1, grammar.mkVector(Double.NegativeInfinity));
    // Root gets score 0
    score(0,s.length)(indexedTree.label) = 0.0;

    // Set the outside score of each child
    indexedTree.preorder.foreach {
      case t @ BinaryTree(_,lchild,rchild) =>
        for {
          p <- t.label.iterator
          pScore = score(t.span.start,t.span.end)(p);
          l <- lchild.label.iterator
          lScore = insideScores(lchild.span.start,lchild.span.end)(l);
          lRules = grammar.binaryRulesByIndexedLeftChild(l);
          r <- rchild.label.iterator
          rScore = insideScores(rchild.span.start,rchild.span.end)(r)
        } {
          val rS = lRules(r)(p);
          score(lchild.span.start,lchild.span.end)(l) = logSum(score(lchild.span.start,lchild.span.end)(l),pScore + rScore + rS);
          score(rchild.span.start,rchild.span.end)(r) = logSum(score(rchild.span.start,rchild.span.end)(r),pScore + lScore + rS);
        }
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_,child) =>
        for {
          p <- t.label
          pScore = score(child.span.start,child.span.end)(p);
          c <- child.label
        } {
          val rScore = grammar.unaryRulesByIndexedChild(c)(p)
          score(child.span.start,child.span.end)(c) = logSum(score(child.span.start,child.span.end)(c),pScore + rScore);
        }

    }

    score
  }


  def expectedCounts[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W]) = {
    val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(0.0)));
    val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(0.0));
    val wordCounts = grammar.fillSparseArray(DoubleCounter[W]());

    val iScores = insideScores(grammar,lexicon,tree,s);
    val oScores = outsideScores(grammar,lexicon, tree,s,iScores);
    val indexedTree:Tree[Seq[Int]] = tree.map { _.map(grammar.index) };

    // normalizer
    val totalProb = logSum(iScores(0,s.length)(tree.label map grammar.index));
    if(totalProb.isInfinite || totalProb.isNaN)
      error("NAn or infinite" + totalProb + " " + indexedTree.render(s) + "\n" + tree.render(s) + "\n" + iScores + "\n"+oScores);

    indexedTree.allChildren foreach {
      case t: NullaryTree[Seq[Int]] =>
        for( l <- t.label) {
          val iS = iScores(t.span.start,t.span.end)(l);
          val oS = oScores(t.span.start,t.span.end)(l);
          val ruleScore = (iS + oS - totalProb);
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          wordCounts.getOrElseUpdate(l)(s(t.span.start)) +=  exp(ruleScore);
        }
      case t@UnaryTree(_,child) =>
        for {
          p <- t.label.iterator;
          opScore = oScores(t.span.start,t.span.end)(p);
          c <- child.label.iterator
          icScore = iScores(child.span.start,child.span.end)(c)
        } {
          val ruleScore = opScore + icScore + grammar.unaryRulesByIndexedChild(c)(p) - totalProb;
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          unaryRuleCounts.getOrElseUpdate(p)(c) += exp(ruleScore);
        }
      case t@ BinaryTree(_,lc,rc) =>
        for {
          p <- t.label iterator;
          opScore = oScores(t.span.start,t.span.end)(p);
          l <- lc.label.iterator
          val lRules = grammar.binaryRulesByIndexedLeftChild(l);
          ilScore = iScores(lc.span.start,lc.span.end)(l);
          r <- rc.label.iterator
        } {
          val irScore = iScores(rc.span.start,rc.span.end)(r)
          val ruleScore = opScore + irScore + ilScore + lRules(r)(p) - totalProb;
          assert(!ruleScore.isNaN);
          //assert(exp(ruleScore) > 0, " " + ruleScore);
          binaryRuleCounts.getOrElseUpdate(p).getOrElseUpdate(l)(r) += exp(ruleScore);
        }
    }
    ExpectedCounts(binaryRuleCounts,unaryRuleCounts,wordCounts,totalProb);
  }

  def splitCounts[L,L2,W](ruleCounts: RuleCounts[L],
                           wordCounts: TagWordCounts[L,W],
                           splitter: L=>Seq[L2],
                           randomNoise: Double = 0.1):(RuleCounts[L2],TagWordCounts[L2,W]) = {

    val splitRules = PairedDoubleCounter[L2,Rule[L2]]();
    for {
      ((_,baseR),count) <- ruleCounts;
      r <- splitRule(baseR,splitter)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitRules(r.parent,r) = count * x;
    }

    val splitWords = PairedDoubleCounter[L2,W]();
    for {
      ((l,w),count) <- wordCounts
      r <- splitter(l)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitWords(r,w) = count * x;
    }

    (splitRules,splitWords);
  }

  private def splitRule[L,L2,W](r: Rule[L], splitter: L=>Seq[L2]):Iterator[Rule[L2]] = r match {
    case UnaryRule(p,c) => for { pp <- splitter(p).iterator ; cc <- splitter(c).iterator} yield UnaryRule(pp,cc);
    case BinaryRule(p,l,r) => 
      for { pp <- splitter(p).iterator ; ll <- splitter(l).iterator; rr <- splitter(r).iterator } yield BinaryRule(pp,ll,rr);
  }

  def splitCycle[L,W](ruleCounts: RuleCounts[L],
                      wordCounts: TagWordCounts[L,W],
                      trees: SplitTreebank[L,W],
                      splitter: L=>Seq[L],
                      randomNoise: Double =0.1,convergence:Double=1E-4):
                      (RuleCounts[L],TagWordCounts[L,W],Double) = {
    val (splitRules,splitWords) = splitCounts(ruleCounts,wordCounts,splitter,randomNoise);

    val stateIterator = Iterator.iterate( (splitRules,splitWords,Double.MaxValue,convergence.abs*100)) { case state =>
      val (splitRules,splitWords,lastLogProb,_) = state;
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(splitRules));
      val lexicon = new UnsmoothedLexicon(LogCounters.logNormalizeRows(splitWords));
      val results = for {
        (t,s) <- trees.iterator
      } yield expectedCounts(grammar,lexicon,t,s);

      val accum = results.reduceLeft( _ += _ );
      val logProb = accum.logProb;

      val (finalRules,finalWords) = accum.decode(grammar);

      for( (k,ctr) <- finalWords.rows) {
        println(k + " " + ctr.maxk(30));
      }

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (finalRules,finalWords,logProb,improvement);
    }
    stateIterator.drop(10).dropWhile(_._4 > convergence).map { case (sR,sW,logP,_) => (sR,sW,logP) }.next
  }

  def splitGrammar[L,W](ruleCounts: RuleCounts[L],
                        wordCounts: TagWordCounts[L,W],
                        trees: Treebank[L,W],
                        root: L,
                        nSplits:Int =3,
                        randomNoise: Double=0.1,
                        convergence: Double = 1E-4): (RuleCounts[(L,Int)],TagWordCounts[(L,Int),W],Double) = {
    def split(s: (L,Int) ) = {
      val (l,state) = s;
      if (l == root) Seq(s) else Seq( (l,state*2),(l,state*2+1));
    }

    val myTrees = trees.map { case (tree,sent) => (tree.map { (_,0) },sent) }

    val myRuleCounts = PairedDoubleCounter[(L,Int),Rule[(L,Int)]]();
    for ( (l,rule,w) <- ruleCounts.triples) rule match {
      case UnaryRule(par,child) =>
        myRuleCounts( (l,0), UnaryRule( (l,0), (child,0))) = w;
      case BinaryRule(par,lc,rc) =>
        myRuleCounts( (l,0), BinaryRule( (l,0), (lc,0), (rc,0))) = w;
    }

    val myWordCounts = PairedDoubleCounter[(L,Int),W]();
    for ( (l,word,w) <- wordCounts.triples) {
      myWordCounts( (l,0), word) = w;
    }

    splitGrammar(myRuleCounts,myWordCounts,myTrees,split _, nSplits, randomNoise, convergence);
  }


  def splitStringGrammar(ruleCounts: RuleCounts[String],
                   wordCounts: TagWordCounts[String,String],
                   trees: Treebank[String,String],
                   nSplits:Int =3,
                   randomNoise: Double=0.1,
                   convergence: Double = 1E-4): (RuleCounts[String],TagWordCounts[String,String],Double) = {
    def split(s: String ) = {
      if (s == "") Seq(s) else if(s.contains("---")) Seq( s+"0",s+"1") else Seq(s+"---0",s+"---1");
    }

    splitGrammar(ruleCounts,wordCounts,trees,split _, nSplits, randomNoise, convergence);
  }

  type RuleCounts[L] = PairedDoubleCounter[L,Rule[L]];
  type TagWordCounts[L,W] = PairedDoubleCounter[L,W];
  type Treebank[L,W] = IndexedSeq[(BinarizedTree[L],Seq[W])];
  type SplitTreebank[L,W] = Seq[(BinarizedTree[Seq[L]],Seq[W])];

  def splitGrammar[L,W](ruleCounts: RuleCounts[L],
                      wordCounts: TagWordCounts[L,W],
                      unsplitTrees: Treebank[L,W],
                      splitter: L=>Seq[L], nSplits: Int,
                      randomNoise: Double,  convergence: Double): (RuleCounts[L],TagWordCounts[L,W],Double)  = {
    val oneCycle = splitCycle(_:RuleCounts[L],_:TagWordCounts[L,W],_:SplitTreebank[L,W],splitter,randomNoise,convergence);

    def splitTree(tree: BinarizedTree[L]):BinarizedTree[Seq[L]] =  tree.map(splitter);
    def resplitTree(tree: BinarizedTree[Seq[L]]):BinarizedTree[Seq[L]] =  tree.map(_ flatMap splitter);
    def splitHelper(pair: (BinarizedTree[L],Seq[W]), splitFun: BinarizedTree[L]=>BinarizedTree[Seq[L]]) = (
      (splitFun(pair._1),pair._2)
    );

    val initState = (ruleCounts,wordCounts,Double.NegativeInfinity, splitTree _)
    val (finalCounts,finalWords,logProb,_) = Iterator.iterate( initState ) { state =>
      val (ruleCounts,wordCounts,logProb,splitFun) = state;
      val myTrees = unsplitTrees.view map { splitHelper(_,splitFun) };
      val (nextRules,nextWords,newLP) = oneCycle(ruleCounts,wordCounts,myTrees);
      (nextRules,nextWords,newLP,splitFun andThen resplitTree);
    } drop (nSplits) next

    //val (rules,wordCounts,logProb) = state;
    (finalCounts,finalWords,logProb);
  }

}

object StateSplittingTrainer extends ParserTrainer {
  def trainParser(trainTreesX: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration) = {

    val trainTrees = trainTreesX.map( c => (c._1,c._2));

    println("Extracting counts");
    val (initialLex,initialProductions) = (
      GenerativeParser.extractCounts(trainTrees.iterator
        )
    );

    val coarseGrammar = new GenerativeGrammar(LogCounters.logNormalizeRows(initialProductions));
    Iterator.tabulate(2) {
      case 0 =>
        ("raw",ChartParser(CKYChartBuilder("", new SimpleLexicon(initialLex), coarseGrammar) ))
      case 1 =>
        println("Splitting Grammar");
        import StateSplitting._;
        val (finalProd,finalLex,logProb) = splitGrammar(initialProductions,initialLex,trainTrees.toIndexedSeq,"");
        val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(finalProd));
        val lex = new SimpleLexicon(finalLex);
        val builder = CKYChartBuilder(("",0),lex,grammar);
        def proj(l: (String,Int)) = l._1;
        val parser = ProjectingParser(builder,coarseGrammar.index,proj);
        ("split",parser);
    }
  }
}