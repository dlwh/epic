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
import scalanlp.collection.mutable.SparseArray
import scalanlp.collection.mutable.TriangularArray
import scalanlp.counters.Counters._;
import scalanlp.counters.LogCounters;
import scalanlp.counters.LogCounters.LogPairedDoubleCounter;
import scalanlp.counters.LogCounters.LogDoubleCounter;
import scalanlp.math.Numerics.logSum;
import scalanlp.parser._;
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees
import scalanlp.trees.UnaryTree

import Math.exp

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
          score(t.span.start)(t.span.start+1)(a) = wScore;
        }
      case t : UnaryTree[Seq[Int]] =>
        val spanScore = score(t.span.start)(t.span.end);
        for {
          c <- t.child.label
          rules = grammar.unaryRulesByIndexedChild(c);
          a <- t.label
        } {
          val ruleScore = rules(a) + spanScore(c);
          spanScore(a) = logSum(spanScore(a), ruleScore);
        }
      case t@BinaryTree(_,lchild,rchild) =>
        val spanScore = score(t.span.start)(t.span.end);
        val begin = t.span.start;
        val split = t.leftChild.span.end;
        assert(split == rchild.span.start);
        val end = t.span.end;
         for {
          a <- t.label
       } {
         val scores = for {
           b <- lchild.label
           rules = grammar.binaryRulesByIndexedLeftChild(b);
           c <- rchild.label
         } yield {
           rules(c)(a) + score(begin)(split)(b) + score(split)(end)(c);
         };
         spanScore(a) = logSum(scores);
       }
      case _ => error("bad tree!");
    }

    score;
  }

  def outsideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], insideScores: TriangularArray[Vector]) = {
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };
    val score = new TriangularArray(s.length+1, grammar.mkVector(Double.NegativeInfinity));
    // Root gets score 0
    score(0)(s.length)(indexedTree.label) = 0.0;

    // Set the outside score of each child
    indexedTree.preorder.foreach {
      case t @ BinaryTree(_,lchild,rchild) =>
        for {
          p <- t.label.iterator
          pScore = score(t.span.start)(t.span.end)(p);
          l <- lchild.label.iterator
          lScore = insideScores(lchild.span.start)(lchild.span.end)(l);
          lRules = grammar.binaryRulesByIndexedLeftChild(l);
          r <- rchild.label.iterator
          rScore = insideScores(rchild.span.start)(rchild.span.end)(r)
        } {
          val rS = lRules(r)(p);
          score(lchild.span.start)(lchild.span.end)(l) = logSum(score(lchild.span.start)(lchild.span.end)(l),pScore + rScore + rS);
          score(rchild.span.start)(rchild.span.end)(r) = logSum(score(rchild.span.start)(rchild.span.end)(r),pScore + lScore + rS);
        }
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_,child) =>
        for {
          p <- t.label
          pScore = score(child.span.start)(child.span.end)(p);
          c <- child.label
        } {
          val rScore = grammar.unaryRulesByIndexedChild(c)(p)
          score(child.span.start)(child.span.end)(c) = logSum(score(child.span.start)(child.span.end)(c),pScore + rScore);
        }

    }

    score
  }

  def expectedCounts[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W]) = {
    val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity)));
    val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity));
    val wordCounts = grammar.fillSparseArray(LogDoubleCounter[W]());

    val iScores = insideScores(grammar,lexicon,tree,s);
    val oScores = outsideScores(grammar,lexicon, tree,s,iScores);
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };

    // normalizer
    val totalProb = logSum(iScores(0)(s.length)(tree.label map grammar.index));
    assert(!totalProb.isInfinite);

    indexedTree.allChildren foreach {
      case t: NullaryTree[Seq[Int]] =>
        for( l <- t.label) {
          val iS = iScores(t.span.start)(t.span.end)(l);
          val oS = oScores(t.span.start)(t.span.end)(l);
          val ruleScore = (iS + oS - totalProb);
          assert(!ruleScore.isNaN);
          //assert(!ruleScore.isInfinite);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          wordCounts(l)(s(t.span.start)) = logSum(wordCounts(l)(s(t.span.start)), ruleScore);
        }
      case t@UnaryTree(_,child) =>
        for {
          p <- t.label.iterator;
          opScore = oScores(t.span.start)(t.span.end)(p);
          c <- child.label.iterator
          icScore = iScores(child.span.start)(child.span.end)(c)
        } {
          val ruleScore = opScore + icScore + grammar.unaryRulesByIndexedChild(c)(p) - totalProb;
          assert(!ruleScore.isNaN);
         // assert(!ruleScore.isInfinite);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          unaryRuleCounts(p)(c) = logSum(unaryRuleCounts(p)(c), ruleScore);
        }
      case t@ BinaryTree(_,lc,rc) =>
        for {
          p <- t.label iterator;
          opScore = oScores(t.span.start)(t.span.end)(p);
          l <- lc.label.iterator
          val lRules = grammar.binaryRulesByIndexedLeftChild(l);
          ilScore = iScores(lc.span.start)(lc.span.end)(l);
          r <- rc.label.iterator
        } {
          val irScore = iScores(rc.span.start)(rc.span.end)(r)
          val ruleScore = opScore + irScore + ilScore + lRules(r)(p) - totalProb;
          //assert(exp(ruleScore) > 0, " " + ruleScore);
          binaryRuleCounts(p)(l)(r) = logSum(binaryRuleCounts(p)(l)(r), ruleScore);
        }
    }
    (binaryRuleCounts,unaryRuleCounts,wordCounts,totalProb);
  }

  def splitCounts[L,L2,W](ruleCounts: RuleCounts[L],
                           wordCounts: TagWordCounts[L,W],
                           splitter: L=>Seq[L2], randomNoise: Double = 0.1) = {

    val splitRules = LogPairedDoubleCounter[L2,Rule[L2]]();
    for {
      ((_,baseR),count) <- ruleCounts;
      r <- splitRule(baseR,splitter);
      x = Math.log(1 + 2 * randomNoise * Math.random - randomNoise)
    } {
      splitRules(r.parent,r) = count + x;
    }

    val splitWords = LogPairedDoubleCounter[L2,W]();
    for {
      ((l,w),count) <- wordCounts
      r <- splitter(l);
      x = Math.log(1 + 2 * randomNoise * Math.random - randomNoise)
    } {
      splitWords(r,w) = count + x;
    }

    (splitRules,splitWords);
  }

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


  private def splitRule[L,L2,W](r: Rule[L], splitter: L=>Seq[L2]):Seq[Rule[L2]] = r match {
    case UnaryRule(p,c) => for { pp <- splitter(p) ; cc <- splitter(c)} yield UnaryRule(pp,cc);
    case BinaryRule(p,l,r) => 
      for { pp <- splitter(p) ; ll <- splitter(l); rr <- splitter(r)} yield BinaryRule(pp,ll,rr);
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
        (t,s) <- trees
      } yield expectedCounts(grammar,lexicon,t,s);

      val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity)));
      val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity));
      val wordCounts = grammar.fillSparseArray(LogDoubleCounter[W]());
      var logProb = 0.0;

      for( (bCounts,uCounts,wCounts,tProb) <- results) {
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
      }

      val finalRules = decodeRules(grammar,binaryRuleCounts,unaryRuleCounts);
      val finalWords = decodeWords(grammar,wordCounts);

      for( (k,ctr) <- finalWords.rows) {
        println(k + " " + ctr.maxk(30));
      }

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (finalRules,finalWords,logProb,improvement);
    }
    stateIterator.drop(10).dropWhile(_._4 > convergence).map { case (sR,sW,logP,_) => (sR,sW,logP) }.next
  }

  def decodeRules[L](g: Grammar[L],
                     binaryRuleCounts: SparseArray[SparseArray[Vector]],
                     unaryRuleCounts: SparseArray[Vector]) = {
    val ctr = LogPairedDoubleCounter[L,Rule[L]]();

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

    val myTrees = trees.view.map { case (tree,sent) => (tree.map { (_,0) },sent) }

    val myRuleCounts = LogPairedDoubleCounter[(L,Int),Rule[(L,Int)]]();
    for ( (l,rule,w) <- ruleCounts.triples) rule match {
      case UnaryRule(par,child) =>
        myRuleCounts( (l,0), UnaryRule( (l,0), (child,0))) = w;
      case BinaryRule(par,lc,rc) =>
        myRuleCounts( (l,0), UnaryRule( (lc,0), (rc,0))) = w;
    }

    val myWordCounts = LogPairedDoubleCounter[(L,Int),W]();
    for ( (l,word,w) <- wordCounts.triples) {
      myWordCounts( (l,0), word) = w;
    }

    splitGrammar(myRuleCounts,myWordCounts,myTrees,split _, nSplits, randomNoise, convergence);
  }


  def decodeWords[L,W](g: Grammar[L], wordCounts: SparseArray[LogDoubleCounter[W]]) = {
    val ctr = LogPairedDoubleCounter[L,W]();
    for( (i,c) <- wordCounts) {
      ctr(g.index.get(i)) := c;
    }
    ctr;
  }

  type RuleCounts[L] = LogPairedDoubleCounter[L,Rule[L]];
  type TagWordCounts[L,W] = LogPairedDoubleCounter[L,W];
  type Treebank[L,W] = Seq[(BinarizedTree[L],Seq[W])];
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

object StateSplittingTest {
  import java.io.File;
 // import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    println("Loading Treebank");
    val trees = (for( (tree,words) <- treebank.trainTrees take 2000)
      yield (Trees.xBarBinarize(xform(tree)),words map (_.intern))) toSeq;
    println("Extracting counts");
    val (initialLex,initialProductions) = (
      GenerativeParser.extractCounts(trees.iterator,Trees.xBarBinarize _)
    );
    println("Splitting Grammar");
    import StateSplitting._;
    val (finalLex,finalProd,logProb) = splitGrammar(LogCounters.log(initialProductions),LogCounters.log(initialLex),trees,"");
  }

}