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
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.parser.ParserParams.NoParams;
import scalala.library.Numerics.logSum;
import scalanlp.parser._;
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.UnaryTree
import scalanlp.parser.projections._;

import math.exp

import InsideOutside._
import scalala.tensor.{Counter,Counter2}
import scalala.library.{Numerics, Library}
import java.util.Arrays
import scalala.tensor.dense.DenseVector
import collection.mutable.BitSet
import scalanlp.util.{Index, Encoder}

/**
 * State Splitting implements the InsideOutside/Tree Message Passing algorithm for Matsuzaki et all 2005
 * Given a gold bracketing and a set of candidate labels, it computes expected counts for those candidate labels
 */
object StateSplitting {

  case class Beliefs(candidates: Seq[Int], inside: Array[Double], outside: Array[Double]) {
    def format[L](idx: Index[L]) = {
      "Beliefs(" + candidates.map(idx.get _) +"," + inside.mkString("{",","," }") +"," + outside.mkString("{",", ","}")+")"
    }
  }
  object Beliefs {
    def apply(candidates: Seq[Int]):Beliefs = {
      val r = this(candidates,new Array[Double](candidates.length),new Array[Double](candidates.length))
      Arrays.fill(r.inside,Double.NegativeInfinity)
      Arrays.fill(r.outside,Double.NegativeInfinity)
      r
    }
  }

  def insideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity) = {
    val indexedTree:BinarizedTree[Beliefs] = tree.map{  candidates => Beliefs(candidates.map(grammar.labelIndex))}
    val arr = new Array[Double](64 * 64)

    indexedTree.postorder.foreach {
      case t@NullaryTree(Beliefs(labels,scores,_)) =>
        // fill in POS tags:
        assert(t.span.length == 1);
        val word = s(t.span.start);
        var foundOne = false;
        for( i <- 0 until scores.length;
            a = labels(i);
            wScore = lexicon.wordScore(grammar.labelIndex.get(a),word) + scorer.scoreSpan(t.span.start,t.span.end,a);
            if !wScore.isInfinite) {
          scores(i) = wScore
          assert(!wScore.isInfinite)
          foundOne = true;
        }
        if(!foundOne) {
          val available: IndexedSeq[(L, Double)] =  (0 until grammar.labelIndex.size).map { i =>
            grammar.labelIndex.get(i) -> scorer.scoreSpan(t.span.start,t.span.end,i)
          }.toIndexedSeq
          sys.error("Trouble with lexical " + s(t.span.start) + " " + t.label.candidates.map(grammar.labelIndex.get _) + "\n" + available.mkString("Available:",",",""))
        }
      case t@UnaryTree(Beliefs(aLabels,aScores,_),Tree(Beliefs(cLabels,cScores,_),_)) =>
        var foundOne = false;
        var ai = 0
        while(ai < aLabels.length) {
          var i = 0
          while(i < cLabels.length) {
            val a = aLabels(ai)
            val c = cLabels(i)
            val ruleScore = (grammar.ruleScore(a,c)
              + cScores(i)
              + scorer.scoreUnaryRule(t.span.start, t.span.end, grammar.ruleIndex(a, c)));
            if(!ruleScore.isInfinite) {
              foundOne = true;
            }
            arr(i) = ruleScore
            i += 1
          }
          aScores(ai) = Numerics.logSum(arr,i)
          ai += 1
        }

        if(!foundOne) {
          sys.error("Trouble with unary " + t.render(s))
        }
      case t@BinaryTree(Beliefs(aLabels,aScores,_),Tree(Beliefs(bLabels,bScores,_),_),Tree(Beliefs(cLabels,cScores,_),_)) =>
        var foundOne = false;
        val begin = t.span.start
        val split = t.leftChild.span.end
        val end = t.span.end
        var ai = 0
        while(ai < aScores.length) {
          val a = aLabels(ai)
          var i = 0;
          var bi = 0
          while(bi < bLabels.length) {
            val b = bLabels(bi)
            var ci = 0
            while(ci < cLabels.length) {
              val c = cLabels(ci)
              arr(i) = ( grammar.ruleScore(a,b,c)
                + bScores(bi)
                + cScores(ci)
                + scorer.scoreBinaryRule(begin,split,end,grammar.ruleIndex(a,b,c)) + scorer.scoreSpan(begin,end,a)
                )
              i += 1;
              ci += 1
            }
            bi += 1
          };
          aScores(ai) = logSum(arr,i)
          if(!aScores(ai).isInfinite) foundOne = true;
          else println("wtf!!!!")
          ai += 1
       }

        if(!foundOne) {
          sys.error("Trouble with binary " + t.render(s))
        }
      case _ => sys.error("bad tree!");
    }

    indexedTree;
  }

  def outsideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], s: Seq[W], tree: BinarizedTree[Beliefs], scorer: SpanScorer[L]= SpanScorer.identity) {
    // Root gets score 0
    Arrays.fill(tree.label.outside,0.0)

    // Set the outside score of each child
    tree.preorder.foreach {
      case t @ BinaryTree(_,lchild,rchild) =>
        for {
          (p,pScore) <- t.label.candidates zip t.label.outside
          li <- 0 until lchild.label.candidates.length
          l = lchild.label.candidates(li)
          lScore = lchild.label.inside(li)
          ri <- 0 until rchild.label.candidates.length
          r = rchild.label.candidates(ri)
          rScore = rchild.label.inside(ri)
        } {
          val spanScore = (
            scorer.scoreBinaryRule(t.span.start, lchild.span.end, t.span.end, grammar.ruleIndex(p, l, r))
            + scorer.scoreSpan(t.span.start, t.span.end, p)
          )
          val rS = grammar.ruleScore(p,l,r) + spanScore
          lchild.label.outside(li) = logSum(lchild.label.outside(li),pScore + rScore + rS)
          rchild.label.outside(ri) = logSum(rchild.label.outside(ri),pScore + lScore + rS)
        }
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_,child) =>
        val arr = new Array[Double](t.label.candidates.size)
        for {
          (c,ci) <- child.label.candidates.zipWithIndex
        } {
          var i = 0
          for {
            (p,pScore) <- t.label.candidates zip t.label.outside
          } {
            val rScore = grammar.ruleScore(p,c) + scorer.scoreUnaryRule(t.span.start,t.span.end,grammar.ruleIndex(p,c));
            arr(i) = pScore + rScore
            i += 1
          }
          child.label.outside(ci) = Numerics.logSum(arr,arr.length)
        }


    }

  }


  def calibrateTree[W, L](grammar: Grammar[L], lexicon: Lexicon[L, W], tree: BinarizedTree[Seq[L]], s: scala.Seq[W], scorer: SpanScorer[L] = SpanScorer.identity[L]) = {
    val stree = insideScores(grammar, lexicon, tree, s, scorer);
    outsideScores(grammar, lexicon, s, stree, scorer);
    stree
  }

  def expectedSymbolCounts[L](grammar: Grammar[L], tree: BinarizedTree[Beliefs]) = {
    val counts = grammar.labelEncoder.mkDenseVector()
    tree.allChildren.foreach { node =>
      val c = Library.logNormalize(DenseVector.tabulate(node.label.candidates.length)(i => node.label.inside(i) + node.label.outside(i)))
      var i = 0;
      while(i < node.label.candidates.length) {
        counts(node.label.candidates(i)) += math.exp(c(i))
        i += 1
      }
    }
    counts
  }

  def expectedCounts[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity[L], acc: ExpectedCounts[W] = null) = {
    val counts = if(acc eq null) new ExpectedCounts[W](grammar) else acc
    val ruleCounts = counts.ruleCounts
    val wordCounts = counts.wordCounts

    val stree = calibrateTree(grammar, lexicon, tree, s, scorer)

    // normalizer
    val totalProb = logSum(stree.label.inside,stree.label.inside.length)
    if(totalProb.isInfinite || totalProb.isNaN)
      sys.error("NAn or infinite" + totalProb + " " + tree.render(s));

    stree.allChildren foreach {
      case t@NullaryTree(Beliefs(labels,iScores,oScores)) =>
        for( i <- 0 until  labels.length) {
          val l = labels(i)
          val iS = iScores(i)
          val oS = oScores(i)
          val ruleScore = (iS + oS - totalProb);
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore).isInfinite);
          wordCounts.getOrElseUpdate(l)(s(t.span.start)) +=  exp(ruleScore);
        }
      case t@UnaryTree(Beliefs(aLabels,_,aScores),Tree(Beliefs(cLabels,cScores,_),_)) =>
        var pi = 0
        while(pi < aLabels.size) {
          val p = aLabels(pi)
          val opScore = aScores(pi)
          pi += 1
          var ci = 0
          while(ci < cLabels.size) {
            val c = cLabels(ci)
            val icScore = cScores(ci)
            ci += 1
            val ruleScore = opScore + icScore + grammar.ruleScore(p,c) - totalProb;
            val span = scorer.scoreUnaryRule(t.span.start,t.span.end,grammar.ruleIndex(p,c));
            assert(!ruleScore.isNaN);
           // assert(exp(ruleScore) > 0, " " + ruleScore);
            assert(!exp(ruleScore + span).isInfinite,ruleScore + " " + span);
            ruleCounts(grammar.ruleIndex(p,c)) += exp(ruleScore + span);
          }
        }
      case t@BinaryTree(Beliefs(aLabels,_,aScores),Tree(Beliefs(bLabels,bScores,_),_),Tree(Beliefs(cLabels,cScores,_),_)) =>
        val begin = t.span.start
        val split = t.rightChild.span.start
        val end = t.span.end
        for {
          (p,opScore) <- aLabels zip aScores
          (l,ilScore) <- bLabels zip bScores
          (r,irScore) <- cLabels zip cScores
        } {
          val ruleScore = opScore + irScore + ilScore + grammar.ruleScore(p,l,r) - totalProb;
          val span = scorer.scoreBinaryRule(begin,split,end,grammar.ruleIndex(p,l,r)) + scorer.scoreSpan(begin,end,p)
          assert(!ruleScore.isNaN);
          //assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore + span).isInfinite, ruleScore + " " + span + " " + (ruleScore + span) + " " + totalProb);
          ruleCounts(grammar.ruleIndex(p,l,r)) += exp(ruleScore + span);
        }
    }

    //println("Problems: " + numProblems * 1.0 / numTotal);
    ExpectedCounts(ruleCounts,wordCounts,counts.logProb + totalProb);
  }

  def splitCounts[L,L2,W](data: SplittingData[L,W],
                          splitter: L=>Seq[L2],
                          randomNoise: Double = 0.01):SplittingData[L2,W] = {

    val SplittingData(binaries,unaries,wordCounts) = data;

    val splitRules = Counter2[L2,BinaryRule[L2],Double]();
    for {
      ((_,baseR),count) <- binaries.nonzero.pairs;
      r <- splitRule(baseR,splitter)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitRules(r.parent,r) = count * x;
    }

    val splitUnaries = Counter2[L2,UnaryRule[L2],Double]();
    for {
      ((_,baseR),count) <- unaries.nonzero.pairs;
      r <- splitRule(baseR,splitter)
    } {
      val x = (1 + 2 * randomNoise * math.random - randomNoise);
      splitUnaries(r.parent,r) = count * x;
    }

    val splitWords = Counter2[L2,W,Double]();
    for {
      ((l,w),count) <- wordCounts.nonzero.pairs
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



  def splitCycle[L,W](splitData: SplittingData[L,W],
                      trees: SplitTreebank[L,W],
                      splitter: L=>Seq[L],
                      randomNoise: Double =0.1,convergence:Double=1E-4,maxIter:Int = 50):
                      (SplittingData[L,W],Double) = {

    val stateIterator = Iterator.iterate( (splitData,Double.MaxValue,convergence.abs*100,0)) { case state =>
      val (SplittingData(splitRules,splitUnRules, splitWords),lastLogProb,_,it) = state;
      assert(splitRules.sum != 0)
      assert(splitUnRules.sum != 0)
      assert(splitWords.sum != 0)
      val grammar = Grammar(Library.logAndNormalizeRows(splitRules), Library.logAndNormalizeRows(splitUnRules));
      val lexicon = new UnsmoothedLexicon(Library.logAndNormalizeRows(splitWords));
      val accum = trees.par.aggregate[ExpectedCounts[W]](null)( (acc,treeInstance) => expectedCounts[L,W](grammar,lexicon,treeInstance.tree,treeInstance.words,acc=acc), _ += _)
      val logProb = accum.logProb;

      val ((finalRules,finalUnaries),finalWords) = accum.decode(grammar);

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (SplittingData(finalRules,finalUnaries,finalWords),logProb,improvement,it+1);
    }
    stateIterator.drop(10).dropWhile(x => x._3 > convergence && (x._4 < maxIter || maxIter < 0)).map { case (data,logP,_,_) => (data,logP) }.next()
  }

  // returns the labels whose refinements should be merged
  def determineLabelsToUnsplit[L,W](oneStepProjections: ProjectionIndexer[L,L],
                      builder: ChartBuilder[ParseChart,L,W],
                      trees: SplitTreebank[L,W],
                      percentMerging: Double = 0.5) = {
    val calibratedTrees = trees.par.map { case TreeInstance(_,t,s,_) => calibrateTree(builder.grammar,builder.lexicon,t,s)}
    val symbolCounts = calibratedTrees.map(expectedSymbolCounts(builder.grammar,_)).reduceLeft{ _ += _ } + 1E-2
    val symbolTrials = {
      DenseVector.tabulate(symbolCounts.length){f =>
        val sibs = oneStepProjections.refinements(oneStepProjections.project(f))
        sibs.map(symbolCounts).sum
      }
    }
//    assert(!symbolTrials.valuesIterator.exists(_ == 0.0), builder.grammar.labelEncoder.decode(symbolTrials))
    val symbolProbs = (symbolCounts :/ symbolTrials).map(math.log)
    assert(!symbolProbs.valuesIterator.exists(_.isNaN))
    assert(!symbolProbs.valuesIterator.exists(_.isInfinite), symbolProbs.pairsIterator.toIndexedSeq.mkString("\n"))
    val mergeCosts = calibratedTrees.map(computeMergeCosts(oneStepProjections,symbolProbs,_)).reduce(_ += _)
    val topK = (0 until oneStepProjections.coarseIndex.size).sortBy(i => -mergeCosts(i)).take(percentMerging * oneStepProjections.coarseIndex.size toInt)
    println("Will merge:")
    for(i <- topK) {
      println(i + ": " + oneStepProjections.refinementsOf(oneStepProjections.coarseIndex.get(i)))
    }
    topK.map(oneStepProjections.coarseIndex.get).toSet
  }

  private def mergeRules[L,W](splitData: SplittingData[L,W], project: L=>L) = {
    val SplittingData(splitRules,splitUnRules, splitWords) = splitData;

    val newBinaryRules = Counter2[L,BinaryRule[L],Double]()
    val newUnaryRules = Counter2[L,UnaryRule[L],Double]()
    val newTagCounts = Counter2[L,W,Double]()
    for( (sym,rule,count) <- splitRules.triples) {
      newBinaryRules(project(sym),rule.map(project)) += count
    }

    for( (sym,rule,count) <- splitUnRules.triples) {
      newUnaryRules(project(sym),rule.map(project)) += count
    }

    for( (sym,w,count) <- splitWords.triples) {
      newTagCounts(project(sym),w) += count
    }

    SplittingData(newBinaryRules, newUnaryRules, newTagCounts)

  }

  private def computeMergeCosts[L,L2](proj: ProjectionIndexer[L,L2], symbolLogProbs: Int => Double, trees: BinarizedTree[Beliefs]) = {
    val costs = proj.coarseEncoder.mkDenseVector()
    // inside merged
    val insideBins = Array.fill(costs.length)(Double.NegativeInfinity)
    // outside merged
    val outsideBins = Array.fill(costs.length)(Double.NegativeInfinity)
    // inside not merged
    val rawBins = Array.fill(costs.length)(Double.NegativeInfinity)
    // index into the above arrays for corresponding coarse symbols
    val coarseRef = Array.fill(costs.length)(0)
    trees.allChildren foreach { node =>
      Arrays.fill(insideBins,Double.NegativeInfinity)
      Arrays.fill(outsideBins,Double.NegativeInfinity)
      Arrays.fill(rawBins,Double.NegativeInfinity)
      Arrays.fill(coarseRef,-1)
      var offset = 0
      for(i <- 0 until node.label.candidates.length) {
        val f = node.label.candidates(i)
        val c = proj.project(f)
        var ci = coarseRef.indexWhere(_ == c)
        if(ci == -1) {
          ci = offset
          coarseRef(ci) = c
          offset += 1
        }
        insideBins(ci) = logSum(insideBins(ci),symbolLogProbs(f) + node.label.inside(i))
        outsideBins(ci) = logSum(outsideBins(ci),node.label.outside(i))
        rawBins(ci) = logSum(rawBins(ci),node.label.outside(i) + node.label.inside(i))
      }
      for(i <- 0 until offset) {
        val c  = coarseRef(i)
        if(!rawBins(i).isInfinite) // underflow happens a lot, actually...
          costs(c) += (insideBins(i)  + outsideBins(i) - rawBins(i))
      }


    }
    costs
  }

  def splitGrammar[L,W](data: SplittingData[L,W],
                        trees: Treebank[L,W],
                        root: L,
                        nSplits:Int =3,
                        randomNoise: Double=0.1,
                        convergence: Double = 1E-4): Iterator[(SplittingData[(L,Seq[Int]),W],Double)] = {
    def split(s: (L,Seq[Int]) ) = {
      val (l,state) = s;
      if (l == root) Seq(s) else Seq( (l,state :+ 0),(l,state :+ 1));
    }

    def proj(s: (L,Seq[Int]))= s._1

    val myTrees = trees.map { case TreeInstance(id,tree,sent, _) => TreeInstance(id,tree.map { (_,Seq.empty[Int]) },sent) }
    val SplittingData(binaries,unaries,wordCounts) = data;

    // turn every label l into (l,0)
    val myRuleCounts = Counter2[(L,Seq[Int]),BinaryRule[(L,Seq[Int])],Double]();
    for ( (l,rule,w) <- binaries.triples) rule match {
      case BinaryRule(par,lc,rc) =>
        myRuleCounts( (l,Seq.empty), BinaryRule( (l,Seq.empty), (lc,Seq.empty), (rc,Seq.empty))) = w;
    }
    val myUnaries = Counter2[(L,Seq[Int]),UnaryRule[(L,Seq[Int])],Double]();
    for ( (l,rule,w) <- unaries.triples) rule match {
      case UnaryRule(par,child) =>
      myUnaries( (l,Seq.empty), UnaryRule( (l,Seq.empty), (child,Seq.empty))) = w;
    }

    val myWordCounts = Counter2[(L,Seq[Int]),W,Double]();
    for ( ((l,word),w) <- wordCounts.nonzero.pairs) {
      myWordCounts( (l,Seq.empty), word) = w;
    };

    val splittingData = SplittingData(myRuleCounts, myUnaries, myWordCounts)
    splitGrammar(splittingData,myTrees,split _, nSplits, randomNoise, convergence);
  }

  case class SplittingData[L,W](binaryRules: Counter2[L,BinaryRule[L],Double],
                                unaryRules: Counter2[L,UnaryRule[L],Double],
                                tagCounts: Counter2[L,W,Double]);
  type Treebank[L,W] = IndexedSeq[TreeInstance[L,W]];
  type SplitTreebank[L,W] = IndexedSeq[TreeInstance[Seq[L],W]];

  def splitGrammar[L,W](initData: SplittingData[L,W],
                      unsplitTrees: Treebank[L,W],
                      splitter: L=>Seq[L], nSplits: Int,
                      randomNoise: Double,  convergence: Double): Iterator[(SplittingData[L,W],Double)]  = {
    val oneCycle = splitCycle(_:SplittingData[L,W],_:SplitTreebank[L,W],splitter,randomNoise,convergence);

    def splitHelper(pair: TreeInstance[L,W], splitFun: BinarizedTree[L]=>BinarizedTree[Seq[L]]) = (
      TreeInstance(pair.id,splitFun(pair.tree),pair.words)
    );

    val coarseGrammar = Grammar(initData.binaryRules, initData.unaryRules);
    val initProjections = ProjectionIndexer.simple(coarseGrammar.labelIndex)

    val initState = (initData,Set.empty[L],Double.NegativeInfinity, initProjections)
    val iterations = Iterator.iterate( initState ) { state =>
      val (data,thingsToNotSplit,logProb,oldProjections) = state;
      def conditionalSplit(refined: L) = {
        if(thingsToNotSplit(refined)) Seq(refined)
        else splitter(refined)
      }
      // last split -> new split
      val splitProjections = ProjectionIndexer.fromSplitter(oldProjections.fineIndex, conditionalSplit)

      // no split -> new split
      val fullProjections = oldProjections compose splitProjections;
      def split(sym: L) = fullProjections.refinementsOf(sym)
      val myTrees = unsplitTrees.map { splitHelper(_, _.map(split)) }.toIndexedSeq;
      val splitData = splitCounts(data,splitProjections.refinementsOf(_:L),randomNoise);
      val (newData,newLP) = oneCycle(splitData,myTrees);

      val grammar = Grammar(Library.logAndNormalizeRows(newData.binaryRules),
        Library.logAndNormalizeRows(newData.unaryRules));
      val lex = new SimpleLexicon(newData.tagCounts);
      // TODO, ugh
      val builder = CKYChartBuilder(null.asInstanceOf[L],lex,grammar);
      val labelsToUnsplit = this.determineLabelsToUnsplit(splitProjections,builder, myTrees)
      val labelsToNotSplit = labelsToUnsplit.flatMap(splitter).toSet

      (newData,labelsToNotSplit,newLP,fullProjections)
    }

    (for( (data,_, logProb,_) <- iterations) yield data -> logProb)

  }


}

object StateSplittingPipeline extends ParserPipeline with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  params: Params) = {

    println("Extracting counts");
    val (initialLex,initialProductions,initUnR) = GenerativeParser.extractCounts(trainTrees)

    val coarseGrammar = Grammar(Library.logAndNormalizeRows(initialProductions),
      Library.logAndNormalizeRows(initUnR));
    println("Splitting Grammar");
    import StateSplitting._;
    val data = SplittingData(initialProductions, initUnR, initialLex)
    var iter = 0
    for( (SplittingData(finalProd,finalUn,finalLex),logProb) <-  splitGrammar(data,trainTrees,"")) yield {
      val grammar = Grammar(Library.logAndNormalizeRows(finalProd),Library.logAndNormalizeRows(finalUn));
      val lex = new SimpleLexicon(finalLex);
      val builder = CKYChartBuilder(("",Seq.empty),lex,grammar);
      def proj(l: (String,Seq[Int])) = l._1;
      val parser = ProjectingParser(builder,coarseGrammar,proj);
      iter += 1
      val name = if(iter == 1) {
        "base"
      } else {
        "split-" + iter
      }
      (name,parser);
    }
  }
}