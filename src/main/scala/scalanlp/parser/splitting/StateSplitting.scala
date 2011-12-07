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
          sys.error("Trouble with lexical " + t.render(s))
        }
      case t@UnaryTree(Beliefs(aLabels,aScores,_),Tree(Beliefs(cLabels,cScores,_),_)) =>
        var foundOne = false;
        val arr = new Array[Double](cLabels.length)
        for(ai <- 0 until aLabels.length) {
          var i = 0
          for(ci <- 0 until cLabels.length) {
            val a = aLabels(ai)
            val c = cLabels(ci)
            val ruleScore = (grammar.ruleScore(a,c)
              + cScores(ci)
              + scorer.scoreUnaryRule(t.span.start, t.span.end, grammar.ruleIndex(a, c)));
            if(!ruleScore.isInfinite) {
              foundOne = true;
            }
            arr(i) = ruleScore
            i += 1
          }
          aScores(ai) = Numerics.logSum(arr,arr.length)
        }

        if(!foundOne) {
          sys.error("Trouble with unary " + t.render(s))
        }
      case t@BinaryTree(Beliefs(aLabels,aScores,_),Tree(Beliefs(bLabels,bScores,_),_),Tree(Beliefs(cLabels,cScores,_),_)) =>
        var foundOne = false;
        val begin = t.span.start
        val split = t.leftChild.span.end
        val end = t.span.end
        for {
          ai <- 0 until aLabels.length
        } {
          val a = aLabels(ai)
          val arr = new Array[Double](bLabels.length * cLabels.length);
          var i = 0;
          for { bi <- 0 until bLabels.length } {
            val b = bLabels(bi)
            for { ci <- 0 until cLabels.length } {
              val c = cLabels(ci)
              arr(i) = ( grammar.ruleScore(a,b,c)
                + bScores(bi)
                + cScores(ci)
                + scorer.scoreBinaryRule(begin,split,end,grammar.ruleIndex(a,b,c)) + scorer.scoreSpan(begin,end,a)
                )
              i += 1;
            }
          };
          aScores(ai) = logSum(arr)
          if(!aScores(ai).isInfinite) foundOne = true;
          else println("wtf!!!!")
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

  def expectedCounts[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], scorer: SpanScorer[L] = SpanScorer.identity[L]) = {
    val ruleCounts = grammar.mkDenseVector()
    val wordCounts = grammar.fillSparseArrayMap(Counter[W,Double]());

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
        for {
          (p,opScore) <- aLabels zip aScores
          (c,icScore) <- cLabels zip cScores
        } {
          val ruleScore = opScore + icScore + grammar.ruleScore(p,c) - totalProb;
          val span = scorer.scoreUnaryRule(t.span.start,t.span.end,grammar.ruleIndex(p,c));
          assert(!ruleScore.isNaN);
         // assert(exp(ruleScore) > 0, " " + ruleScore);
          assert(!exp(ruleScore + span).isInfinite,ruleScore + " " + span);
          ruleCounts(grammar.ruleIndex(p,c)) += exp(ruleScore + span);
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
    ExpectedCounts(ruleCounts,wordCounts,totalProb);
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



  def splitCycle[L,W](data: SplittingData[L,W],
                      trees: SplitTreebank[L,W],
                      splitter: L=>Seq[L],
                      randomNoise: Double =0.1,convergence:Double=1E-4,maxIter:Int = 30):
                      (SplittingData[L,W],Double) = {
    val splitData = splitCounts(data,splitter,randomNoise);

    val stateIterator = Iterator.iterate( (splitData,Double.MaxValue,convergence.abs*100,0)) { case state =>
      val (SplittingData(splitRules,splitUnRules, splitWords),lastLogProb,_,it) = state;
      assert(splitRules.sum != 0)
      assert(splitUnRules.sum != 0)
      assert(splitWords.sum != 0)
      val grammar = Grammar(Library.logAndNormalizeRows(splitRules), Library.logAndNormalizeRows(splitUnRules));
      val lexicon = new UnsmoothedLexicon(Library.logAndNormalizeRows(splitWords));
      val accum = trees.par.view.map {case TreeInstance(_,t,s,_) =>  expectedCounts[L,W](grammar,lexicon,t,s)}.reduce((_:ExpectedCounts[W]) += (_:ExpectedCounts[W]) )
      val logProb = accum.logProb;

      val ((finalRules,finalUnaries),finalWords) = accum.decode(grammar);

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (SplittingData(finalRules,finalUnaries,finalWords),logProb,improvement,it+1);
    }
    stateIterator.drop(10).dropWhile(x => x._3 > convergence && (x._4 < maxIter || maxIter < 0)).map { case (data,logP,_,_) => (data,logP) }.next()
  }

  // returns the labels whose refinements should be merged
  def determineLabelsToUnsplit[L,W](data: SplittingData[L,W],
                      splitData: SplittingData[L,W],
                      trees: SplitTreebank[L,W],
                      splitter: L=>Seq[L],
                      percentMerging: Double = 0.1) = {
    val SplittingData(splitRules,splitUnRules, splitWords) = splitData;
    val SplittingData(coarseRules,coarseUnRules, coarseWords) = data
    val grammar = Grammar(Library.logAndNormalizeRows(splitRules), Library.logAndNormalizeRows(splitUnRules));
    val lexicon = new UnsmoothedLexicon(Library.logAndNormalizeRows(splitWords));
    val coarseGrammar = Grammar(coarseRules,coarseUnRules)
    val proj = ProjectionIndexer.fromSplitter(coarseGrammar.labelIndex, grammar.labelIndex, splitter)

    val calibratedTrees = trees.par.map { case TreeInstance(_,t,s,_) => calibrateTree(grammar,lexicon,t,s)}
    val symbolCounts = calibratedTrees.map(expectedSymbolCounts(grammar,_)).reduceLeft{ _ += _ }
    val symbolTrials = {
      DenseVector.tabulate(symbolCounts.length){f =>
        val sibs = proj.refinements(proj.project(f))
        sibs.map(symbolCounts).sum
      }
    }
    assert(!symbolTrials.valuesIterator.exists(_ == 0.0))
    val symbolProbs = (symbolCounts :/ symbolTrials).map(math.log)
    assert(!symbolProbs.valuesIterator.exists(_.isNaN))
    assert(!symbolProbs.valuesIterator.exists(_.isInfinite), symbolProbs.pairsIterator.toIndexedSeq.mkString("\n"))
    println(Encoder.fromIndex(proj.fineIndex).decode(symbolProbs))
    val mergeCosts = calibratedTrees.map(computeMergeCosts(proj,symbolProbs,_)).reduce(_ += _)
    println(proj.coarseEncoder.decode(mergeCosts))
    val topK = (0 until proj.coarseIndex.size).sortBy(i => -mergeCosts(i)).take(percentMerging * proj.coarseIndex.size toInt)
    println("Will merge:")
    for(i <- topK) {
      println(i + ": " + splitter(proj.coarseIndex.get(i)))
    }
    topK.map(proj.coarseIndex.get).toSet
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
                        convergence: Double = 1E-4): Iterator[(SplittingData[(L,Int),W],Double)] = {
    def split(s: (L,Int) ) = {
      val (l,state) = s;
      if (l == root) Seq(s) else Seq( (l,state*2),(l,state*2+1));
    }

    def proj(s: (L,Int))= s._1

    val myTrees = trees.map { case TreeInstance(id,tree,sent, _) => TreeInstance(id,tree.map { (_,0) },sent) }
    val SplittingData(binaries,unaries,wordCounts) = data;

    // turn every label l into (l,0)
    val myRuleCounts = Counter2[(L,Int),BinaryRule[(L,Int)],Double]();
    for ( (l,rule,w) <- binaries.triples) rule match {
      case BinaryRule(par,lc,rc) =>
        myRuleCounts( (l,0), BinaryRule( (l,0), (lc,0), (rc,0))) = w;
    }
    val myUnaries = Counter2[(L,Int),UnaryRule[(L,Int)],Double]();
    for ( (l,rule,w) <- unaries.triples) rule match {
      case UnaryRule(par,child) =>
      myUnaries( (l,0), UnaryRule( (l,0), (child,0))) = w;
    }

    val myWordCounts = Counter2[(L,Int),W,Double]();
    for ( ((l,word),w) <- wordCounts.nonzero.pairs) {
      myWordCounts( (l,0), word) = w;
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

    val initState = (initData,initData,Double.NegativeInfinity, initProjections)
    val iterations = Iterator.iterate( initState ) { state =>
      val (_,data,logProb,oldProjections) = state;
      def split(sym: L) = oldProjections.refinementsOf(sym).flatMap(splitter)
      val myTrees = unsplitTrees.map { splitHelper(_, _.map(split)) }.toIndexedSeq;
      val (newData,newLP) = oneCycle(data,myTrees);

      val labelsToUnsplit = this.determineLabelsToUnsplit(data,newData,myTrees,splitter)

      val splitProjections = ProjectionIndexer.fromSplitter(oldProjections.fineIndex, splitter)
      def conditionalSplit(c: L) = if(labelsToUnsplit(c)) splitProjections.refinementsOf(c).take(1) else splitProjections.refinementsOf(c)

      val mergedProjections = ProjectionIndexer.fromSplitter(oldProjections.fineIndex, conditionalSplit _)

      def project(fine: L) = {
        val r = if(mergedProjections.fineIndex.contains(fine)) fine
        else splitProjections.refinementsOf(splitProjections.project(fine)).head
        assert(mergedProjections.fineIndex.contains(r))
        r
      }
      val merged = this.mergeRules(newData, project _)
      (newData,merged,newLP,oldProjections compose mergedProjections);
    }

    (for( (data,merged,logProb,_) <- iterations; d2 <- Seq(data,merged)) yield d2 -> logProb).drop(1)

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
      val builder = CKYChartBuilder(("",0),lex,grammar);
      def proj(l: (String,Int)) = l._1;
      val parser = ProjectingParser(builder,coarseGrammar,proj);
      iter += 1
      val name = if(iter == 1) {
        "base"
      } else if(iter % 2 == 0) {
        "split-" + (iter / 2)
      } else {
        "merge-" + (iter / 2)
      }
      (name,parser);
    }
  }
}