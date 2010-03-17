package scalanlp.parser

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.counters.LogCounters
import scalanlp.math.Numerics.logSum;
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.Treebank
import scalanlp.trees.Trees
import scalanlp.trees.UnaryTree
import scalanlp.util.IntBloomFilter
import scalanlp.counters.Counters._;
import Math.exp

object StateSplitting {

  def insideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W]) = {
    val score = Array.fill(s.length+1, s.length + 1)(grammar.mkVector(Double.NegativeInfinity));
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

  def outsideScores[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W], tree: BinarizedTree[Seq[L]], s: Seq[W], insideScores: Array[Array[Vector]]) = {
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };
    val score = Array.fill(s.length+1, s.length + 1)(grammar.mkVector(Double.NegativeInfinity));
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
    val ruleCounts = LogCounters.LogPairedDoubleCounter[L,Rule[L]]();
    val wordCounts = LogCounters.LogPairedDoubleCounter[L,W]();

    val iScores = insideScores(grammar,lexicon,tree,s);
    val oScores = outsideScores(grammar,lexicon, tree,s,iScores);
    // normalizer
    val totalProb = logSum(iScores(0)(s.length)(tree.label map grammar.index));
    println("Total Prob" + totalProb);
    assert(!totalProb.isInfinite);
    println("Expected Counts");

    tree.allChildren foreach {
      case t: NullaryTree[L] =>
        for( l <- t.label) {
          val iLabel = grammar.index(l);
          val iS = iScores(t.span.start)(t.span.end)(iLabel);
          val oS = oScores(t.span.start)(t.span.end)(iLabel);
          val ruleScore = (iS + oS - totalProb);
          assert(!ruleScore.isNaN);
          assert(!ruleScore.isInfinite);
          wordCounts(l,s(t.span.start)) = logSum(wordCounts(l,s(t.span.start)), ruleScore);
        }
      case t@UnaryTree(_,child) =>
        for {
          p <- t.label.iterator;
          pIndex = grammar.index(p);
          opScore = oScores(t.span.start)(t.span.end)(pIndex);
          c <- child.label.iterator
          cIndex = grammar.index(c);
          icScore = iScores(child.span.start)(child.span.end)(cIndex)
        } {
          val rule = UnaryRule(p,c);
          val ruleScore = opScore + icScore + grammar.unaryRulesByIndexedChild(cIndex)(pIndex) - totalProb;
          assert(!ruleScore.isNaN);
          assert(!ruleScore.isInfinite);
          ruleCounts(rule.parent,rule) += logSum(ruleCounts(rule.parent,rule), ruleScore);
        }
      case t@ BinaryTree(_,lc,rc) =>
        for {
          p <- t.label iterator;
          pIndex = grammar.index(p);
          opScore = oScores(t.span.start)(t.span.end)(pIndex);
          l <- lc.label.iterator
          lIndex = grammar.index(l);
          val lRules = grammar.binaryRulesByIndexedLeftChild(lIndex);
          ilScore = iScores(lc.span.start)(lc.span.end)(lIndex);
          r <- rc.label.iterator
        } {
          val rIndex = grammar.index(r);
          val irScore = iScores(rc.span.start)(rc.span.end)(rIndex)
          val rule = BinaryRule(p,l,r);
          val ruleScore = opScore + irScore + ilScore + lRules(rIndex)(pIndex) - totalProb;
          ruleCounts(rule.parent,rule) = logSum(ruleCounts(rule.parent,rule), ruleScore);
        }
    }
    (ruleCounts,wordCounts,totalProb);
  }

  def splitCounts[L,L2,W](ruleCounts: RuleCounts[L],
                          wordCounts: TagWordCounts[L,W],
                          splitter: L=>Seq[L2], randomNoise: Double = 0.01) = {

    val splitRules = LogCounters.LogPairedDoubleCounter[L2,Rule[L2]]();
    for {
      ((_,baseR),count) <- ruleCounts;
      r <- splitRule(baseR,splitter);
      x = randomNoise * 2 * Math.random - randomNoise
    } {
      splitRules(r.parent,r) = count + x;
    }

    val splitWords = LogCounters.LogPairedDoubleCounter[L2,W]();
    for {
      ((l,w),count) <- wordCounts
      r <- splitter(l);
      x = randomNoise * 2 * Math.random - randomNoise
    } {
      splitWords(r,w) = count + x;
    }

    (splitRules,splitWords);
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
      val lexicon = new UnsmoothedLexicon(splitWords);
      val results = for {
        (t,s) <- trees.iterator
      } yield expectedCounts(grammar,lexicon,t,s);

      val (finalCounts, finalWords, logProb) = results.reduceLeft { (counts1,counts2) =>
        val (ruleCounts1,wCounts1,tProb1) = counts1;
        val (ruleCounts2,wCounts2,tProb2) = counts2;
        import LogCounters.logSum;
        (logSum(ruleCounts1,ruleCounts2), logSum(wCounts1,wCounts2), tProb1 + tProb2 );
      }
      println(finalWords);
      for( (r,ctr) <- finalWords.rows) {
        println(r,ctr.maxk(30));
      }
      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (finalCounts,finalWords,logProb,improvement);
    }
    stateIterator.dropWhile(_._4 > convergence).map { case (sR,sW,logP,_) => (sR,sW,logP) }.next
  }

  type RuleCounts[L] = LogCounters.LogPairedDoubleCounter[L,Rule[L]];
  type TagWordCounts[L,W] = LogCounters.LogPairedDoubleCounter[L,W];
  type Treebank[L,W] = Seq[(BinarizedTree[L],Seq[W])];
  type SplitTreebank[L,W] = Seq[(BinarizedTree[Seq[L]],Seq[W])];

  def splitGrammar[L,W](ruleCounts: RuleCounts[L],
                      wordCounts: TagWordCounts[L,W],
                      unsplitTrees: Treebank[L,W],
                      splitter: L=>Seq[L], nSplits: Int,
                      randomNoise: Double,  convergence: Double): (RuleCounts[L],TagWordCounts[L,W],Double)  = {
    val oneCycle = splitCycle(_:RuleCounts[L],_:TagWordCounts[L,W],_:SplitTreebank[L,W],splitter,randomNoise,convergence);

    def splitTrees(trees: Treebank[L,W]):SplitTreebank[L,W] = for { (tree,sent) <- trees } yield (tree.map(splitter),sent)
    val initialTrees: SplitTreebank[L,W]= splitTrees(unsplitTrees);

    def resplitTrees(trees: SplitTreebank[L,W]) =for { (tree,sent) <- trees } yield (tree.map(_ flatMap splitter),sent)

    val (state,trees) = Iterator.iterate( (oneCycle(ruleCounts,wordCounts,initialTrees),initialTrees) ) { state =>
      val ((ruleCounts,wordCounts,logProb),trees) = state;
      println("LP: " + logProb);
      val split = resplitTrees(trees)
      val nextState = oneCycle(ruleCounts,wordCounts,split);
      (nextState,split);
    } drop (nSplits-1) next

    //val (rules,wordCounts,logProb) = state;
    state;
  }

  def splitGrammar(ruleCounts: RuleCounts[String],
                   wordCounts: TagWordCounts[String,String],
                   trees: Treebank[String,String],
                   nSplits:Int =3,
                   randomNoise: Double=0.1,
                   convergence: Double = 1E-4): (RuleCounts[String],TagWordCounts[String,String],Double) = {
    def split(s: String) = if (s.isEmpty) Seq(s) else if(!s.contains("**")) Seq(s+"**0",s+"**1") else Seq(s+"0",s+"1")
    splitGrammar(ruleCounts,wordCounts,trees,split _, nSplits, randomNoise, convergence);
  }

}

object StateSplittingTest {
  import java.io.File;
 // import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    println("Loading Treebank");
    val trees = (for( (tree,words) <- treebank.trainTrees)
      yield (Trees.xBarBinarize(xform(tree)),words map (_.intern))).take(2000) toSeq;
    println("Extracting counts");
    val (initialLex,initialProductions) = (
      GenerativeParser.extractCounts(trees.iterator,Trees.xBarBinarize _)
    );
    println("Splitting Grammar");
    val (finalLex,finalProd,logProb) = StateSplitting.splitGrammar(LogCounters.log(initialProductions),LogCounters.log(initialLex),trees);
    for( (r,ctr) <- finalLex.rows) {
      println(r,ctr.maxk(30));
    }
  }

}