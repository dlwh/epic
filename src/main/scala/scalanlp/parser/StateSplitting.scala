package scalanlp.parser

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.math.Numerics.logSum;
import scalanlp.parser.GenerativeParser.Lexicon
import scalanlp.trees.BinarizedTree
import scalanlp.trees.BinaryTree
import scalanlp.trees.NullaryTree
import scalanlp.trees.Tree
import scalanlp.trees.UnaryTree
import scalanlp.util.IntBloomFilter

class StateSplitting[L,W](grammar: Grammar[L], lexicon: Lexicon[L,W]) {

  def insideScores(tree: BinarizedTree[Seq[L]], s: Seq[W]) = {
    val score = Array.fill(s.length+1, s.length + 1)(grammar.mkVector(Double.NegativeInfinity));
    val active = Array.fill(s.length+1,s.length+1)(new IntBloomFilter(grammar.index.size,3));
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };

    indexedTree.postorder.foreach {
      case t : NullaryTree[Seq[Int]] =>
        // fill in POS tags:
        assert(t.span.length == 1);
        val word = s(t.span.first);
        for( a <- t.label.iterator;
            wScore = lexicon.wordScore(grammar.index.get(a),word)
            if !wScore.isInfinite) {
          score(t.span.first)(t.span.first+1)(a) = wScore;
          active(t.span.start)(t.span.end) += a;
        }
      case t : UnaryTree[Seq[Int]] =>
        val spanScore = score(t.span.start)(t.span.end);
        for {
          // Need to create a new Seq so that we can modify score(t.span.start)(t.span.end) concurrently
          (c,cScore) <- Seq.empty ++ spanScore.activeElements
          rules = grammar.unaryRulesByIndexedChild(c);
          a <- t.label
        } {
          val ruleScore = rules(a) + cScore;
          spanScore(a) = Numerics.logSum( spanScore(a), ruleScore);
          active(t.span.start)(t.span.end) += a;
        }
      case t : BinaryTree[Seq[Int]] =>
        val spanScore = score(t.span.start)(t.span.end);
        for {
          // Need to create a new Seq so that we can modify score(t.span.start)(t.span.end) concurrently
          (b,bScore) <- score(t.span.start)(t.span.end).activeElements;
          rules = grammar.binaryRulesByIndexedLeftChild(b);
          (c,cScore) <- score(t.span.start)(t.span.end).activeElements;
          a <- t.label
        } {
          val ruleScore = rules(c)(a) + bScore + cScore;
          spanScore(a) = Numerics.logSum( spanScore(a), ruleScore);
          active(t.span.start)(t.span.end) += a;
        }
      case _ => error("bad tree!");
    }

    score;
  }

  def outsideScores(tree: BinarizedTree[Seq[L]], s: Seq[W], insideScores: Seq[Seq[Vector]]) = {
    val indexedTree:Tree[Seq[Int]] = tree.map{ _.map(grammar.index) };
    val score = Array.fill(s.length+1, s.length + 1)(grammar.mkVector(Double.NegativeInfinity));
    val active = Array.fill(s.length+1,s.length+1)(new IntBloomFilter(grammar.index.size,3));
    score(0)(s.length)(indexedTree.label.head) = 0.0;

    // Set the outside score of each child
    indexedTree.preorder.foreach {
      case tree: NullaryTree[Seq[Int]] => () // do nothing
      case t @ UnaryTree(_,child) =>
        for {
          p <- t.label
          pScore = score(child.span.start)(child.span.end)(p);
          c <- child.label
        } {
          score(child.span.start)(child.span.end)(c) = logSum(score(child.span.start)(child.span.end)(c),pScore);
        }
      case t @ UnaryTree(_,child) =>
        for {
          p <- t.label
          pScore = score(child.span.start)(child.span.end)(p);
          c <- child.label
        } {
          score(child.span.start)(child.span.end)(c) = logSum(score(child.span.start)(child.span.end)(c),pScore);
        }
      case t @ BinaryTree(_,lchild,rchild) =>
        for {
          p <- t.label.iterator
          pScore = score(t.span.start)(t.span.end)(p);
          l <- lchild.label.iterator
          lScore = score(lchild.span.start)(lchild.span.end)(l);
          r <- rchild.label.iterator
          rScore = score(rchild.span.start)(rchild.span.end)(r);
        } {
          score(lchild.span.start)(lchild.span.end)(l) = logSum(score(lchild.span.start)(lchild.span.end)(l),pScore + rScore);
          score(rchild.span.start)(rchild.span.end)(r) = logSum(score(rchild.span.start)(rchild.span.end)(r),pScore + lScore);
        }
    }

    score
  }

  def expectedCounts(tree: Tree[Seq[L]], s: Seq[W]) = {
    val ruleCounts = LogDoubleCounter[Rule[L]]();
    val wordCounts = LogPairedDoubleCounter[L,W]();

    val iScores = insideScores(tree,s);
    val oScores = outsideScores(tree,s,iScores);
    // normalizer
    val totalProb = iScores(0)(s.length)(grammar.get(tree.labels.apply(0)));

    tree.allChildren foreach {
      case t: NullaryTree =>
        for( l <- t.label) {
          val iLabel = grammar.get(l);
          val iS = iScores(t.span.start)(t.span.end)(iLabel);
          val oS = oScores(t.span.start)(t.span.end)(iLabel);
          wordCounts(l,w) = logSum(wordCounts(l,w),is + os - totalProb);
        }
      case t: UnaryTree(_,child) =>
        for {
          p <- t.label iterator
          pIndex = grammar.index(p);
          opScore = oScores(t.span.start)(t.span.end)(pIndex);
          c <- child.label.iterator
          cIndex = grammar.index(c);
          icScore = iScores(child.span.start)(child.span.end)(cIndex);
        } {
          val rule = UnaryRule(p,c);
          val ruleScore = opScore + icScore + grammar.unaryRulesByIndexedChild(cIndex)(pIndex) - totalProb;
          ruleCounts(rule) = logSum(ruleCounts(rule),ruleScore);
        }
      case t: BinaryTree(_,lc,rc) =>
        for {
          p <- t.label iterator
          pIndex = grammar.index(p);
          opScore = oScores(t.span.start)(t.span.end)(pIndex);
          l <- lc.label.iterator
          lIndex = grammar.index(l);
          val lRules = grammar.binaryRulesByIndexedLeftChild(lIndex);
          ilScore = iScores(lc.span.start)(lc.span.end)(lIndex);
          r <- rc.label.iterator
          rIndex = grammar.index(r);
          irScore = iScores(rc.span.start)(rc.span.end)(rIndex);
        } {
          val rule = BinaryScore(p,l,r);
          val ruleScore = opScore + irScore + ilScore + lRules(rIndex)(pIndex) - totalProb;
          ruleCounts(rule) = logSum(ruleCounts(rule),ruleScore);
        }
    }
    (ruleCounts,wordCounts);
  }
}
