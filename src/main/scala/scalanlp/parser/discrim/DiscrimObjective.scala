package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalala.tensor.sparse.SparseVector
import scalanlp.optimize.DiffFunction
import scalanlp.trees._;
import InsideOutside._;
import ParseChart.LogProbabilityParseChart;
import scalanlp.concurrent.ParallelOps._;
import scalanlp.concurrent.ThreadLocal;
import scalala.Scalala._;
import scalala.tensor.counters.Counters._

/**
 * 
 * @author dlwh
 */
class DiscrimObjective[L,W](feat: Featurizer[L,W],
                            root: L,
                            trees: IndexedSeq[(BinarizedTree[L],Seq[W])]) extends DiffFunction[Int,DenseVector] {
  def calculate(weights: DenseVector) = {

    val grammar = weightsToGrammar(weights);
    val lexicon = weightsToLexicon(weights);
    val parser = new CKYParser[LogProbabilityParseChart,L,W](root,lexicon,grammar,ParseChart.logProb);
    val ecounts = indexedTrees.par.fold(new ExpectedCounts[W](grammar)) { (counts, treewords) =>
      val tree = treewords._1;
      val words = treewords._2;

      val treeCounts = treeToExpectedCounts(grammar,lexicon,tree,words);
      val wordCounts = wordsToExpectedCounts(words, parser);
      counts += treeCounts -= wordCounts;

    } { (ecounts1, ecounts2) =>
      ecounts1 += ecounts2
    }

    val grad = -expectedCountsToFeatureVector(ecounts)

    (ecounts.logProb, grad value);
  }

  val indexedFeatures: FeatureIndexer[L,W] = FeatureIndexer(feat,trees);
  val indexedTrees = for( (t,w) <- trees) yield (t.map(indexedFeatures.labelIndex),w);

  val openTags = Set.empty ++ {
    val lexicon = new PairedDoubleCounter[L,W]();
    val tags = collection.mutable.Set[L]();
    for{
      (tree,words) <- trees
      leaves = tree.leaves map (l => (l,words(l.span.start)));
      (l,w) <- leaves
    }{
      tags += l.label;
      lexicon(l.label).incrementCount(w,1);
    }
    for( t <- tags if lexicon(t).size > 50)  yield t;
  }

  def weightsToLexicon(weights: DenseVector) = {
    val grammar = new FeaturizedLexicon(openTags, weights, indexedFeatures);
    grammar;
  }

  def weightsToGrammar(weights: DenseVector):Grammar[L] = {
    val grammar =  new FeaturizedGrammar(weights,indexedFeatures)
    grammar;
  }

  def wordsToExpectedCounts(words: Seq[W], parser: ChartParser[LogProbabilityParseChart,L,W]) = {
    val ecounts = new InsideOutside(parser).expectedCounts(words);
    ecounts
  }

  // these expected counts are in normal space, not log space.
  private def treeToExpectedCounts(g: Grammar[L],
                                   lexicon: Lexicon[L,W],
                                   t: BinarizedTree[Int],
                                   words: Seq[W]):ExpectedCounts[W] = {
    val expectedCounts = new ExpectedCounts[W](g)
    var score = 0.0;
    for(t2 <- t.allChildren) {
      t2 match {
        case BinaryTree(a,Tree(b,_),Tree(c,_)) =>
          expectedCounts.binaryRuleCounts(a)(b)(c) += 1
          score += g.binaryRuleScore(a,b,c);
        case UnaryTree(a,Tree(b,_)) =>
          expectedCounts.unaryRuleCounts(a)(b) += 1
          score += g.unaryRuleScore(a,b);
        case n@NullaryTree(a) =>
          val w = words(n.span.start);
          expectedCounts.wordCounts(a)(w) += 1
          score += lexicon.wordScore(g.index.get(a), w);
      }
    }
    expectedCounts.logProb = score;
    expectedCounts;
  }

  def expectedCountsToFeatureVector(ecounts: ExpectedCounts[W]):DenseVector = {
    val result = indexedFeatures.mkDenseVector(0.0);

    // binaries
    for( (a,bvec) <- ecounts.binaryRuleCounts;
         (b,cvec) <- bvec;
         (c,v) <- cvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b,c) * v);
    }

    // unaries
    for( (a,bvec) <- ecounts.unaryRuleCounts;
         (b,v) <- bvec.activeElements) {
      result += (indexedFeatures.featuresFor(a,b) * v);
    }

    // lex
    for( (a,ctr) <- ecounts.wordCounts;
         (w,v) <- ctr) {
      result += (indexedFeatures.featuresFor(a,w) * v);
    }

    result;
  }


}