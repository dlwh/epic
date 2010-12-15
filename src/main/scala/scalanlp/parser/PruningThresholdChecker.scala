package scalanlp.parser

import scalanlp.trees._
import scalanlp.trees.UnaryChainRemover.ChainReplacer;

import java.io._
import scalanlp.concurrent.ParallelOps._
import scalanlp.trees.DenseTreebank

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
class PruningThresholdChecker[C,L,W](parser: ChartBuilder[ParseChart.LogProbabilityParseChart,L,W]) {

  def minimumThreshold(s: Seq[W], t: BinarizedTree[L], scorer: SpanScorer = SpanScorer.identity) = {
    val coarseRootIndex = parser.grammar.index(parser.root);
    val inside = parser.buildInsideChart(s, scorer)
    val outside = parser.buildOutsideChart(inside, scorer);

    val sentProb = inside(0,s.length,coarseRootIndex);
    if(sentProb.isInfinite) {
      error("Couldn't parse " + s + " " + sentProb)
    }

    val minScore = checkTree(s, t, inside,outside,sentProb);

    minScore
  }

  def checkTree(w: Seq[W], tree: BinarizedTree[L], inside: ParseChart[L], outside: ParseChart[L], sentProb: Double) = {
    tree.allChildren.foldLeft(Double.PositiveInfinity) { (minScore,t) =>
      val begin = t.span.start;
      val end = t.span.end;
      val l = t.label;
      val myScore = inside.labelScore(begin, end, l) + outside.labelScore(begin, end, l) - sentProb
      if(myScore < -10) {
        println("rawr!" + l + tree.render(w) + " " +  t.render(w));
      }
      if(myScore < minScore) myScore else minScore
    }
  }

}

object CheckThresholds {
  def main(args: Array[String]) {
    val parser = loadParser(new File(args(0)));
    val treebank = DenseTreebank.fromZipFile(new File(args(1)));

    val (trainTrees, replacer) = transformTrees(treebank.trainTrees,40, Trees.xBarBinarize(_), Trees.Transforms.StandardStringTransform);

    val thresh = new PruningThresholdChecker(parser.builder.withCharts(ParseChart.logProb));
    trainTrees.par.foreach { case (t,w) =>
      println(w.length, thresh.minimumThreshold(w,t));
    }
  }

  def loadParser(loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)));
    val parser = oin.readObject().asInstanceOf[ChartParser[String,String,String]]
    oin.close();
    parser;
  }

  def transformTrees(trees: Iterator[(Tree[String],Seq[String])],
                     maxLength: Int,
                     binarize: (Tree[String]) => BinarizedTree[String],
                     xform: Tree[String]=>Tree[String]): (IndexedSeq[(BinarizedTree[String], Seq[String])],ChainReplacer[String]) = {

      val binarizedAndTransformed = (for {
        (tree, words) <- (trees) if words.length <= maxLength
      } yield (binarize(xform(tree)),words)).toIndexedSeq

      val chainRemover = new UnaryChainRemover[String];

      val (dechained,chainReplacer) = chainRemover.removeUnaryChains(binarizedAndTransformed.iterator);

      (dechained, chainReplacer)
    }


} */

