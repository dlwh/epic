package scalanlp.parser

import java.io.File
import scalanlp.trees._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
;
import scalanlp.config.Configuration
import scalanlp.util._;

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1)
@serializable
class GoldTagScorer[L](tags: Seq[Int], wrongTag: Double=Double.NegativeInfinity) extends SpanScorer[L] {
  def scoreLexical(begin: Int, end: Int, tag: Int) = if(tags(begin) == tag) 0.0 else wrongTag;

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = 0.0;

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = 0.0
}

object GoldTagExperiment {
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val params = config.readIn[ProcessedTreebank]("treebank");
    val parserFile = config.readIn[File]("parser.test");
    import params._;


    val parser = readObject[ChartParser[String,_,String]](parserFile);

    val goldenTrees = for ( TreeInstance(id,tree,words,span) <- testTrees) yield {
      TreeInstance("gold-"+id,tree,words,SpanScorer.sum(new GoldTagScorer(getTags(parser.projections.coarseIndex, tree), -90),span))
    };

    evalParser(goldenTrees,parser,"gold-test",replacer);
  }

  def getTags[L](index: Index[L], tree: Tree[L]) = {
    val tags = tree.leaves.map(_.label).map(index).toIndexedSeq;
    tags
  }

  protected def evalParser(testTrees: IndexedSeq[TreeInstance[String,String]],
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]) = {
    println("Evaluating Parser...");
    val stats = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    import stats._;
    println("Eval finished. Results:");
    println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tagging: " + stats.tagAccuracy);
    f1
  }
}