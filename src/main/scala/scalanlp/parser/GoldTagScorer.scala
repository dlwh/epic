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
  import ParserTrainer._;
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val params = config.readIn[ParserTrainerParams]("treebank");
    val parserFile = config.readIn[File]("parser.test");

    import params.treebank._;
    import params.spans._;

    val trainTreesWithUnaries = transformTrees(treebank.trainTrees, trainSpans, maxLength, binarize, xform);
    val (trainTrees,replacer) = removeUnaryChains(trainTreesWithUnaries);

    val parser = readObject[ChartParser[String,_,String]](parserFile);

    val testTrees = transformTrees(treebank.testTrees, testSpans, maxLength, binarize, xform)

    val goldenTrees = for ( (tree,words,span) <- testTrees) yield {
      (tree,words,SpanScorer.sum(new GoldTagScorer(getTags(parser.projections.coarseIndex, tree), -90),span))
    };

    evalParser(goldenTrees,parser,"test",replacer);
  }

  def getTags[L](index: Index[L], tree: Tree[L]) = {
    val tags = tree.leaves.map(_.label).map(index).toIndexedSeq;
    tags
  }

  protected def evalParser(testTrees: IndexedSeq[(Tree[String],Seq[String],SpanScorer[String])],
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]) = {
    println("Evaluating Parser...");
    val stats = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    import stats._;
    println("Eval finished. Results:");
    println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tagging: " + stats.tagAccuracy);
    f1
  }
}