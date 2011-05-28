package scalanlp.parser

import collection.mutable.ArrayBuffer
import scalanlp.trees._

/**
 *
 * @author dlwh
 */
trait ParserTestHarness {
  def getTrainTreesAndReplacer(binarization:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize(_:Tree[String],false)),
                               maxLength:Int= 15) = {
    val treebank = {
      TstTreebank.treebank;
    }
    val trees = massageTrees(treebank.train.trees,binarization,maxLength);
    removeUnaryChains(trees);
  }

  def getTestTrees(binarization:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize(_:Tree[String],false)),
                   maxLength:Int= 15) = {
    val treebank = {
      TstTreebank.treebank;
    }
    massageTrees(treebank.test.trees,binarization,maxLength);
  }

  def removeUnaryChains(trees: IndexedSeq[TreeInstance[String,String]]) = {
    val chainRemover = new UnaryChainRemover[String];

    val (dechained,chainReplacer) = chainRemover.removeUnaryChains(trees.iterator.map { ti => (ti.tree,ti.words)})

    val dechainedWithSpans = for {
      ((t,w),TreeInstance(id,_,_,span)) <- (dechained zip trees)
    } yield TreeInstance(id,t,w,span);

    (dechainedWithSpans, chainReplacer)
  }

  def massageTrees(trees: Iterator[(Tree[String],Seq[String])],
                   binarize:(Tree[String]=>BinarizedTree[String]) = (Trees.xBarBinarize(_:Tree[String],false)),
                   maxLength:Int=15) = {
    val xform = Trees.Transforms.StandardStringTransform;
    val trainTrees = ArrayBuffer() ++= (for( (tree,words) <- trees.filter(_._2.length <= maxLength))
    yield TreeInstance("",binarize(xform(tree)),words));

    trainTrees
  }


  def evalParser(testTrees: IndexedSeq[TreeInstance[String,String]],parser: Parser[String,String]) = {
    ParseEval.evaluate(testTrees,parser, ParserTestHarness.unaryReplacer);
  }


}

object ParserTestHarness extends ParserTestHarness {
  val ((simpleLexicon,simpleGrammar), unaryReplacer) = {
    val (trees,replacer) = getTrainTreesAndReplacer();
    (GenerativeParser.extractLexiconAndGrammar(trees.iterator),replacer);
  }
  val simpleParser = {
    val chartBuilder = new CKYChartBuilder[ParseChart.ViterbiParseChart, String, String]("", simpleLexicon, simpleGrammar, ParseChart.viterbi)
    ChartParser(chartBuilder);
  }
}