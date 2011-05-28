package scalanlp.parser;
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


import scalanlp.trees._
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.config.Configuration;
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import scala.actors.Actor
import scalanlp.concurrent.ParallelOps._;
import scalanlp.util._;


/**
* Hack approximation to true parse eval. Gives Labeled Precision
* and Labeled Recall.
*
* @author dlwh
*/
class ParseEval[L](ignoredLabels: Set[L]) {
  import ParseEval.Statistics;
  /**
  * Computes precision, recall, and exact match for the each
  * guess/gold pair of trees.
  */
  def apply(guessgold: Iterator[(Tree[L],Tree[L])]):Statistics = {
    val allStats = for( (guess,gold) <- guessgold) yield { apply(guess,gold) }

    val stats = allStats.reduceLeft(_ + _);

    stats
  }

  def apply(guess: Tree[L], gold: Tree[L]): Statistics = {
    val guessSet = labeledConstituents(guess);
    val goldSet = labeledConstituents(gold);
    val inter = (guessSet intersect goldSet)
    val exact = if(goldSet.size == inter.size && guessSet.size == inter.size) 1 else 0;
    val guessLeaves = guess.leaves;
    val goldLeaves = gold.leaves;
    val numRight = goldLeaves.zip(guessLeaves).foldLeft(0) { (acc,gg) => if(gg._1.label == gg._2.label) acc + 1 else acc};
    Statistics(guessSet.size, goldSet.size, inter.size, exact, numRight, guess.span.end, 1);
  }


  private def labeledConstituents(tree: Tree[L]) = Set() ++ {
    for(child <- tree.preorder
        if !ignoredLabels.contains(child.label) && !child.isLeaf)
        yield (child.label,child.span);
  }
}

object ParseEval {

  case class Statistics(guess: Int, gold: Int, right: Int, numExact: Int,
                        tagsRight: Int, numWords: Int,
                        numParses: Int) {
    def +(stats: Statistics) = {
      Statistics(guess + stats.guess,
        gold + stats.gold,
        right + stats.right,
        numExact + stats.numExact,
        tagsRight + stats.tagsRight,
        numWords + stats.numWords,
        numParses + stats.numParses)
    }

    def precision = if(guess == 0) 1.0 else (right * 1.0 / guess);
    def recall = if(guess == 0) 1.0 else (right * 1.0 / gold);
    def exact = (numExact * 1.0 / numParses);
    def tagAccuracy = tagsRight * 1.0 / numWords;
    def f1 = (2 * precision * recall)/(precision + recall);

    override def toString() = {
      "Statistics(precision=" + precision +
        ", recall=" + recall + ", f1=" + f1 + ", exact=" + exact + ", tagAccuracy=" + tagAccuracy +")";
    }
  }

  type PostParseFn = (Tree[String],Tree[String],Seq[String],Statistics,Int)=>Unit;
  val noPostParseFn = (_:Tree[String],_:Tree[String],_:Seq[String],_:Statistics,_:Int)=>()

  def evaluate(trees: IndexedSeq[TreeInstance[String,String]],
               parser: Parser[String,String], chainReplacer: ChainReplacer[String],
               postEval: PostParseFn = noPostParseFn) = {

    val peval = new ParseEval(Set("","''", "``", ".", ":", ","));

    def evalSentence(sent: TreeInstance[String,String]) = try {
      val TreeInstance(id,goldTree,words,scorer) = sent;
      val startTime = System.currentTimeMillis;
      val guessTree = Trees.debinarize(chainReplacer.replaceUnaries(Trees.deannotate(parser.bestParse(words,scorer))));
      val deBgold = Trees.debinarize(Trees.deannotate(goldTree));
      val stats = peval(guessTree,deBgold);
      val endTime = System.currentTimeMillis;
      postEval(guessTree,deBgold,words,stats,(endTime-startTime).toInt);
      stats;
    } catch {
      case e:RuntimeException => e.printStackTrace();
      Statistics(0, peval.labeledConstituents(sent.tree).size, 0, 0, 0, sent.words.length, 1);
    }
    val stats = trees.par.withSequentialThreshold(100).mapReduce({ evalSentence _ },{ (_:Statistics) + (_:Statistics)});

    stats
  }

  def evaluateAndLog(trees: IndexedSeq[TreeInstance[String,String]],
                     parser: Parser[String,String], evalDir: String, chainReplacer: ChainReplacer[String]) = {

    val parsedir = new File(evalDir);
    parsedir.exists() || parsedir.mkdirs() || error("Couldn't make directory: " + parsedir);
    val goldOut = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File(parsedir,"gold"))));
    val guessOut = new PrintStream(new BufferedOutputStream(new FileOutputStream(new File(parsedir,"guess"))));

    import Actor._;
    val appender = actor {
      loop {
        react {
          case Some((guess,gold)) =>  goldOut.println(gold); guessOut.println(guess);
          case None => goldOut.close(); guessOut.close(); Actor.exit();
        }
      }
    }

    def postEval(guessTree: Tree[String], goldTree: Tree[String], words: Seq[String],
                 stats: Statistics, time: Int) = {
      val buf = new StringBuilder();
      buf ++= "======\n";
      buf ++= words.mkString(",");
      buf ++= "\nGold:\n";
      buf ++= goldTree.render(words);
      buf ++= "\nGuess:\n";
      buf ++= guessTree.render(words);
      buf ++= ("\nLocal Accuracy:" + (stats.precision,stats.recall,stats.f1,stats.exact,stats.tagAccuracy) + "\n") ;
      buf ++= (time / 1000.0 + " Seconds")
      buf ++= "\n======";
      println(buf.toString);
      appender ! Some((guessTree.render(words,newline=false)), goldTree.render(words,newline=false));
    }

    val r = evaluate(trees,parser,chainReplacer, postEval _);
    appender ! None;
    r
  }
}

/*
object ParserTester {
  import ParserTrainer._;
  def main(args: Array[String]) {
    val config = Configuration.fromPropertiesFiles(args.map(new File(_)));
    val params = config.readIn[ParserTrainerParams]("treebank");
    val parserFile = config.readIn[File]("parser.test");

    import params.treebank._;
    import params.spans._;

    val trainTreesWithUnaries = transformTrees(treebank.trainTrees, trainSpans, maxLength, binarize, xform, verticalMarkovization, horizontalMarkovization);
    val (trainTrees,replacer) = removeUnaryChains(trainTreesWithUnaries);

    val parser = readObject[Parser[String,String]](parserFile);

    val testTrees = transformTrees(treebank.testTrees, testSpans, maxLength, binarize, xform, verticalMarkovization, horizontalMarkovization)

    evalParser(testTrees,parser,"test",replacer);
  }

  protected def evalParser(testTrees:
          parser: Parser[String,String], name: String, chainReplacer: ChainReplacer[String]) = {
    println("Evaluating Parser...");
    val stats = ParseEval.evaluateAndLog(testTrees,parser,name,chainReplacer);
    import stats._;
    println("Eval finished. Results:");
    println( "P: " + precision + " R:" + recall + " F1: " + f1 +  " Ex:" + exact + " Tagging: " + stats.tagAccuracy);
    f1
  }
}
*/