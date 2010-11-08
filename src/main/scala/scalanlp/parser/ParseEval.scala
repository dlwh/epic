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


import scalanlp.trees._;
import java.io.BufferedOutputStream
import java.io.File
import java.io.FileOutputStream
import java.io.PrintStream
import scala.actors.Actor
import scala.collection.mutable.ArrayBuffer
import scalanlp.concurrent.ParallelOps._;


/**
* Hack approximation to true parse eval. Gives Labeled Precision
* and Labeled Recall.
*
* @author dlwh
*/
class ParseEval[L](ignoredLabels: Set[L]) {
  /**
  * Computes precision, recall, and exact match for the each
  * guess/gold pair of trees.
  */
  def apply(guessgold: Iterator[(Tree[L],Tree[L])]):(Double,Double,Double) = {
    val allStats = for( (guess,gold) <- guessgold) yield { apply(guess,gold) }

    val stats = allStats.reduceLeft(_ + _);

    (stats.precision,stats.recall,stats.exact);
  }

  def apply(guess: Tree[L], gold: Tree[L]): Statistics = {
    val guessSet = labeledConstituents(guess);
    val goldSet = labeledConstituents(gold);
    val inter = (guessSet intersect goldSet)
    val exact = if(goldSet.size == inter.size && guessSet.size == inter.size) 1 else 0;
    Statistics(guessSet.size, goldSet.size, inter.size, exact, 1);
  }

  case class Statistics(guess: Int=0, gold: Int=0, right: Int=0, numExact: Int=0, numParses: Int) {
    def +(stats: Statistics) = {
      Statistics(guess + stats.guess,
                 gold + stats.gold,
                 right + stats.right,
                 numExact + stats.numExact,
                 numParses + stats.numParses)
    }

    def precision = (right * 1.0 / guess);
    def recall = (right * 1.0 / gold);
    def exact = (numExact * 1.0 / numParses);
  }

  private def labeledConstituents(tree: Tree[L]) = Set() ++ {
    for(child <- tree.preorder
        if !ignoredLabels.contains(child.label) && !child.isLeaf)
        yield (child.label,child.span);
  }
}

object ParseEval {

  type PostParseFn = (Tree[String],Tree[String],Seq[String],ParseEval[String]#Statistics,Int)=>Unit;
  val noPostParseFn = (_:Tree[String],_:Tree[String],_:Seq[String],_:ParseEval[String]#Statistics,_:Int)=>()

  def evaluate(trees: IndexedSeq[(Tree[String],Seq[String])],
               parser: Parser[String,String],
               postEval: PostParseFn = noPostParseFn) = {

    val peval = new ParseEval(Set("","''", "``", ".", ":", ","));
    import peval.Statistics;

    def evalSentence(sent: (Tree[String],Seq[String])) = try {
      val (goldTree,words) = sent;
      val startTime = System.currentTimeMillis;
      val guessTree = Trees.debinarize(parser(words))
      val stats = peval(guessTree,goldTree);
      val endTime = System.currentTimeMillis;
      postEval(guessTree,goldTree,words,stats,(endTime-startTime).toInt);
      stats;
    } catch {
      case e:RuntimeException => throw new RuntimeException("Error parsing: " + sent._1.render(sent._2), e);
    }
    val stats = trees.par.withSequentialThreshold(100).mapReduce({ evalSentence(_:(Tree[String],Seq[String]))},{ (_:Statistics) + (_:Statistics)});

    val (prec,recall,exact) = (stats.precision,stats.recall,stats.exact);
    (prec,recall,exact);
  }

  def evaluateAndLog(trees: IndexedSeq[(Tree[String],Seq[String])], parser: Parser[String,String], evalDir: String) = {

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
                 stats: ParseEval[String]#Statistics, time: Int) = {
      val buf = new StringBuilder();
      buf ++= "======\n";
      buf ++= words.mkString(",");
      buf ++= "\nGold:\n";
      buf ++= goldTree.render(words);
      buf ++= "\nGuess:\n";
      buf ++= guessTree.render(words);
      buf ++= ("\nLocal Accuracy:" + (stats.precision,stats.recall,stats.exact) + "\n") ;
      buf ++= (time / 1000.0 + " Seconds")
      buf ++= "\n======";
      println(buf.toString);
      appender ! Some((guessTree.render(words,newline=false)), goldTree.render(words,newline=false));
    }

    val r = evaluate(trees,parser,postEval _);
    appender ! None;
    r
  }
}

