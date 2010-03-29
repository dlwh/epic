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



import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.BitSet

import scalanlp.data._;
import scalanlp.data.process._;
import scalanlp.counters._;
import Counters._;
import LogCounters.{LogPairedDoubleCounter,LogDoubleCounter,logNormalizeRows};
import scalanlp.trees._;
import scalanlp.util._;
import scalala.Scalala.{log=>_,_};
import scalala.tensor._;

import scalanlp.util.Index;
import Math._;


import GenerativeParser._;

@serializable
@SerialVersionUID(1L)
class GenerativeParser[L,W](root: L, lexicon: Lexicon[L,W],
                            grammar: Grammar[L]) extends Parser[L,W] {
  
  // Score is a vector of scores whose indices are nonterms or preterms
  private def updateUnaries(chart: ParseChart[L], begin:Int, end: Int) = {
    var recheck:ArrayBuffer[Int] = new ArrayBuffer[Int]();
    var lastRecheck = new ArrayBuffer[Int]();
    val set = new BitSet();
    recheck ++= chart.enteredLabelIndexes(begin, end);
    val max_iter = 5;

    var iter = 0;
    while(iter < max_iter) {
      iter += 1;

      val elems = recheck.iterator;
      val temp = recheck;
      recheck = lastRecheck;
      lastRecheck = temp;
      set.clear();

      for( b <- elems) {
        val bScore = chart.labelScore(begin,end,b);
        for( (a,aScore) <- grammar.unaryRulesByIndexedChild(b).activeElements) {
          val prob = aScore + bScore;
          if(prob > chart.labelScore(begin,end,b)) {
            chart.enterUnary(begin,end,a,b,prob);
            if(!set(a)) {
              set += a;
              recheck += a;
            }
          }
        }
      }
      lastRecheck.clear();
    }
  }


  def scores(s: Seq[W]) = {

    val chart = new ParseChart(grammar,s.length);

    for{i <- 0 until s.length} {
      for ( a <- lexicon.tags;
            wScore = lexicon.wordScore(a,s(i))
            if !wScore.isInfinite) {
        assert(a != null);
        chart.enterTerm(i,i+1,a,wScore);
      }

      updateUnaries(chart,i,i+1);
    }

    for {
      span <- 2 to s.length;
      begin <- 0 to (s.length - span);
      end = begin + span
    } {
      for {
        split <- (begin+1) to (end-1);
        (b,bScore) <- chart.enteredLabelScores(begin, split);
        if !bScore.isInfinite
        (c,parentVector) <- grammar.binaryRulesByIndexedLeftChild(b)
      } {
        val cScore = chart.labelScore(split, end, c)
        if (!cScore.isInfinite)
          for {
            (a,aScore) <- parentVector.activeElements
          } {
            val prob = bScore + cScore + aScore;
            if(prob > chart.labelScore(begin,end,a)) {
              chart.enterBinary(begin,split,end,a,b,c,prob);
            }
          }
      }
      updateUnaries(chart,begin,end);
    }

    val bestParse = chart.buildTree(0,s.length,grammar.index(root));
    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = chart.labelScore(0, s.length, root);
    c;
  }
}

object GenerativeParser {

  def fromTrees[W](data: Collection[(Tree[String],Seq[W])], binarize: Tree[String]=>BinarizedTree[String]):GenerativeParser[String,W] = {
    fromTrees(data.iterator,binarize);
  }
    
    
  def fromTrees[W](data: Iterator[(Tree[String],Seq[W])], binarize: Tree[String]=>BinarizedTree[String]):GenerativeParser[String,W] = {
    val root = "";
    val (lexicon,productions) = extractCounts(data,binarize);
    new GenerativeParser(root,new SimpleLexicon(lexicon),new GenerativeGrammar(logNormalizeRows(productions)));
  }

  def extractCounts[W](data: Iterator[(Tree[String],Seq[W])], binarize: Tree[String]=>BinarizedTree[String]) = {
    val lexicon = new PairedDoubleCounter[String,W]();
    val productions = new PairedDoubleCounter[String,Rule[String]]();

    for( (tree,words) <- data) {
      val btree = binarize(tree);
      val leaves = tree.leaves map (l => (l,words(l.span.start)));
      btree.allChildren foreach { 
        case t @ BinaryTree(a,bc,cc) => 
          productions(a,BinaryRule(a,bc.label,cc.label)) += 1.0;
        case t@UnaryTree(a,bc) => 
          productions(a,UnaryRule(a,bc.label)) += 1.0;
        case t => 
      }
      for( (l,w) <- leaves) {
        lexicon(l.label).incrementCount(w,1);
      }
      
    }
    (lexicon,productions)

  }
}


object GenerativeTester {
  import java.io.File;
 // import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    val trees = for( (tree,words) <- treebank.trainTrees)
      yield (xform(tree),words map (_.intern));
    val parser = GenerativeParser.fromTrees(trees,Trees.xBarBinarize _ );
    //println("Train:");
    //eval(treebank.trainTrees,parser,xform);
    println("Dev:");
    eval(treebank.devTrees,parser,xform);
    println("Test:");
    eval(treebank.testTrees,parser,xform);
  }

  def eval(trees: Iterator[(Tree[String],Seq[String])],
           parser: GenerativeParser[String,String],
           xform: Tree[String]=>Tree[String]) {
    val peval = new ParseEval(Set(""));
    val goldGuess = {
      for { 
        (goldTree, words) <- trees;
        () = println(words);
        () = println(xform(goldTree) render words);
        startTime = System.currentTimeMillis;
        guessTree = Trees.debinarize(parser(words))
      } yield {
        val endTime = System.currentTimeMillis;
        println(guessTree render words);
        val ret = (xform(goldTree),guessTree)
        println("Local Accuracy:" + peval(Iterator.single(ret)));
        println( (endTime - startTime) / 1000.0 + " Seconds");
        println("===========");
        ret;
      }
    }
    val (prec,recall,exact) = peval(goldGuess);
    val f1 = (2 * prec * recall)/(prec + recall);
    println( "P: " + prec + " R:" + recall + " F1: " + f1 +  " Ex:" + exact);
  }

}

object GenerativeInterpreter {
  import java.io.File;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    val trees = for( (tree,words) <- treebank.trainTrees)
      yield (xform(tree),words map (_.intern));
    val parser = GenerativeParser.fromTrees(trees,Trees.xBarBinarize _);
    while(true) {
      print("Ready> ");
      val line = readLine();
      if(line.trim == "quit") System.exit(1);
      val words = PTBTokenizer.tokenize(line.trim);
      words match {
        case Left(words)=>
        println(words.mkString(","));
        try {
          val parse = parser(words);
          println(Trees.debinarize(parse) render words);
        } catch {
          case e => e.printStackTrace();
        }
        case Right(bleh) => println(bleh);
      }
    }
  }
}
