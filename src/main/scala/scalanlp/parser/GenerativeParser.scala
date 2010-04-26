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
import scalanlp.config.Configuration
import scalala.tensor.counters._;
import Counters._;
import LogCounters.{LogPairedDoubleCounter,LogDoubleCounter,logNormalizeRows};
import scalanlp.trees._;
import scalanlp.util._;
import scalala.Scalala.{log=>_,_};
import scalala.tensor._;

import scalanlp.util.Index;
import Math._;

class GenerativeParser[L,W](root: L, lexicon: Lexicon[L,W],
                            grammar: Grammar[L]) extends Parser[L,W] {
  
  // Score is a vector of scores whose indices are nonterms or preterms
  private def updateUnaries(chart: ParseChart[L], begin:Int, end: Int) = {
    var recheck:ArrayBuffer[Int] = new ArrayBuffer[Int]();
    val set = new BitSet();
    recheck ++= chart.enteredLabelIndexes(begin, end);
    val max_iter = 5;

    var iter = 0;
    while(iter < max_iter) {
      iter += 1;

      val elems = recheck.iterator;
      recheck = new ArrayBuffer[Int]();
      set.clear();

      for( b <- elems) {
        val bScore = chart.labelScore(begin,end,b);
        for( (a,aScore) <- grammar.unaryRulesByIndexedChild(b).activeElements) {
          val prob = aScore + bScore;
          if(prob > chart.labelScore(begin,end,a)) {
            chart.enterUnary(begin,end,a,b,prob);
            if(!set(a)) {
              set += a;
              recheck += a;
            }
          }
        }
      }
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

    try {
      val bestParse = chart.buildTree(0,s.length,grammar.index(root));
      val c = DoubleCounter[Tree[L]]();
      c(bestParse) = chart.labelScore(0, s.length, root);
      c;
    } catch {
      case e =>
        chart.dumpChart();
        throw e;
    }
  }
}

object GenerativeParser {

  def fromTrees[W](data: Iterable[(BinarizedTree[String],Seq[W])]):GenerativeParser[String,W] = {
    fromTrees(data.iterator);
  }
    
    
  def fromTrees[W](data: Iterator[(BinarizedTree[String],Seq[W])]):GenerativeParser[String,W] = {
    val root = "";
    val (lexicon,productions) = extractCounts(data);
    new GenerativeParser(root,new SimpleLexicon(lexicon),new GenerativeGrammar(logNormalizeRows(productions)));
  }

  def extractCounts[L,W](data: Iterator[(BinarizedTree[L],Seq[W])]) = {
    val lexicon = new PairedDoubleCounter[L,W]();
    val productions = new PairedDoubleCounter[L,Rule[L]]();

    for( (tree,words) <- data) {
      val leaves = tree.leaves map (l => (l,words(l.span.start)));
      tree.allChildren foreach { 
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

object GenerativeTester extends ParserTester {
  def trainParser(trainTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  devTrees: Iterable[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = Iterator.single(("",GenerativeParser.fromTrees(trainTrees)));
}

object GenerativeInterpreter {
  import java.io.File;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    val trees = for( (tree,words) <- treebank.trainTrees)
      yield (Trees.binarize(xform(tree)),words map (_.intern));
    val parser = GenerativeParser.fromTrees(trees);
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
