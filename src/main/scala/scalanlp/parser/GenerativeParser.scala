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
import scalanlp.text.tokenize.PTBTokenizer
import scalanlp.trees._;
import scalanlp.util._;
import scalala.Scalala.{log=>_,_};
import scalala.tensor._;

import scalanlp.util.Index;
import Math._;



object GenerativeParser {

  def fromTrees[W](data: Iterable[(BinarizedTree[String],Seq[W])]):ChartParser[String,String,W] = {
    fromTrees(data.iterator);
  }
    
    
  def fromTrees[W](data: Iterator[(BinarizedTree[String],Seq[W])]):ChartParser[String,String,W] = {
    val root = "";
    val (lexicon,grammar) = extractLexiconAndGrammar(data);
    val builder = CKYChartBuilder(root, lexicon, grammar);
    ChartParser(builder);
  }

  def extractLexiconAndGrammar[W](data: Iterator[(BinarizedTree[String],Seq[W])]):(Lexicon[String,W],GenerativeGrammar[String]) = {
    val (lexicon,binaryProductions,unaryProductions) = extractCounts(data);
    (new SimpleLexicon(lexicon),new GenerativeGrammar(logNormalizeRows(binaryProductions),logNormalizeRows(unaryProductions)));
  }


  def extractCounts[L,W](data: Iterator[(BinarizedTree[L],Seq[W])]) = {
    val lexicon = new PairedDoubleCounter[L,W]();
    val binaryProductions = new PairedDoubleCounter[L,BinaryRule[L]]();
    val unaryProductions = new PairedDoubleCounter[L,UnaryRule[L]]();

    for( (tree,words) <- data) {
      val leaves = tree.leaves map (l => (l,words(l.span.start)));
      tree.allChildren foreach { 
        case t @ BinaryTree(a,bc,cc) => 
          binaryProductions(a,BinaryRule(a,bc.label,cc.label)) += 1.0;
        case t@UnaryTree(a,bc) => 
          unaryProductions(a,UnaryRule(a,bc.label)) += 1.0;
        case t => 
      }
      for( (l,w) <- leaves) {
        lexicon(l.label).incrementCount(w,1);
      }
      
    }
    (lexicon,binaryProductions,unaryProductions)
  }
}

object GenerativeTrainer extends ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  devTrees: Seq[(BinarizedTree[String],Seq[String],SpanScorer)],
                  config: Configuration) = {
    Iterator.single(("Gen",GenerativeParser.fromTrees(trainTrees.view.map(c => (c._1,c._2)))));
  }
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
      val words = PTBTokenizer().apply(line.trim);
      println(words.mkString(","));
      try {
        val parse = parser(words.toSeq);
        println(Trees.debinarize(parse) render words.toSeq);
      } catch {
        case e => e.printStackTrace();
      }
    }
  }
}
