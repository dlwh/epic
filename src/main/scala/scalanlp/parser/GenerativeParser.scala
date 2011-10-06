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



import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.parser.ParserParams.NoParams

import scalanlp.trees._;

import scalala.tensor.Counter2
import scalala.library.Library


object GenerativeParser {

  def fromTrees[W](data: Traversable[TreeInstance[String,W]]):SimpleChartParser[String,String,W] = {
    val root = "";
    val (lexicon,grammar) = extractLexiconAndGrammar(data);
    val builder = CKYChartBuilder(root, lexicon, grammar);
    SimpleChartParser(builder);
  }

  def extractLexiconAndGrammar[W](data: TraversableOnce[TreeInstance[String,W]]) = {
    val (wordCounts,binaryProductions,unaryProductions) = extractCounts(data);
    val lexicon = new SimpleLexicon(wordCounts);
    (lexicon,Grammar(Library.logAndNormalizeRows(binaryProductions),Library.logAndNormalizeRows(unaryProductions)));
  }


  def extractCounts[L,W](data: TraversableOnce[TreeInstance[L,W]]) = {
    val lexicon = Counter2[L,W,Double]();
    val binaryProductions = Counter2[L,BinaryRule[L],Double]();
    val unaryProductions = Counter2[L,UnaryRule[L],Double]();

    for( ti <- data) {
      val TreeInstance(_,tree,words,_) = ti;
      val leaves = tree.leaves map (l => (l,words(l.span.start)));
      tree.allChildren foreach { 
        case t @ BinaryTree(a,bc,cc) => 
          binaryProductions(a,BinaryRule(a,bc.label,cc.label)) += 1.0;
        case t@UnaryTree(a,bc) => 
          unaryProductions(a,UnaryRule(a,bc.label)) += 1.0;
        case t => 
      }
      for( (l,w) <- leaves) {
        lexicon(l.label,w) += 1
      }
      
    }
    (lexicon,binaryProductions,unaryProductions)
  }
}

object GenerativePipeline extends ParserPipeline with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  config: Params) = {
    val parser = GenerativeParser.fromTrees(trainTrees);
    Iterator.single(("Gen",parser));
  }
}

object SigPipeline extends ParserPipeline with NoParams {
  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  devTrees: IndexedSeq[TreeInstance[String,String]],
                  unaryReplacer : ChainReplacer[String],
                  config: Params) = {
    val (words,binary,unary) = GenerativeParser.extractCounts(trainTrees);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val parser = SimpleChartParser(CKYChartBuilder("",lexicon,grammar).withCharts(ParseChart.logProb));
    Iterator.single(("Gen",parser));
  }
}
