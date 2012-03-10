package scalanlp.parser.lex

import scalala.library.Library
import scalala.tensor.Counter2
import scalanlp.parser._
import scalanlp.trees.{UnaryTree, BinaryTree}

/**
 * 
 * @author dlwh
 */
object GenLexGrammarExtractor {

//  def extractLexiconAndGrammar[W](data: TraversableOnce[TreeInstance[String,W]]) = {
//    val (wordCounts,binaryProductions,unaryProductions) = extractCounts(data);
//    val lexicon = new SimpleLexicon(wordCounts);
//    (lexicon,Grammar(Library.logAndNormalizeRows(binaryProductions),Library.logAndNormalizeRows(unaryProductions)));
//  }

  def extractCounts[L,W](data: TraversableOnce[TreeInstance[L,W]]) = {
    val lexicon = Counter2[L,W,Double]();
    val binaryProductions = Counter2[L,BinaryRule[L],Double]();
    val unaryProductions = Counter2[L,UnaryRule[L],Double]();
    val leftBilexicalCounts = Counter2[W,W,Double]() // how often w1 is the head of a phrase dominated by w2
    val rightBilexicalCounts = Counter2[W,W,Double]()
    val leftLabelCounts = Counter2[L,W,Double]()

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


  }

}


