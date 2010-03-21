package scalanlp.parser;

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.BitSet
import scala.collection.mutable.Map;

import scalanlp.data._;
import scalanlp.data.process._;
import scalanlp.collection.mutable.SparseArray
import scalanlp.collection.mutable.TriangularArray
import scalanlp.counters._;
import Counters._;
import LogCounters.{LogPairedDoubleCounter,LogDoubleCounter,logNormalizeRows};
import scalanlp.trees._;
import java.util.Arrays;
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
  
  private class BackPtr;
  private case object Term extends BackPtr;
  private case class Unary(lchild: Int) extends BackPtr;
  private case class Binary(lchild: Int, rchild: Int, split: Int) extends BackPtr;

  // Score is a vector of scores whose indices are nonterms or preterms
  private def updateUnaries(score: Vector,
                            back: SparseArray[BackPtr],
                            active: IntBloomFilter) {
    var recheck:ArrayBuffer[Int] = new ArrayBuffer[Int]();
    var lastRecheck = new ArrayBuffer[Int]();
    val set = new BitSet(score.size);
    recheck ++= score.activeElements.filter(!_._2.isInfinite).map(_._1);
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
        val bScore = score(b);
        for( (a,aScore) <- grammar.unaryRulesByIndexedChild(b).activeElements) {
          val prob = aScore + bScore;
          if(prob > score(a)) {
            score(a) = prob;
            back(a) = Unary(b);
            active += a;
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
    val score = new TriangularArray(s.length+1, grammar.mkVector(Double.NegativeInfinity));
    val back = new TriangularArray(s.length+1, grammar.mkSparseArray[BackPtr]);
    val active = new TriangularArray(s.length+1, new IntBloomFilter(grammar.index.size,3));

    for{i <- 0 until s.length} {
      for ( a <- lexicon.tags;
            wScore = lexicon.wordScore(a,s(i))
            if !wScore.isInfinite) {
        assert(a != null);
        score(i)(i+1)(grammar.index(a)) = wScore;
        back(i)(i+1)(grammar.index(a)) = Term;
        active(i)(i+1) += grammar.index(a);
      }

      updateUnaries(score(i)(i+1),back(i)(i+1),active(i)(i+1));
    }

    for {
      span <- 2 to s.length;
      begin <- 0 to (s.length - span);
      end = begin + span
    } {
      for {
        split <- (begin+1) to (end-1);
        (b,bScore) <- score(begin)(split).activeElements;
        if !bScore.isInfinite
        (c,parentVector) <- grammar.binaryRulesByIndexedLeftChild(b)
        if active(split)(end)(c)
      } {
        val cScore = score(split)(end)(c)
        if (!cScore.isInfinite)
          for {
            (a,aScore) <- parentVector.activeElements
          } {
            val prob = bScore + cScore + aScore;
            if(prob > score(begin)(end)(a)) {
              score(begin)(end)(a) = prob;
              back(begin)(end)(a) = Binary(b,c,split);
              active(begin)(end) += a;
            }
          }
      }
      updateUnaries(score(begin)(end),back(begin)(end),active(begin)(end));
    }

    val bestParse = try {
      buildTree(back,0,s.length,grammar.index(root));
    } catch { case e =>
      println( score(0)(s.length).toString);
      println( back(0)(s.length).toString);
      throw e;
    }

    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = score(0)(s.length)(grammar.index(root));
    c;
  }

  private def buildTree(back: TriangularArray[SparseArray[BackPtr]], start: Int, end: Int, root: Int):BinarizedTree[L] = {
    val b = back(start)(end)(root)
    b match {
      case Term =>
        NullaryTree(grammar.index.get(root))(Span(start,end));
      case Unary(child) => 
        val c = buildTree(back,start,end,child);
        UnaryTree(grammar.index.get(root),c)(Span(start,end));
      case Binary(lchild,rchild,split) => 
        val lc = buildTree(back,start,split,lchild);
        val rc = buildTree(back,split,end,rchild);
        BinaryTree(grammar.index.get(root),lc,rc)(Span(start,end));
    }
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
