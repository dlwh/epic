package scalanlp.parser;

import scalanlp.data._;
import scalanlp.counters._;
import scalanlp.trees._;
import java.util.Arrays;

import scalanlp.util.Index;
import Math._;

import scala.collection.mutable.Map;

import GenerativeParser._;

@serializable
@SerialVersionUID(1L)
class GenerativeParser[L,W](root: L, lexicon: Lexicon[L,W],
                            grammar: Grammar[L]) extends Parser[L,W] {
  
  private class BackPtr;
  private case object Term extends BackPtr;
  private case class Unary(lchild: L) extends BackPtr;
  private case class Binary(lchild: L, rchild: L, split: Int) extends BackPtr;

  def scores(o: Observation[Seq[W]]) = {
    val s = o.features;
    val score = new Array[Array[DoubleCounter[L]]](s.length+1, s.length + 1);
    for(arr1 <- score;
      i <- 0 until arr1.length) {
      arr1(i) = DoubleCounter[L]();
    }
    val back = new Array[Array[Map[L,BackPtr]]](s.length+1,s.length+1);

    for(arr1 <- back;
      i <- 0 until arr1.length) {
      arr1(i) = Map[L,BackPtr]();
    }
    for{i <- 0 until s.length} {
      for ( a <- lexicon.tags;
            wScore = lexicon.wordScore(a,s(i))
            if !wScore.isInfinite) {
        score(i)(i+1)(a) = wScore;
        back(i)(i+1)(a) = Term;
      }
      var added = true;
      while(added) {
        added = false;

        for( (b,bScore) <- score(i)(i+1);
            ((a,_),aScore) <- grammar.unaryRulesByChild(b)) {
          val prob = aScore + bScore;
          if(!score(i)(i+1).contains(a) || prob > score(i)(i+1)(a)) {
            score(i)(i+1)(a) = prob;
            back(i)(i+1)(a) = Unary(b);
            added = true;
          }
        }
      }
    }
    for( span <- 2 to s.length;
        begin <- 0 to (s.length - span);
        end = begin + span;
        split <- (begin+1) to (end-1);
        (b,bScore) <- score(begin)(split);
        ((a,(_,c)),aScore) <- grammar.binaryRulesByLeftChild(b)
        if score(split)(end) contains c) {
      val prob = bScore + score(split)(end)(c) + aScore;
      if(!score(begin)(end).contains(a) || prob > score(begin)(end)(a)) {
        score(begin)(end)(a) = prob;
        back(begin)(end)(a) = Binary(b,c,split);
      }
      var added = true;
      while(added) {
        added = false;
        for( (b,bScore) <- score(begin)(end);
          ((a,_),aScore) <- grammar.unaryRulesByChild(b)) {
          val prob = aScore + bScore;
          if(!score(begin)(end).contains(a) || prob > score(begin)(end)(a)) {
            score(begin)(end)(a) = prob;
            back(begin)(end)(a) = Unary(b);
            added = true;
          }
        } 
      }
    }

/*
    for( (arr,i) <- back zipWithIndex;
         (ctr,j) <- arr zipWithIndex
       )  {
       println( (i,j) + score(i)(j).toString);
       println( (i,j) + ctr.toString);
     }
     */

    val bestParse = buildTree(back,0,s.length,root);
    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = score(0)(s.length)(root);
    c;
  }

  private def buildTree(back: Array[Array[Map[L,BackPtr]]], start: Int, end: Int, root: L):BinarizedTree[L] = {
    val b = back(start)(end)(root)
    b match {
      case Term =>
        NullaryTree(root)(Span(start,end));
      case Unary(child) => 
        val c = buildTree(back,start,end,child);
        UnaryTree(root,c)(Span(start,end));
      case Binary(lchild,rchild,split) => 
        val lc = buildTree(back,start,split,lchild);
        val rc = buildTree(back,split,end,rchild);
        BinaryTree(root,lc,rc)(Span(start,end));
    }
  }
}


object GenerativeParser {

  class Grammar[L](private val unaryRules: PairedDoubleCounter[L,L],
                   private val binaryRules: PairedDoubleCounter[L,(L,L)]) {
    {
      val totals = DoubleCounter[L]();
      for( (a,ctr) <- unaryRules) {
        totals(a) += ctr.total;
      }
      for( (a,ctr) <- binaryRules) {
        totals(a) += ctr.total;
      }

      unaryRules.transform { (a,ctr) =>
        val total = log(totals(a));
        ctr.transform  { (b,count) =>
          val c = log(count) - total;
          assert(c <= 0,"" + ((a,b,c)) + count + total);
          c
        }
        ctr;
      }

      binaryRules.transform { (a,ctr) =>
        val total = log(totals(a));
        ctr.transform  { (r,count) =>
          val c = log(count) - total;
          assert(c <= 0, "" + ((a,r,c)) + count + total + totals(a));
          c
        }
        ctr;
      }
    }

    private val leftChildBinaryRules = new PairedDoubleCounter[L,(L,(L,L))];
    for((a,ctr) <- binaryRules;
        (r@(b,_),score) <- ctr) {
      leftChildBinaryRules.incrementCount(b,(a,r),score);
    }

    private val childUnaryParents = new PairedDoubleCounter[L,(L,L)];
    for((a,ctr) <- unaryRules;
        (b,score) <- ctr) {
      childUnaryParents.incrementCount(b,(a,b),score);
    }

    /**
    * Returns pairs of the form ((parent,child),score);
    */
    def unaryRulesByChild(c: L) = childUnaryParents(c).elements;
    /**
    * Returns pairs of the form (child,score);
    */
    def unaryRulesByParent(p: L) = unaryRules(p).elements;

    /**
    * Returns pairs of the form ( (parent,(left,right)),score);
    */
    def binaryRulesByLeftChild(c: L) = leftChildBinaryRules(c).elements;
    /**
    * Returns pairs of the form ( (lchild,rchild),
    */
    def binaryRulesByParent(p: L) = binaryRules(p).elements;
  }

  class Lexicon[L,W](private val lexicon: PairedDoubleCounter[L,W], smoothing: Double) {
    def wordScore(l: L, w: W) = {
      val result = log(lexicon(l)(w) + smoothing) - log(lexicon(l).total + lexicon(l).size * smoothing)
      result;
    }

    def tags = lexicon.keys;
  }

  def fromTrees[W](data: Collection[(Tree[String],Seq[W])]):GenerativeParser[String,W] = {
    fromTrees(data.elements);
  }
    
    
  def fromTrees[W](data: Iterator[(Tree[String],Seq[W])]):GenerativeParser[String,W] = {
    val root = "";
    val lexicon = new PairedDoubleCounter[String,W]();
    val unaryRules = new PairedDoubleCounter[String,String]();
    val binaryRules = new PairedDoubleCounter[String,(String,String)]();

    for( (tree,words) <- data) {
      val btree = Trees.binarize(tree);
      val leaves = tree.leaves map (l => (l,words(l.span.start)));
      btree.allChildren foreach { 
        case t @ BinaryTree(a,bc,cc) => 
          binaryRules.incrementCount( a, (bc.label,cc.label), 1.0); 
        case t@UnaryTree(a,bc) => 
          unaryRules.incrementCount( a, bc.label, 1.0); 
        case t => 
      }
      for( (l,w) <- leaves) {
        lexicon(l.label).incrementCount(w,1);
      }
      
    }

    new GenerativeParser(root,new Lexicon(lexicon,0.1),new Grammar(unaryRules,binaryRules));
  }
}

object GenerativeInterpreter {
  import java.io.File;
  import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val trees = {
     for(file <- new File(args(0)).listFiles.elements;
     //for(dir <- new File(args(0)).listFiles.elements;
       //   if dir.isDirectory;
     //     file <- dir.listFiles.elements;
          (tree,words) <- PennTreeReader.readTrees(file.slurp).fold( x=>x, x => throw new Exception("error in " + file + " " + x.toString)) elements)
        yield (tree.map(_.replaceAll("\\-.*","").intern),words.map(_.intern));
    }
    val parser = GenerativeParser.fromTrees(trees);
    while(true) {
      print("Ready> ");
      val line = readLine();
      if(line.trim == "quit") System.exit(1);
      val words =new scalanlp.data.process.PTBTokenizer tokenize(line.trim);
      words match {
        case Left(words)=>
        println(words.mkString(","));
        try {
          val parse = parser(Observation("console",words));
          println(Trees.debinarize(parse) render words);
        } catch {
          case e => e.printStackTrace();
        }
        case Right(bleh) => println(bleh);
      }
      ()
    }
    ()
  }
}
