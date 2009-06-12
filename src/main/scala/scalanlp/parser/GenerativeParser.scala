package scalanlp.parser;

import scalanlp.data._;
import scalanlp.counters._;
import scalanlp.trees._;
import java.util.Arrays;

import scalanlp.util.Index;

@serializable
@SerialVersionUID(1L)
class GenerativeParser[L,W](rootL: L, lexicon: PairedDoubleCounter[L,W],
                            lUnaryRules: PairedDoubleCounter[L,L], 
                            lBinaryRules: PairedDoubleCounter[L,(L,L)]) extends Parser[L,W] {
  
  private val nonTermIndex = new Index[L];

  private val root = nonTermIndex(rootL);
  private val tags = new PairedDoubleCounter[Int,W];
  tags ++= lexicon map { case (k,v) => (nonTermIndex(k),v); }
  private val unaryRules = new PairedDoubleCounter[Int,Int];
  unaryRules += lUnaryRules.triples map { case (k,j,v) => (nonTermIndex(k),nonTermIndex(j),v) }
  private val binaryRules = new PairedDoubleCounter[Int,(Int,Int)];
  binaryRules += lBinaryRules.triples map { case (k,(j,i),v) => (nonTermIndex(k),(nonTermIndex(j),nonTermIndex(i)),v) }

  
  
  def scores(o: Observation[Seq[W]]) = {
    val s = o.features;
    val score = new Array[Array[Array[Double]]](s.length+1, s.length + 1, nonTermIndex.size);
    for(arr1 <- score;
      arr2 <- arr1) {
      Arrays.fill(arr2,Math.NEG_INF_DOUBLE);
    }
    val back = new Array[Array[Array[(Int,Int,Int)]]](s.length+1,s.length+1, nonTermIndex.size);
    for{i <- 0 until s.length} {
      for ( a <- tags.keys) {
        score(i)(i+1)(a) = Math.log( tags(a)(s(i)) ) - Math.log(tags(a).total);
        back(i)(i+1)(a) = (-1,-1,-1)
      }
      var added = true;
      while(added) {
        added = false;
        for( (a,prods) <- unaryRules;
              (b,myProb) <- prods) {
          if(!score(i)(i+1)(b).isInfinite) {
            val prob = Math.log(myProb/prods.total) + score(i)(i+1)(b);
            if(prob > score(i)(i+1)(a)) {
              score(i)(i+1)(a) = prob;
              back(i)(i+1)(a) = (b,-1,i);
              added = true;
            }
          }
        }
      }
    }
    for( span <- 2 to s.length;
      begin <- 0 to (s.length - span);
      end = begin + span;
      split <- (begin+1) to (end-1);
      (a,prods) <- binaryRules;
      ((b,c),myProb) <- prods) {

      val prob = score(begin)(split)(b) + score(split)(end)(c) + Math.log( myProb/prods.total);
      if(prob > score(begin)(end)(a)) {
        score(begin)(end)(a) = prob;
        back(begin)(end)(a) = (b,c,split);
      }
      var added = true;
      while(added) {
        added = false;
        for( (a,prods) <- unaryRules;
          (b,myProb) <- prods) {
          if(!score(begin)(end)(b).isInfinite) {
            val prob = Math.log(myProb/prods.total) + score(begin)(end)(b);
            if(prob > score(begin)(end)(a)) {
              score(begin)(end)(a) = prob;
              back(begin)(end)(a) = (b,-1,begin);
              added = true;
            }
          } 
        } 
      }
    }

    val bestParse = buildTree(back,0,s.length,root) map (nonTermIndex.get _ );
    val c = DoubleCounter[Tree[L]]();
    c(bestParse) = score(0)(s.length)(root);
    c;
  }

  private def buildTree(back: Array[Array[Array[(Int,Int,Int)]]], start: Int, end: Int, root: Int):BinarizedTree[Int] = {
    val b = back(start)(end)(root)
    b match {
      case (-1,-1,-1) =>
        NullaryTree(root)(Span(start,end));
      case (child,-1,i) => 
        val c = buildTree(back,start,end,child);
        UnaryTree(root,c)(Span(start,end));
      case (lchild,rchild,split) => 
        val lc = buildTree(back,start,split,lchild);
        val rc = buildTree(back,split,end,rchild);
        BinaryTree(root,lc,rc)(Span(start,end));
    }
  }
}


object GenerativeParser {
  def fromTrees[W](data: Collection[(Tree[String],Seq[W])]):GenerativeParser[String,W] = {
    fromTrees(data.elements);
  }
    
    
  def fromTrees[W](data: Iterator[(Tree[String],Seq[W])]):GenerativeParser[String,W] = {
    val root = "";
    val lexicon = new PairedDoubleCounter[String,W](0.1);
    val unaryRules = new PairedDoubleCounter[String,String](0.1);
    val binaryRules = new PairedDoubleCounter[String,(String,String)](0.1);

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

    new GenerativeParser(root,lexicon,unaryRules,binaryRules);
  }
}

object GenerativeInterpreter {
  import java.io.File;
  import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val trees = {
      for(dir <- new File(args(0)).listFiles.elements;
          if dir.isDirectory;
          file <- dir.listFiles.elements;
          (tree,words) <- PennTreeReader.readTrees(file.slurp).fold( x=>x, x => throw new Exception("error in " + file + " " + x.toString)) elements)
        yield (tree.map(_.replaceAll("\\-.*","").intern),words.map(_.intern));
    }
    val parser = GenerativeParser.fromTrees(trees);
    while(true) {
      print("Ready> ");
      val line = readLine();
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
