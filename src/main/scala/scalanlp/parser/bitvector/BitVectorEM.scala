package scalanlp.parser.bitvector

import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.counters.LogCounters
import scalanlp.counters.LogCounters.LogDoubleCounter
import scalanlp.math.Numerics.logSum;
import scalanlp.trees.Treebank
import scalanlp.trees.Trees
import scalanlp.parser._;
import scalala.tensor.dense.DenseMatrix
import scalanlp.collection.mutable.SparseArray
import scalanlp.counters.Counters._;
import Math.exp

import scalanlp.parser.splitting.StateSplitting
import scalanlp.parser.splitting.StateSplitting._;


object BitVectorEM {

  def iterateBits(field: Int, numBits: Int) = for ( b <- 0 until numBits iterator) yield {
    val m = 1 << b;
    val bit = if( (m & field) != 0) 1 else 0
    (b,bit);
  }

  def collapseBinaryRules[L](g: Grammar[(L,Int)], counts: SparseArray[SparseArray[Vector]], numBits: Int) = {
    import collection.mutable.HashMap
    def mkMap: HashMap[(L,L),DenseMatrix] = new HashMap[(L,L),DenseMatrix] {
      override def default(k: (L,L)) = {
        val dm = new DenseMatrix(numBits,2);
        update(k,dm);
        dm
      }
    };

    // (parent,BitVector) index -> leftChild rightChild -> (leftBit,set or not) -> count
    val leftBitCounts = g.fillArray(mkMap);
    // (parent,BitVector) index -> leftChild rightChild -> (rightBit,set or not) -> count
    val rightBitCounts = g.fillArray(mkMap);

    for {
      (parentI,c1) <- counts.iterator;
      (lchildI,c2) <- c1.iterator;
      lchild = g.index.get(lchildI);
      (lbit,leftPolarity) <- iterateBits(lchild._2, numBits);
      (rchildI,count) <- c2.activeElements
      rchild = g.index.get(rchildI);
      (rbit,rightPolarity) <- iterateBits(rchild._2, numBits)
    } {
      val lcur = leftBitCounts(parentI)((lchild._1,rchild._1))(lbit,leftPolarity);
      leftBitCounts(parentI)((lchild._1,rchild._1))(lbit,leftPolarity) = logSum(lcur,count);
      val rcur = rightBitCounts(parentI)((lchild._1,rchild._1))(rbit,rightPolarity)
      rightBitCounts(parentI)((lchild._1,rchild._1))(rbit,rightPolarity) = logSum(rcur,count);
    }

    // Should actualy be 0.0 (i.e log(1))
    val binaryRuleCounts = g.fillSparseArray(g.fillSparseArray(g.mkVector(0.0)));
    // i.e. true if the bit in field is set or not set
    def bitMatches(field: Int, bit: Int, set: Int) = ((field & (1 << bit)) > 0) == (set != 0);
    for {
      (map,parentI) <- leftBitCounts.zipWithIndex;
      ((lchild,rchild),mat) <- map;
      bit <- 0 until numBits;
      set <- 0 to 1;
      lstate <- 0 until (1 << numBits) if bitMatches(lstate,bit,set);
      rstate <- 0 until (1 << numBits)
    } {
      val denom = logSum(mat(bit,1),mat(bit,0));
      binaryRuleCounts(parentI)(g.index((lchild,lstate)))(g.index((rchild,rstate))) += mat(bit,set) - denom;
    }

    for {
      (map,parentI) <- rightBitCounts.zipWithIndex;
      ((lchild,rchild),mat) <- map;
      bit <- 0 until numBits;
      set <- 0 to 1;
      rstate <- 0 until (1 << numBits) if bitMatches(rstate,bit,set);
      lstate <- 0 until (1 << numBits)
    } {
      val denom = logSum(mat(bit,1),mat(bit,0));
      binaryRuleCounts(parentI)(g.index((lchild,lstate)))(g.index((rchild,rstate))) += mat(bit,set) - denom;
    }

    binaryRuleCounts;
  }

  def collapseUnaryRules[L](g: Grammar[(L,Int)], counts: SparseArray[Vector], numBits: Int) = {
    import collection.mutable.HashMap
    def mkMap: HashMap[L,DenseMatrix] = new HashMap[L,DenseMatrix] {
      override def default(k: L) = {
        val dm = new DenseMatrix(numBits,2);
        update(k,dm);
        dm
      }
    };

    // (parent,BitVector) index -> child -> (bit,set or not) -> count
    val bitCounts = g.fillArray(mkMap);

    for {
      (parentI,vec) <- counts.iterator;
      (childI,count) <- vec.activeElements
      child = g.index.get(childI);
      (bit,polarity) <- iterateBits(child._2, numBits)
    } {
      val cur = bitCounts(parentI)(child._1)(bit,polarity) ;
      bitCounts(parentI)(child._1)(bit,polarity) = logSum(cur,count);
    }

    // Should actualy be 0.0 (i.e log(1))
    val unaryRuleCounts = g.fillSparseArray(g.mkVector(0.0));
    // i.e. true if the bit in field is set or not set
    def bitMatches(field: Int, bit: Int, set: Int) = ((field & (1 << bit)) > 0) == (set != 0);
    for {
      (map,parentI) <- bitCounts.zipWithIndex;
      (child,mat) <- map;
      bit <- 0 until numBits;
      set <- 0 to 1;
      state <- 0 until (1 << numBits) if bitMatches(state,bit,set)
    } {
      val denom = logSum(mat(bit,1), mat(bit,0));
      unaryRuleCounts(parentI)(g.index((child,state))) += mat(bit,set) - denom;
    }

    unaryRuleCounts;
  }

  def splitCycle[L,W](ruleCounts: RuleCounts[L],
                      wordCounts: TagWordCounts[L,W],
                      trees: StateSplitting.Treebank[L,W],
                      numBits: Int = 3,
                      randomNoise: Double =0.1,convergence:Double=1E-4):
                      (RuleCounts[(L,Int)],TagWordCounts[(L,Int),W],Double) = {

    def split(x: L) = (0 until (1 << numBits) ) map { i => (x,i) };
    val (splitRules,splitWords) = splitCounts(ruleCounts,wordCounts,split _,randomNoise);

    val stateIterator = Iterator.iterate( (splitRules,splitWords,Double.MaxValue,convergence.abs*100)) { case state =>
      val (splitRules,splitWords,lastLogProb,_) = state;
      val grammar = new GenerativeGrammar(LogCounters.logNormalizeRows(splitRules));
      val lexicon = new UnsmoothedLexicon(LogCounters.logNormalizeRows(splitWords));
      val results = for {
        (t,s) <- trees.iterator
      } yield expectedCounts(grammar,lexicon,t map split,s);

      val binaryRuleCounts = grammar.fillSparseArray(grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity)));
      val unaryRuleCounts = grammar.fillSparseArray(grammar.mkVector(Double.NegativeInfinity));
      val wordCounts = grammar.fillSparseArray(LogDoubleCounter[W]());
      var logProb = 0.0;

      for( (bCounts,uCounts,wCounts,tProb) <- results) {
        for( (k1,c) <- bCounts;
            (k2,vec) <- c) {
          logAdd(binaryRuleCounts(k1)(k2),vec);
        }
        for( (k,vec) <- uCounts) {
          logAdd(unaryRuleCounts(k),vec);
        }
        for( (k,vec) <- wCounts) {
          logAdd(wordCounts(k),vec);
        }
        logProb += tProb;
      }

      val twiddledBinaryCounts = collapseBinaryRules(grammar,binaryRuleCounts,numBits);
      val twiddledUnaryCounts = collapseUnaryRules(grammar,unaryRuleCounts,numBits);

      val finalRules = decodeRules(grammar,twiddledBinaryCounts,twiddledUnaryCounts);
      val finalWords = decodeWords(grammar,wordCounts);

      for( (k,ctr) <- finalWords.rows) {
        println(k + " " + ctr.maxk(30));
      }

      val improvement = (lastLogProb-logProb)/lastLogProb;
      println("Iter: " + logProb + "(" + lastLogProb + "->" +improvement+")");

      (finalRules,finalWords,logProb,improvement);
    }
    stateIterator.drop(10).dropWhile(_._4 > convergence).map { case (sR,sW,logP,_) => (sR,sW,logP) }.next
  }
}


object BitVectorTest {
  import java.io.File;
 // import scalax.io.Implicits._;
  def main(args: Array[String]) {
    val treebank = Treebank.fromPennTreebankDir(new File(args(0)));
    val xform = Trees.Transforms.StandardStringTransform;
    println("Loading Treebank");
    val trees = (for( (tree,words) <- treebank.trainTrees take 2000)
      yield (Trees.xBarBinarize(xform(tree)),words map (_.intern))) toSeq;
    println("Extracting counts");
    val (initialLex,initialProductions) = (
      GenerativeParser.extractCounts(trees.iterator,Trees.xBarBinarize _)
    );
    println("Splitting Grammar");
    val (finalLex,finalProd,logProb) = BitVectorEM.splitCycle(LogCounters.log(initialProductions),LogCounters.log(initialLex),trees,args(1).toInt);
  }

}