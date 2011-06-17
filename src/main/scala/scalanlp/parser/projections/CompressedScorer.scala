package scalanlp.parser
package projections

import scalala.tensor.Vector
import scalanlp.collection.mutable.TriangularArray
import scalala.tensor.sparse.SparseHashVector

/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class CompressedScorer[L](vector: Array[Vector], length: Int, numLabels: Int) extends SpanScorer[L] {
  def scoreLexical(begin: Int, end: Int, tag: Int) = {
    encode(begin,0,end,tag,numLabels,numLabels)
  }

  def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = {
    encode(begin,0,end,parent,child,numLabels)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    encode(begin,split,end,parent,leftChild,rightChild)
  }

  @inline
  private def encode(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    if(vector(begin) == null) Double.NegativeInfinity
    else {
      val index = CompressedScorer.encode(length,numLabels,split,end,parent,leftChild,rightChild)
      vector(begin)(index);
    }

  }
}

object CompressedScorer {
  @inline
  def encode(length:Int, numLabels: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
    val encodedSplit = (TriangularArray.index(split,end))
    val encodedRule = parent + (numLabels) * (rightChild + (numLabels+1) * leftChild);
    encodedSplit + (numLabels * (numLabels + 1) * (numLabels+1)) * encodedRule

  }

  def fromScorer[L](scorer: SpanScorer[L], length: Int, numLabels: Int) = {
    val vec = Array.fill(length){
      val v = new SparseHashVector(length * (length+1)/2 * numLabels * (numLabels + 1) * (numLabels+1));
      v.default = Double.NegativeInfinity;
      v:Vector
    }

    // fill in lexicals
    for(begin <- 0 until length; end = begin + 1; l <- 0 until numLabels) {
      val currentScore = scorer.scoreLexical(begin,end,l);
      if(currentScore != Double.NegativeInfinity)
        vec(begin)(encode(length,numLabels,0,end,l,numLabels,numLabels)) = currentScore;
    }
    // the main loop, similar to cky
    for(diff <- 1 to length) {
      for(begin <- 0 until (length - diff + 1)) {
        val end = begin + diff;
        // do binaries
        for( parent <- 0 until numLabels) {
          for(split <- (begin+1) until end) {
            for(b <- 0 until numLabels; c <- 0 until numLabels) {
              val score = scorer.scoreBinaryRule(begin,split,end,parent,b,c);
              if(score != Double.NegativeInfinity)
                vec(begin)(encode(length,numLabels,split,end,parent,b,c)) = score;
            }
          }
        }

        // do unaries. Similar to above
        for( parent <- 0 until numLabels) {
          for( child <- 0 until numLabels) {
            val score =  scorer.scoreUnaryRule(begin,end,parent,child);
            if(score != Double.NegativeInfinity) {
              vec(begin)(encode(length,numLabels,0,end,parent,child,numLabels)) = score;
            }
          }
        }
      }
    }

    new CompressedScorer[L](vec,length,numLabels);
  }
}