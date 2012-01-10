package scalanlp.parser

import scalanlp.collection.mutable.TriangularArray
import java.util.Arrays
import scalala.tensor.dense.{DenseVectorCol, DenseVector}

/**
 * A LabelScoreArray is just a triangular array whose entries are arrays. Admits efficient
 * iteration over "on" elements in an (i,j) index.
 * @author dlwh
 */
@SerialVersionUID(1)
class LabelScoreArray(length: Int, grammarSize: Int, fill: Double) extends Serializable {
  final val score = TriangularArray.raw(length+1, null:Array[Double])
  final val enteredLabels = TriangularArray.raw(length+1,new collection.mutable.BitSet());

  final def apply(begin: Int, end: Int, label: Int) = labelScore(begin,end,label);
  @inline final def labelScore(begin: Int, end: Int, label: Int) = {
    val ind = TriangularArray.index(begin,end)
    if(score(ind) eq null) Double.NegativeInfinity
    else score(ind)(label)
  };
  final def enteredLabelIndexes(begin: Int, end: Int) = {
    enteredLabels(TriangularArray.index(begin,end)).iterator;
  }

  def enteredLabelScores(begin: Int, end: Int) = {
    val scoreArray = score(TriangularArray.index(begin,end));
    if(scoreArray eq null) Iterator.empty
    else enteredLabels(TriangularArray.index(begin,end)).iterator.map { i => (i,scoreArray(i))};
  }
}

object LabelScoreArray {
  @inline
  def mkGrammarVector(grammarSize: Int, fill: Double) = {
//    val r: DenseVectorCol[Double] = DenseVector.zeros[Double](grammarSize);
    val arr = new Array[Double](grammarSize)
    Arrays.fill(arr,fill);
    arr
  }
}