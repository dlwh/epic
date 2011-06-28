package scalanlp.parser

import scalanlp.collection.mutable.TriangularArray
import java.util.Arrays
import scalala.tensor.dense.DenseVector

/**
 * 
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class LabelScoreArray(length: Int, grammarSize: Int, fill: Double) {
  protected final val score = TriangularArray.raw(length+1, LabelScoreArray.mkGrammarVector(grammarSize, fill));
  protected final val enteredLabels = TriangularArray.raw(length+1,new collection.mutable.BitSet());

  final def apply(begin: Int, end: Int, label: Int) = labelScore(begin,end,label);
  final def labelScore(begin: Int, end: Int, label: Int) = score(TriangularArray.index(begin,end))(label);
  final def enteredLabelIndexes(begin: Int, end: Int) = {
    enteredLabels(TriangularArray.index(begin,end)).iterator;
  }

  def enteredLabelScores(begin: Int, end: Int) = {
    val scoreArray = score(TriangularArray.index(begin,end));
    enteredLabels(TriangularArray.index(begin,end)).iterator.map { i => (i,scoreArray(i))};
  }
}

object LabelScoreArray {
  private def mkGrammarVector(grammarSize: Int, fill: Double) = {
    val r = DenseVector.zeros[Double](grammarSize);
    Arrays.fill(r.data,fill);
    r
  }
}