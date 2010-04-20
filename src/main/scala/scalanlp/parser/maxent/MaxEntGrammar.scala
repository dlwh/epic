package scalanlp.parser.maxent

import MaxEntParser._;
import java.util.Arrays
import scala.collection.mutable.ArrayBuffer
import scalala.Scalala._;
import scalala.tensor.Vector;
import scalanlp.collection.mutable.SparseArray
import scalala.tensor.counters.Counters.PairedDoubleCounter
import scalanlp.data.VectorBroker
import scalanlp.parser._
import scalanlp.trees.Span
import scalanlp.util.Index


class MaxEntGrammar[L,W](featurizer: SentenceFeaturizer[L,W], rules: Map[L,PairedDoubleCounter[Rule[L],Feature]]) extends VectorBroker[L] {
  /*def unaryRulesByChild(c: L): Iterator[UnaryRule[L]];
  def unaryRulesByParent(p: L): Iterator[UnaryRule[L]];
  def binaryRulesByLeftChild(c: L): Iterator[BinaryRule[L]];
  */

  val index: Index[L] = {
    val index = Index[L]();
    for( (l,ctr) <- rules) {
      index.index(l);
      for( (r,_) <- ctr.rows;
           c <- r.children) {
        index.index(c);
      }
    }

    index;
  }

  def score(r: Rule[L], span: Span, words: Seq[W]):Double = {
    val weight = rules(r.parent)(r);
    val features = featurizer(r,span,words);
    weight dot features;
  }

  private val unaryRules = fillArray(new ArrayBuffer[Int]);
  private val binaryRules_lc_rc_parent = fillArray(fillSparseArray(new ArrayBuffer[Int]));
  private val binaryRules_rc_lc_parent = fillArray(fillSparseArray(new ArrayBuffer[Int]));
  for( (l,ctr) <- rules) {
    val lIndex = index(l);
    for( (r,_) <- ctr.rows) {
      r match {
        case r: UnaryRule[L] => unaryRules(index(r.child)) += lIndex;
        case r: BinaryRule[L] => 
          binaryRules_lc_rc_parent(index(r.left))(index(r.right)) += lIndex;
          binaryRules_rc_lc_parent(index(r.right))(index(r.left)) += lIndex;
      }
    }
  }

  /**
   * Returns an array of parent index
   */
  def unaryRulesByIndexedChild(c: Int): Seq[Int] = {
    unaryRules(c);
  }

  /**
   * Returns a SparseArray[Seq[Int]] with RightIndex -> ParentIndex
   */
  def binaryRulesByIndexedLeftChild(b: Int): SparseArray[ArrayBuffer[Int]] = {
    binaryRules_lc_rc_parent(b)
  }

  /**
   * Returns a SparseArray[Vector] with LeftIndex -> ParentIndex -> Score
   */
  def binaryRulesByIndexedRightChild(c: Int): SparseArray[ArrayBuffer[Int]] = {
    binaryRules_rc_lc_parent(c);
  }



}
