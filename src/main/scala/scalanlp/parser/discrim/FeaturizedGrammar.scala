package scalanlp.parser;
package discrim;

import scalala.tensor.dense._
import scalanlp.collection.mutable.SparseArray;
import scalala.tensor.sparse._;
import scalanlp.parser.Grammar

/**
 *
 * @author dlwh
 */
class FeaturizedGrammar[L,W](weights: DenseVector, features: FeatureIndexer[L,W]) extends Grammar[L] {
  override val index = features.labelIndex;

  private val indexedUnaryRulesByChild:Array[SparseVector] = fillArray(mkSparseVector(Double.NegativeInfinity));
  private val indexedUnaryRulesByParent:Array[SparseVector] = fillArray(mkSparseVector(Double.NegativeInfinity));
  for( (bRules,a) <- features.unaryRuleCache.iterator.zipWithIndex; (b,f) <- bRules) {
    val score = f dot weights;
    indexedUnaryRulesByChild(b)(a) = score;
    indexedUnaryRulesByParent(a)(b) = score;
  }

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByLeftChild:SparseArray[SparseArray[SparseVector]] = {
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  };
  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByRightChild:SparseArray[SparseArray[SparseVector]] = {
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  };
  // Mapping is Parent -> Left Child -> Right Child ->  Score
  private val indexedBinaryRulesByParent:SparseArray[SparseArray[SparseVector]] = {
    fillSparseArray(fillSparseArray(mkSparseVector(Double.NegativeInfinity)))
  }
  for( (bRules,a) <- features.binaryRuleCache.iterator.zipWithIndex; (b,cRules) <- bRules; (c,f) <- cRules) {
    val score = f dot weights;
    indexedBinaryRulesByLeftChild(b)(c)(a) = score;
    indexedBinaryRulesByRightChild(c)(b)(a) = score;
    indexedBinaryRulesByParent(a)(b)(c) = score;
  }

   /**
   * Returns pairs of the form ((parent,child),score);
   */
  def unaryRulesByChild(c: L) = {
    assert(c != null);
    val cc = index(c);
    indexedUnaryRulesByChild(cc).activeElements.map { case (a,v) => (UnaryRule(index.get(a),c),v)};
  }


  def unaryRulesByIndexedChild(c: Int) = {
    indexedUnaryRulesByChild(c);
  }

  def unaryRulesByIndexedParent(p: Int) = {
    indexedUnaryRulesByParent(p);
  }

  /**
   * Returns pairs of the form (child,score);
   */
  def unaryRulesByParent(p: L) = {
    assert(p != null);
    val pp = index(p);
    indexedUnaryRulesByParent(pp).activeElements.map { case (c,v) => (UnaryRule(p,index.get(c)),v)};
  }

  /**
   * Returns pairs of the form ( BinaryRule, score);
   */
  def binaryRulesByLeftChild(b: L) = {
    val bb = index(b);
    for( (c,arr) <- indexedBinaryRulesByLeftChild(bb).iterator;
        (a,v) <- arr.activeElements) yield (BinaryRule(index.get(a),b,index.get(c)), v);
  }

  def allBinaryRules = indexedBinaryRulesByLeftChild;

  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild.getOrElse(b,indexedBinaryRulesByLeftChild.defaultValue);

  def binaryRulesByIndexedRightChild(c: Int): SparseArray[SparseVector] = indexedBinaryRulesByRightChild(c);

  def binaryRulesByIndexedParent(a: Int): SparseArray[SparseVector] = indexedBinaryRulesByParent.getOrElse(a,indexedBinaryRulesByParent.defaultValue);

  def binaryRuleScore(a: Int, b: Int, c: Int) = indexedBinaryRulesByParent(a)(b)(c);
  def unaryRuleScore(a: Int, b: Int) = indexedUnaryRulesByParent(a)(b);
}