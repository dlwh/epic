package scalanlp.parser;
package discrim;

import scalala.tensor.dense._
import scalanlp.collection.mutable.SparseArrayMap;
import scalala.tensor.sparse._;
import scalanlp.parser.Grammar
import scalanlp.tensor.sparse.OldSparseVector

/**
 *
 * @author dlwh
 */
class FeaturizedGrammar[L,W](weights: DenseVector[Double], features: FeatureIndexer[L,W]) extends Grammar[L] {
  override val index = features.labelIndex;

  private val indexedUnaryRulesByChild = fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity));
  private val indexedUnaryRulesByParent = fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity));
  for( (bRules,a) <- features.unaryRuleCache.iterator.zipWithIndex; (b,f) <- bRules) {
    val score = f dot weights;
    indexedUnaryRulesByChild.getOrElseUpdate(b)(a) = score;
    indexedUnaryRulesByParent.getOrElseUpdate(a)(b) = score;
  }

  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByLeftChild:SparseArrayMap[SparseArrayMap[OldSparseVector]] = {
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  };
  // Mapping is Left Child -> Right Child -> Parent -> Score
  private val indexedBinaryRulesByRightChild:SparseArrayMap[SparseArrayMap[OldSparseVector]] = {
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  };
  // Mapping is Parent -> Left Child -> Right Child ->  Score
  private val indexedBinaryRulesByParent:SparseArrayMap[SparseArrayMap[OldSparseVector]] = {
    fillSparseArrayMap(fillSparseArrayMap(mkOldSparseVector(Double.NegativeInfinity)))
  }
  for( (bRules,a) <- features.binaryRuleCache.iterator.zipWithIndex; (b,cRules) <- bRules; (c,f) <- cRules) {
    val score = f dot weights;
    indexedBinaryRulesByLeftChild.getOrElseUpdate(b).getOrElseUpdate(c)(a) = score;
    indexedBinaryRulesByRightChild.getOrElseUpdate(c).getOrElseUpdate(b)(a) = score;
    indexedBinaryRulesByParent.getOrElseUpdate(a).getOrElseUpdate(b)(c) = score;
  }

   /**
   * Returns pairs of the form ((parent,child),score);
   */
  def unaryRulesByChild(c: L) = {
    assert(c != null);
    val cc = index(c);
    indexedUnaryRulesByChild(cc).activeIterator.map { case (a,v) => (UnaryRule(index.get(a),c),v)};
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
    indexedUnaryRulesByParent(pp).activeIterator.map { case (c,v) => (UnaryRule(p,index.get(c)),v)};
  }

  /**
   * Returns pairs of the form ( BinaryRule, score);
   */
  def binaryRulesByLeftChild(b: L) = {
    val bb = index(b);
    for( (c,arr) <- indexedBinaryRulesByLeftChild(bb).iterator;
        (a,v) <- arr.activeIterator) yield (BinaryRule(index.get(a),b,index.get(c)), v);
  }

  def allBinaryRules = indexedBinaryRulesByLeftChild;
  def allUnaryRules = indexedUnaryRulesByChild;

  def binaryRulesByIndexedLeftChild(b: Int) = indexedBinaryRulesByLeftChild.getOrElse(b,indexedBinaryRulesByLeftChild.defaultValue);

  def binaryRulesByIndexedRightChild(c: Int): SparseArrayMap[OldSparseVector] = indexedBinaryRulesByRightChild(c);

  def binaryRulesByIndexedParent(a: Int): SparseArrayMap[OldSparseVector] = indexedBinaryRulesByParent.getOrElse(a,indexedBinaryRulesByParent.defaultValue);

  def binaryRuleScore(a: Int, b: Int, c: Int) = indexedBinaryRulesByParent(a)(b)(c);
  def unaryRuleScore(a: Int, b: Int) = indexedUnaryRulesByParent(a)(b);
}