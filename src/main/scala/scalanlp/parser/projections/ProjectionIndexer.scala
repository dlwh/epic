package scalanlp.parser.projections

import scalanlp.util.{Encoder, Index}
import collection.mutable.ArrayBuffer

/**
 * For computing projections from a fine grammar to a coarse grammar
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ProjectionIndexer[C,F] private (val coarseIndex: Index[C],
                                      val fineIndex:Index[F], indexedProjections: Array[Int]) extends (Int=>Int) {
  val coarseEncoder = Encoder.fromIndex(coarseIndex);

  val refinements = {
    val result = Encoder.fromIndex(coarseIndex).fillArray(new ArrayBuffer[Int]);
    for( (coarse,fine) <- indexedProjections zipWithIndex) {
      result(coarse) += fine
    }
    result.map(_.toArray);
  }

  def refinementsOf(c: Int):IndexedSeq[Int] = refinements(c);
  def refinementsOf(c: C):IndexedSeq[F] = refinements(coarseIndex(c)).map(fineIndex.get _);

  /**
   * Computes the projection of the indexed fine label f to an indexed coarse label.
   */
  def project(f: Int):Int = indexedProjections(f);

  def project(f: F):C = coarseIndex.get(project(fineIndex(f)));

  def coarseSymbol(f: Int) = coarseIndex.get(project(f));

  /**
   * Same as project(f)
   */
  def apply(f: Int) = project(f)
}

object ProjectionIndexer {
  def simple[L](index: Index[L]) = ProjectionIndexer(index,index, identity[L] _);

  def apply[C,F](coarseIndex: Index[C], fineIndex: Index[F], proj: F=>C) = {
    val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1);
    for( (l,idx) <- fineIndex.zipWithIndex) {
      val projectedIdx = coarseIndex(proj(l));
      if(projectedIdx < 0) throw new RuntimeException("error while indexing" + l + " to " + proj(l) + fineIndex(l));
      indexedProjections(idx) = projectedIdx;
    }
    new ProjectionIndexer(coarseIndex,fineIndex,indexedProjections);
  }
}
