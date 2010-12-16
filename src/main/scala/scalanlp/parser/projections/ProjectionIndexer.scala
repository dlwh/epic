package scalanlp.parser.projections

import scalanlp.util.{Encoder, Index}
import collection.mutable.ArrayBuffer

/**
 * For computing projections from a fine grammar to a coarse grammar
 * @author dlwh
 */
@serializable
@SerialVersionUID(1)
class ProjectionIndexer[C,F](val coarseIndex: Index[C], val fineIndex:Index[F], proj: F=>C) extends (Int=>Int) {
  private val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1);
  for( (l,idx) <- fineIndex.zipWithIndex) {
    indexedProjections(idx) = coarseIndex(proj(l));
  }

  val coarseEncoder = Encoder.fromIndex(coarseIndex);

  val refinements = {
    val result = Encoder.fromIndex(coarseIndex).fillArray(new ArrayBuffer[Int]);
    for( (coarse,fine) <- indexedProjections zipWithIndex) {
      result(coarse) += fine
    }
    result.map(_.toArray);
  }

  def refinementsOf(c: Int):Array[Int] = refinements(c);

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