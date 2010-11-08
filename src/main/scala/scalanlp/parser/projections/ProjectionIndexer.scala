package scalanlp.parser.projections

import scalanlp.util.{Encoder, Index}

/**
 * For computing projections from a fine grammar to a coarse grammar
 * @author dlwh
 */
class ProjectionIndexer[C,F](coarseIndex: Index[C], fineIndex:Index[F], proj: F=>C) extends (Int=>Int) {
  private val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1);
  for( (l,idx) <- fineIndex.zipWithIndex) {
    indexedProjections(idx) = coarseIndex(proj(l));
  }

  /**
   * Computes the projection of the indexed fine label f to an indexed coarse label.
   */
  def project(f: Int):Int = indexedProjections(f);

  def project(f: F):C = coarseIndex.get(project(fineIndex(f)));

  /**
   * Same as project(f)
   */
  def apply(f: Int) = project(f)
}