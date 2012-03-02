package scalanlp.parser.projections

import scalanlp.util.{Encoder, Index}
import collection.mutable.ArrayBuffer

/**
 * For computing projections from a fine grammar to a coarse grammar
 * @author dlwh
 */
@SerialVersionUID(1)
final class ProjectionIndexer[C,F] private (val coarseIndex: Index[C],
                                      val fineIndex:Index[F], indexedProjections: Array[Int]) extends (Int=>Int) with Serializable {
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

  /**
   *  From a PI[C,F] and a PI[F,F2], makes a PI[C,F2]
   */
  def compose[F2](finerProjections: ProjectionIndexer[F,F2]) = {
    ProjectionIndexer(coarseIndex, finerProjections.fineIndex, {finerProjections.project(_ : F2)} andThen {project(_ : F)})
  }

  override def toString() = {
    coarseIndex.map(x => x -> refinementsOf(x)).mkString("ProjectionIndexer(",", ", ")")

  }
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

  def fromSplitter[C,F](coarseIndex: Index[C], fineIndex: Index[F], split: C=>Seq[F]) = {
    val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1);
    for( (c,cf) <- coarseIndex.zipWithIndex; f <- split(c)) {
      try {
        indexedProjections(fineIndex(f)) = cf
      } catch {
        case e => println("Grrr... " + f + "\n" + fineIndex); throw e;
      }
    }
    new ProjectionIndexer(coarseIndex,fineIndex,indexedProjections)

  }

  def fromSplitter[C,F](coarseIndex: Index[C], split: C=>Seq[F]) = {
    val fineIndex = Index[F]()
    val indexedProjections = new ArrayBuffer[Int]()
    for( (c,cf) <- coarseIndex.zipWithIndex; f <- split(c)) {
      val i = fineIndex.index(f)
      indexedProjections += cf
    }
    new ProjectionIndexer(coarseIndex,fineIndex,indexedProjections.toArray)

  }
}
