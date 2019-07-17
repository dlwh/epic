package epic.parser
package projections

import breeze.util.{Encoder, Index}
import collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

/**
 * For computing projections from a fine grammar to a coarse grammar.
 * 
 * There are two indexes maintained for the fine symbols: a globally unique
 * id, and a local per-symbol id (in the range 0...numRefinements(coarseSym).
 *
 * Yuck.
 * 
 * @author dlwh
 */
@SerialVersionUID(1)
final class ProjectionIndexer[C, F] private (val coarseIndex: Index[C],
                                                 val fineIndex:Index[F],
                                                 indexedProjections: Array[Int]) extends (Int=>Int) with Serializable {

  val coarseEncoder = Encoder.fromIndex(coarseIndex)

  // coarseSymbolIndex -> localizedFineSymbol -> globalizedFineSymbolIndex
  val globalRefinements: Array[Array[Int]] = {
    val result = Encoder.fromIndex(coarseIndex).fillArray(new ArrayBuffer[Int])
    for( (coarse, fine) <- indexedProjections.zipWithIndex if coarse != -1) {
      result(coarse) += fine
    }
    result.map(arr => arr.toArray)
  }

  // globaleRefined -> localRefined
  val localizationArray = new Array[Int](indexedProjections.length)
  // just coarseSymbolIndex -> (0 until numGlobalRefinements(coarseSymbolIndex))
  val perSymbolRefinements = globalRefinements.map { arr =>
    for( (global,local) <- arr.zipWithIndex) {
      localizationArray(global) = local
    }
    Array.range(0, arr.length)
  }

  def localize(f: Int): Int =  localizationArray(f)
  def globalize(c: Int, f: Int): Int = globalRefinements(c)(f)
  def globalize(c: C, f: Int):F = fineIndex.get(globalRefinements(coarseIndex(c))(f))
  def indexAndLocalize(f: F):(Int, Int) = {
    val glob = fineIndex(f)
    if (glob < 0) (-1, -1)
    else project(glob) -> localize(glob)
  }

  def localize(f: F):(C, Int) = {
    val i = fineIndex(f)
    if (i < 0) throw new RuntimeException(s"Not in fine index: $f")
    coarseIndex.get(indexedProjections(i)) -> localizationArray(i)
  }

  def refinementsOf(c: Int):Array[Int] = globalRefinements(c)

  def localRefinements(c: Int):Array[Int] = perSymbolRefinements(c)

  def numRefinements(c: Int): Int = perSymbolRefinements(c).length

  def refinementsOf(c: C):IndexedSeq[F] = {
    val ci = coarseIndex(c)
    if (ci < 0) throw new RuntimeException("Not a coarse symbol: " + c)
    globalRefinements(ci).map(fineIndex.get _)
  }

  /**
   * Computes the projection of the indexed fine label f to an indexed coarse label.
   */
  def project(f: Int): Int = indexedProjections(f)

  def project(f: F):C = coarseIndex.get(project(fineIndex(f)))

  def coarseSymbol(f: Int) = coarseIndex.get(project(f))

  /**
   * Same as project(f)
   */
  def apply(f: Int) = project(f)

  /**
   *  From a PI[C, F] and a PI[F, F2], makes a PI[C, F2]
   */
  def compose[F2](finerProjections: ProjectionIndexer[F, F2]) = {
    ProjectionIndexer(coarseIndex, finerProjections.fineIndex, {finerProjections.project(_ : F2)} andThen {project(_ : F)})
  }

  override def toString() = {
    coarseIndex.map(x => x -> refinementsOf(x)).mkString("ProjectionIndexer(", ", ", ")")
  }

  def localizeArray[T:ClassTag](array: Array[T]):Array[Array[T]] = {
    require(array.length == fineIndex.size)
    Array.tabulate(coarseIndex.size) { c =>
      val refs = refinementsOf(c)
      Array.tabulate(refs.length)(r => array(r))
    }
  }
}

object ProjectionIndexer {
  def simple[L](index: Index[L]) = ProjectionIndexer(index, index, identity[L] _)

  def apply[C, F](coarseIndex: Index[C], fineIndex: Index[F], proj: F=>C, skipMissingCoarse: Boolean = false) = {
    val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1)
    for( (l, idx) <- fineIndex.zipWithIndex) {
      val projectedIdx = coarseIndex(proj(l))
      if (projectedIdx < 0) {
        if (!skipMissingCoarse)
        throw new RuntimeException("error while indexing" + l + " to " + proj(l) + fineIndex(l))
      } else {
        indexedProjections(idx) = projectedIdx
      }
    }
    new ProjectionIndexer(coarseIndex, fineIndex, indexedProjections)
  }

  def fromSplitter[C, F](coarseIndex: Index[C], fineIndex: Index[F], split: C=>Seq[F]) = {
    val indexedProjections = Encoder.fromIndex(fineIndex).fillArray(-1)
    for( (c, cf) <- coarseIndex.zipWithIndex; f <- split(c)) {
      try {
        indexedProjections(fineIndex(f)) = cf
      } catch {
        case e: Throwable => throw e
      }
    }
    new ProjectionIndexer(coarseIndex, fineIndex, indexedProjections)
  }

  def fromSplitter[C, F](coarseIndex: Index[C], split: C=>Seq[F]) = {
    val fineIndex = Index[F]()
    val indexedProjections = new ArrayBuffer[Int]()
    for( (c, cf) <- coarseIndex.zipWithIndex; f <- split(c)) {
      val i = fineIndex.index(f)
      indexedProjections += cf
    }
    new ProjectionIndexer(coarseIndex, fineIndex, indexedProjections.toArray)
  }

}
