package epic.features

import breeze.util.Index
import epic.framework.{ComponentFeature, Feature}
import java.util

/**
 *
 *
 * @author dlwh
 */
class SegmentedIndex[T,IndexType](val indices: IndexedSeq[IndexType])(implicit view: IndexType <:< Index[T]) extends Index[Feature] {
  def apply(t: Feature): Int = t match {
    case ComponentFeature(i, f) if i < indices.length && i >= 0 => indices(i)(f.asInstanceOf[T])
    case _ => -1
  }

  private val offsets = indices.scanLeft(0)(_ + _.size).toArray

  override def size = offsets.last

  def unapply(i: Int): Option[Feature] = {
    if(i < 0 || i >= size) {
      None
    } else {
      var component = util.Arrays.binarySearch(offsets, i)
      if(component < 0) component = ~component - 1
      indices(component).unapply(i - offsets(component)).map(ComponentFeature(component, _))
    }
  }

  def pairs: Iterator[(Feature, Int)] = Iterator.range(0, size).map(i => unapply(i).get -> i)

  def iterator: Iterator[Feature] =  Iterator.range(0, size).map(i => unapply(i).get)

  def addComponentOffset(component: Int, feature: Int) = feature + offsets(component)
  def componentOffset(component: Int) = offsets(component)
}

object SegmentedIndex {
  def apply[T,IndexType](indices: IndexType*)(implicit view: IndexType <:< Index[T]) =  {
    new SegmentedIndex(indices.toIndexedSeq)(view)
  }

}
