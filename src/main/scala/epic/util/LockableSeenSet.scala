package epic.util

import breeze.util.BloomFilter
import java.io.ObjectStreamException

/**
 * TODO
 *
 * @author dlwh
 **/
trait LockableSeenSet[@specialized(Int, Long) -T] extends Serializable {
  def addOrSeen(x: T): Boolean
  def lock: LockableSeenSet[T]
}

@SerialVersionUID(1L)
class BloomFilterSeenSet[@specialized(Int, Long) T](bf: BloomFilter[T]) extends LockableSeenSet[T] with Serializable {
  override def addOrSeen(x: T): Boolean = {
    bf(x)
  }
  override def lock: LockableSeenSet[T] = this
}

object LockableSeenSet {
  def always[T]:LockableSeenSet[T] = AlwaysSeenSet
}

@SerialVersionUID(1L)
object AlwaysSeenSet extends LockableSeenSet[Any] {
  override def addOrSeen(x: Any): Boolean = true
  override def lock: LockableSeenSet[Any] = this
  @throws[ObjectStreamException]
  private def readResolve() = {
    AlwaysSeenSet
  }
}