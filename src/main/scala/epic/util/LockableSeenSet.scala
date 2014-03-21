package epic.util

import breeze.util.BloomFilter

/**
 * TODO
 *
 * @author dlwh
 **/
trait LockableSeenSet[@specialized(Int, Long) -T] {
  def addOrSeen(x: T):Boolean
  def lock: LockableSeenSet[T]
}

class BloomFilterSeenSet[@specialized(Int, Long) T](bf: BloomFilter[T]) extends LockableSeenSet[T] {
  override def addOrSeen(x: T): Boolean = {
    bf(x)
  }

  override def lock: LockableSeenSet[T] = this
}

object LockableSeenSet {
  def always[T]:LockableSeenSet[T] = AlwaysSeenSet
}


object AlwaysSeenSet extends LockableSeenSet[Any] {
  override def addOrSeen(x: Any): Boolean = true

  override def lock: LockableSeenSet[Any] = this
}