package epic.util

import breeze.util.BloomFilter
import java.util.concurrent.ConcurrentLinkedDeque

import scala.collection.JavaConverters._

/**
 * TODO
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class ThreadLocalBloomFilter[@specialized(Int, Long) T](numBuckets: Int, numHashFunctions: Int) extends LockableSeenSet[T] {
  private val tl = new ThreadLocal[BloomFilter[T]]() {
    override def initialValue(): BloomFilter[T] = {
      val bf = new BloomFilter[T](numBuckets, numHashFunctions)
      queue.offer(bf)
      bf
    }
  }


  override def addOrSeen(x: T): Boolean = {tl.get() += x; true}

  private val queue = new ConcurrentLinkedDeque[BloomFilter[T]]()

  def union:BloomFilter[T] = queue.asScala.reduceLeft(_ | _)

  def lock = new BloomFilterSeenSet[T](union)
}

