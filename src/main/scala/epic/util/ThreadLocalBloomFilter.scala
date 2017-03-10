package epic.util

import breeze.util.{ BloomFilter, SerializableLogging }
import java.util.concurrent.ConcurrentLinkedDeque

import scala.collection.JavaConverters._

/**
 * TODO
 *
 * @author dlwh
 **/
@SerialVersionUID(1L)
class ThreadLocalBloomFilter[@specialized(Int, Long) T](numBuckets: Int, numHashFunctions: Int) extends LockableSeenSet[T] with SerializableLogging {
  private val tl = new ThreadLocal[BloomFilter[T]]() {
    override def initialValue(): BloomFilter[T] = {
      val bf = new BloomFilter[T](numBuckets, numHashFunctions)
      queue.offer(bf)
      bf
    }
  }

  override def addOrSeen(x: T): Boolean = {tl.get() += x; true}

  private val queue = new ConcurrentLinkedDeque[BloomFilter[T]]()

  def union:BloomFilter[T] = {
    val bf = tl.get()
    var i = 0
    val len = queue.size
    while (!queue.isEmpty && i < len) {
      bf |= queue.pop()
      i += 1
    }
    queue.push(bf)
    bf
  }

  def lock = {
    val u = union
    val load = u.load
    val size = - u.numBuckets * math.log1p(-load)/u.numHashFunctions
    logger.info(f"Bloom filter has load of ${u.load}%.3f and approx size $size. Queue is ${queue.size()} elements long.")
    new BloomFilterSeenSet[T](u)
  }
}

