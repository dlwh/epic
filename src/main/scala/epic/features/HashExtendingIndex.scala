package epic.features

import breeze.util.Index
import epic.util.{LockableSeenSet, ThreadLocalBloomFilter}
import breeze.util.SerializableLogging

/**
 * TODO
 *
 * @author dlwh
 **/
@SerialVersionUID(-5376213550426304522L)
class HashExtendingIndex[T](val baseIndex: Index[T],
                            hashWrapper: Int=>T,
                            hashScale: HashFeature.Scale = HashFeature.Relative(1),
                            // we expect ~1 million features, 8MB gives about a 2% error rate
                            //= new ThreadLocalBloomFilter(8 * 1024 * 1024, 6)
                            cache: LockableSeenSet[Long] = LockableSeenSet.always) extends Index[T] with SerializableLogging {
  val numHashFeatures = hashScale.numFeatures(baseIndex.size)
  override def size: Int = baseIndex.size + numHashFeatures

  def lock = new HashExtendingIndex(baseIndex, hashWrapper, hashScale, cache.lock)

  def apply(t: T): Int = baseIndex(t) match {
    case -1 =>
      val code = t.##.abs
      if (!cache.addOrSeen(code))
        -1
      else
        t.##.abs % numHashFeatures + baseIndex.size
    case x => x
  }

  def unapply(i: Int): Option[T] = {
    if (i < baseIndex.size) baseIndex.unapply(i)
    else if (i < size) Some(hashWrapper(i - baseIndex.size))
    else  None
  }

  def pairs: Iterator[(T, Int)] = baseIndex.pairs ++ Iterator.range(0, numHashFeatures).map(i => hashWrapper(i) -> (i+baseIndex.size))

  def iterator: Iterator[T] =  baseIndex.iterator ++ Iterator.range(0, numHashFeatures).map(i => hashWrapper(i))
}
