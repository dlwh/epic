package epic.features

import breeze.util.Index

/**
 * TODO
 *
 * @author dlwh
 **/
class HashExtendingIndex[T](val baseIndex: Index[T], hashWrapper: Int=>T, hashScale: HashFeature.Scale = HashFeature.Relative(1)) extends Index[T] {
  val numHashFeatures = hashScale.numFeatures(baseIndex.size)
  override def size: Int = baseIndex.size + numHashFeatures

  def apply(t: T): Int = baseIndex(t) match {
    case -1 => t.##.abs % numHashFeatures + baseIndex.size
    case x => x
  }

  def unapply(i: Int): Option[T] = {
    if(i < baseIndex.size) baseIndex.unapply(i)
    else if(i < size) Some(hashWrapper(i - baseIndex.size))
    else  None
  }

  def pairs: Iterator[(T, Int)] = baseIndex.pairs ++ Iterator.range(0, numHashFeatures).map(i => hashWrapper(i) -> (i+baseIndex.size))

  def iterator: Iterator[T] =  baseIndex.iterator ++ Iterator.range(0, numHashFeatures).map(i => hashWrapper(i))
}
