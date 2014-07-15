package epic.features

import breeze.util.Index
import epic.framework.{VisitableMarginal, Feature}
import scala.collection.mutable
import breeze.collection.mutable.OpenAddressHashArray
import scala.util.hashing.MurmurHash3
import scala.collection.mutable.ArrayBuffer
import epic.util.{LockableSeenSet, Arrays}
import com.typesafe.scalalogging.slf4j.{LazyLogging, Logger}
import epic.util.SafeLogging

@SerialVersionUID(1743448091752596096L)
case class CrossProductFeature[A, B](labelPart: A, surfacePart: B, id: String = "") extends Feature {
  override def toString = s"${if(id.nonEmpty) id else "CrossProduct"}Feature($labelPart, $surfacePart)"
}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class CrossProductIndex[A, B] private (val firstIndex: Index[A],
                                       val secondIndex: Index[B],
                                       mapping: Array[OpenAddressHashArray[Int]],
                                       labelPartOfFeature: Array[Int],
                                       surfacePartOfFeature: Array[Int],
                                       id: String= "CrossProductIndex",
                                       val includePlainLabelFeatures: Boolean = true,
                                       val numHashFeatures: Int=0,
                                       seenSet: LockableSeenSet[Long] = LockableSeenSet.always) extends Index[Feature] with Serializable {

  def lock = {
    val lockedFirst: Index[A] = firstIndex match {
      case x: HashExtendingIndex[A] => x.lock
      case _ => firstIndex
    }

    val lockedSecond: Index[B] = secondIndex match {
      case x: HashExtendingIndex[B] => x.lock
      case _ => secondIndex
    }

    new CrossProductIndex(lockedFirst, lockedSecond, mapping, labelPartOfFeature,
                                   surfacePartOfFeature, id, includePlainLabelFeatures,
                                   numHashFeatures, seenSet.lock)
  }

  def apply(t: Feature): Int = t match {
    case CrossProductFeature(a,b, `id`) =>
      mapped(firstIndex(a.asInstanceOf[A]), secondIndex(b.asInstanceOf[B]))
    case HashFeature(x) =>
      x + trueSize
    case LabelFeature(x) if includePlainLabelFeatures => firstIndex(x.asInstanceOf[A])
    case _ => -1
  }

  def mapped(labelFeature: Int, surfaceFeature: Int):Int = {
    if(labelFeature < 0 || surfaceFeature < 0) {
      -1
    } else {
      val arr = mapping(labelFeature)
      val f = if(arr ne null) {
        arr(surfaceFeature)
      } else {
        -1
      }

      if(f >= 0 || numHashFeatures == 0) {
        f
      } else {
        val hf = MurmurHash3.mixLast(MurmurHash3.mix(10891, labelFeature.##), surfaceFeature.##).abs
        if(!seenSet.addOrSeen(hf)) {
          -1
        } else {
          (hf % numHashFeatures) + trueSize
        }
      }
    }

  }


  private val labelOnlySize: Int = if(includePlainLabelFeatures) firstIndex.size else 0
  private val trueSize = labelOnlySize + labelPartOfFeature.length
  override def size: Int = trueSize + numHashFeatures

  def unapply(i: Int): Option[Feature] = if(i >= size || i < 0)  None else Some(get(i))

  override def get(i: Int): Feature = {
    if (i >= size || i < 0) {
      throw new NoSuchElementException(s"index $i is not in CrossProductIndex of size $size")
    } else if (i < labelOnlySize) {
      LabelFeature(firstIndex.get(i))
    } else if (i < trueSize) {
      CrossProductFeature(firstIndex.get(labelPartOfFeature(i-labelOnlySize)), secondIndex.get(surfacePartOfFeature(i-labelOnlySize)), id)
    } else {
      HashFeature(i - trueSize)
    }
  }

  def pairs: Iterator[(Feature, Int)] = Iterator.range(0,size).map(i => get(i) -> i)

  def iterator: Iterator[Feature] = Iterator.range(0,size).map(i => get(i))

  def crossProduct(lFeatures: Array[Int], sFeatures: Array[Int], offset: Int = 0, usePlainLabelFeatures: Boolean = true):Array[Int] = {
    val builder = new mutable.ArrayBuilder.ofInt
    builder.sizeHint(lFeatures.length * (sFeatures.length + {if(includePlainLabelFeatures) 1 else 0}))
    var i = 0
    while(i < lFeatures.length) {
      if(usePlainLabelFeatures && includePlainLabelFeatures && lFeatures(i) >= 0)
        builder += (lFeatures(i) + offset)
      var j = 0
      while(j < sFeatures.length) {
        val m = mapped(lFeatures(i),sFeatures(j)) + offset
        if(m != -1)
          builder += m
        j += 1
      }

      i += 1
    }

    builder.result()
  }
}

object CrossProductIndex {

  class Builder[A, B](firstIndex: Index[A],
                      secondIndex: Index[B],
                      hashFeatures: HashFeature.Scale = HashFeature.Absolute(0),
                      id: String = "CrossProductIndex",
                      val includeLabelOnlyFeatures: Boolean = true,
                      seenSet: LockableSeenSet[Long] = LockableSeenSet.always) extends SafeLogging {
    private val mapping = Array.fill(firstIndex.size)(new OpenAddressHashArray[Int](secondIndex.size max 1, -1, 4))
    private val labelPart, surfacePart = new ArrayBuffer[Int]()
    private val labelOnlySize: Int = if(includeLabelOnlyFeatures) firstIndex.size else 0

    def size = labelPart.size + labelOnlySize

    def add(firstArray: Array[Int], secondArray: Array[Int]):Array[Int] = {
      Arrays.crossProduct(firstArray, secondArray)(add)
    }

    def add(first: Int, secondArray: Array[Int]):Array[Int] = {
      secondArray.map(add(first, _))
    }

    def add(first: Int, second: Int):Int = {
      if(first < 0 || second < 0) {
        -1
      } else {
        val currentIndex: Int = mapping(first)(second)
        if(currentIndex == -1) {
          val x = size
          mapping(first)(second) = x
          labelPart += first
          surfacePart += second
          x
        } else {
          currentIndex
        }
      }
    }

    def result() = {
      new CrossProductIndex(firstIndex,
        secondIndex,
        mapping,
        labelPart.toArray, surfacePart.toArray,
        id, includeLabelOnlyFeatures,
        hashFeatures.numFeatures(labelPart.length), seenSet)
    }
  }

}
