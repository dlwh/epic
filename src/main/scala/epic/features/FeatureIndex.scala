package epic.features

import breeze.util.Index
import epic.framework.{VisitableMarginal, Feature}
import scala.collection.mutable
import breeze.collection.mutable.OpenAddressHashArray
import scala.util.hashing.MurmurHash3
import scala.collection.mutable.ArrayBuffer
import epic.util.Arrays

case class LabeledFeature[A, B](labelPart: A, surfacePart: B) extends Feature

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FeatureIndex[A, B](val labelFeatureIndex: Index[A],
                         val surfaceFeatureIndex: Index[B],
                         mapping: Array[OpenAddressHashArray[Int]],
                         labelPartOfFeature: Array[Int],
                         surfacePartOfFeature: Array[Int],
                         val numHashFeatures: Int=0) extends Index[Feature] with Serializable {

  def apply(t: Feature): Int = t match {
    case LabeledFeature(a,b) =>
      mapped(labelFeatureIndex(a.asInstanceOf[A]), surfaceFeatureIndex(b.asInstanceOf[B]))
    case HashFeature(x) =>
      x + trueSize
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
      } else if(numHashFeatures == 1) {
        trueSize
      } else {
        val hf = MurmurHash3.mixLast(MurmurHash3.mix(10891, labelFeature.##), surfaceFeature.##).abs % numHashFeatures
        hf + trueSize
      }
    }

  }


  private def trueSize = labelPartOfFeature.length
  override def size: Int = trueSize + numHashFeatures

  def unapply(i: Int): Option[Feature] = if(i >= size || i < 0)  None else Some(get(i))

  override def get(i: Int): Feature = {
    if(i >= size || i < 0) {
      throw new NoSuchElementException(s"index $i is not in FeatureIndex of size $size")
    } else if (i < trueSize) {
      LabeledFeature(labelFeatureIndex.get(labelPartOfFeature(i)), surfaceFeatureIndex.get(surfacePartOfFeature(i)))
    } else {
      HashFeature(i - trueSize)
    }
  }

  def pairs: Iterator[(Feature, Int)] = Iterator.range(0,size).map(i => get(i) -> i)

  def iterator: Iterator[Feature] = Iterator.range(0,size).map(i => get(i))

  def crossProduct(lFeatures: Array[Int], sFeatures: Array[Int]):Array[Int] = {
    val builder = new mutable.ArrayBuilder.ofInt
    builder.sizeHint(lFeatures.length * sFeatures.length)
    var i = 0
    while(i < lFeatures.length) {
      var j = 0
      while(j < sFeatures.length) {
        val m = mapped(lFeatures(i),sFeatures(j))
        if(m != -1)
          builder += m
        j += 1
      }

      i += 1
    }

    builder.result()
  }

  def stripEncode(features: Array[Feature]) = if(numHashFeatures > 0) {
    val result = new Array[Int](features.length)
    var i = 0
    while(i < features.length) {
      result(i) = apply(features(i))
      i += 1
    }
    result
  } else {
    val result = mutable.ArrayBuilder.make[Int]()
    result.sizeHint(features)
    var i = 0
    while(i < features.length) {
      val fi = apply(features(i))
      if(fi >= 0)
        result += fi
      i += 1
    }
    result.result()
  }
}

object FeatureIndex {
  def build[A, B](labelFeatureIndex: Index[A],
                  surfaceFeatureIndex: Index[B],
                  hashFeatures: HashFeature.Scale = HashFeature.Absolute(0))(pairEnumerator: ((Array[Int],Array[Int])=>Array[Int])=>Unit) = {
    val mapping = Array.fill(labelFeatureIndex.size)(new OpenAddressHashArray[Int](surfaceFeatureIndex.size, -1, 4))
    val labelPart, surfacePart = new ArrayBuffer[Int]()
    pairEnumerator {(lParts, sParts) =>
      Arrays.crossProduct(lParts, sParts) { (lPart, sPart) =>
        val currentIndex: Int = mapping(lPart)(sPart)
        if(currentIndex == -1) {
          val next = labelPart.size
          mapping(lPart)(sPart) = next
          labelPart += lPart
          surfacePart += sPart
          next
        } else {
          currentIndex
        }
      }
    }
    new FeatureIndex(labelFeatureIndex,
      surfaceFeatureIndex,
      mapping,
      labelPart.toArray, surfacePart.toArray,
      hashFeatures.numFeatures(labelPart.length))
  }
}
