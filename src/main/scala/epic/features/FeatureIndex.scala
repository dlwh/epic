package epic.features

import breeze.util.Index
import epic.framework.{VisitableMarginal, Feature}
import scala.collection.mutable
import breeze.collection.mutable.OpenAddressHashArray
import scala.util.hashing.MurmurHash3
import scala.collection.mutable.ArrayBuffer
import epic.util.Arrays

case class LabeledFeature[B](labelPart: Feature, surfacePart: B, id: String = "") extends Feature {
  override def toString = s"${if(id.nonEmpty) id else "CrossProduct"}Feature($labelPart, $surfacePart)"
}

/**
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
class FeatureIndex[B](val labelFeatureIndex: Index[Feature],
                      val surfaceFeatureIndex: Index[B],
                      mapping: Array[OpenAddressHashArray[Int]],
                      labelPartOfFeature: Array[Int],
                      surfacePartOfFeature: Array[Int],
                      id: String= "FeatureIndex",
                      includePlainLabelFeatures: Boolean = true,
                      val numHashFeatures: Int=0) extends Index[Feature] with Serializable {

  def apply(t: Feature): Int = t match {
    case LabeledFeature(a,b, `id`) =>
      mapped(labelFeatureIndex(a), surfaceFeatureIndex(b.asInstanceOf[B]))
    case HashFeature(x) =>
      x + trueSize
    case x if includePlainLabelFeatures => labelFeatureIndex(x)
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


  private def labelOnlySize: Int = if(includePlainLabelFeatures) labelFeatureIndex.size else 0
  private def trueSize = labelOnlySize + labelPartOfFeature.length
  override def size: Int = trueSize + numHashFeatures

  def unapply(i: Int): Option[Feature] = if(i >= size || i < 0)  None else Some(get(i))

  override def get(i: Int): Feature = {
    if(i >= size || i < 0) {
      throw new NoSuchElementException(s"index $i is not in FeatureIndex of size $size")
    } else if(i < labelOnlySize) {
      labelFeatureIndex.get(i)
    } else if (i < trueSize) {
      LabeledFeature(labelFeatureIndex.get(labelPartOfFeature(i-labelOnlySize)), surfaceFeatureIndex.get(surfacePartOfFeature(i-labelOnlySize)), id)
    } else {
      HashFeature(i - trueSize)
    }
  }

  def pairs: Iterator[(Feature, Int)] = Iterator.range(0,size).map(i => get(i) -> i)

  def iterator: Iterator[Feature] = Iterator.range(0,size).map(i => get(i))

  def crossProduct(lFeatures: Array[Int], sFeatures: Array[Int], usePlainLabelFeatures: Boolean = true):Array[Int] = {
    val builder = new mutable.ArrayBuilder.ofInt
    builder.sizeHint(lFeatures.length * (sFeatures.length +1))
    var i = 0
    while(i < lFeatures.length) {
      if(usePlainLabelFeatures && includePlainLabelFeatures && lFeatures(i) >= 0)
        builder += lFeatures(i)
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

  def stripEncode(features: Array[Feature]) = {
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
  def build[B](labelFeatureIndex: Index[Feature],
               surfaceFeatureIndex: Index[B],
               hashFeatures: HashFeature.Scale = HashFeature.Absolute(0),
               id: String = "FeatureIndex",
               includeLabelOnlyFeatures: Boolean = true)(pairEnumerator: ((Array[Int],Array[Int])=>Array[Int])=>Unit) = {
    val mapping = Array.fill(labelFeatureIndex.size)(new OpenAddressHashArray[Int](surfaceFeatureIndex.size, -1, 4))
    val labelPart, surfacePart = new ArrayBuffer[Int]()
    val labelOnlySize: Int = if(includeLabelOnlyFeatures) labelFeatureIndex.size else 0
    def next = labelPart.size + labelOnlySize
    pairEnumerator {(lParts, sParts) =>
      Arrays.crossProduct(lParts, sParts) { (lPart, sPart) =>
        val currentIndex: Int = mapping(lPart)(sPart)
        if(currentIndex == -1) {
          val x = next
          mapping(lPart)(sPart) = x
          labelPart += lPart
          surfacePart += sPart
          x
        } else {
          currentIndex
        }
      }
    }
    new FeatureIndex(labelFeatureIndex,
      surfaceFeatureIndex,
      mapping,
      labelPart.toArray, surfacePart.toArray,
      id, includeLabelOnlyFeatures,
      hashFeatures.numFeatures(labelPart.length))
  }
}
