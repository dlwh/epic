package epic.features

import breeze.collection.mutable.OpenAddressHashArray
import breeze.linalg.{ CSCMatrix, DenseVector, SparseVector, VectorBuilder }
import breeze.util.{ Index, SerializableLogging }
import epic.framework.Feature
import epic.util.{ Arrays, LockableSeenSet }

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.hashing.MurmurHash3

@SerialVersionUID(1743448091752596096L)
case class CrossProductFeature[A, B](labelPart: A, surfacePart: B, id: String = "") extends Feature {
  override def toString = s"${if (id.nonEmpty) id else "CrossProduct"}Feature($labelPart, $surfacePart)"
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
  def surfacePart(i: Int) = surfacePartOfFeature(i - labelOnlySize)
  def labelPart(i: Int) = labelPartOfFeature(i - labelOnlySize)

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

  def mapped(labelFeature: Int, surfaceFeature: Int): Int = {
    if (labelFeature < 0 || surfaceFeature < 0) {
      -1
    } else {
      val arr = mapping(labelFeature)
      val f = if (arr ne null) {
         arr(surfaceFeature)
      } else {
        -1
      }
      if (f != -1 || numHashFeatures == 0) {
        f
      } else if (f < -1) { // really not present
        -1
      } else {
        val hf = MurmurHash3.mixLast(MurmurHash3.mix(10891, labelFeature.##), surfaceFeature.##).abs
        if (!seenSet.addOrSeen(hf)) {
          -1
        } else {
          (hf % numHashFeatures) + trueSize
        }
      }
    }
  }

  private val labelOnlySize: Int = if (includePlainLabelFeatures) firstIndex.size else 0
  private val trueSize = labelOnlySize + labelPartOfFeature.length
  override def size: Int = trueSize + numHashFeatures

  def unapply(i: Int): Option[Feature] = if (i >= size || i < 0)  None else Some(get(i))

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
    builder.sizeHint(lFeatures.length * (sFeatures.length + {if (includePlainLabelFeatures) 1 else 0}))
    var i = 0
    while (i < lFeatures.length) {
      if (usePlainLabelFeatures && includePlainLabelFeatures && lFeatures(i) >= 0)
        builder += (lFeatures(i) + offset)
      var j = 0
      while (j < sFeatures.length) {
        val m = mapped(lFeatures(i),sFeatures(j)) + offset
        if (m != -1)
          builder += m
        j += 1
      }

      i += 1
    }

    builder.result()
  }

  def buildSparseMatrix(weights: DenseVector[Double]):(CSCMatrix[Double], SparseVector[Double]) = {
    val builder = new CSCMatrix.Builder[Double](firstIndex.size, secondIndex.size)
    val vbuilder = new VectorBuilder[Double](firstIndex.size)

    if (includePlainLabelFeatures) {
      for(i <- 0 until firstIndex.size) {
        val w = weights(i)
        if (w != 0.0)
          vbuilder.add(i, w)
      }
    }

    if (numHashFeatures == 0) {
      // if no hash features, we can just iterate over the enumerated part of the index
      for(((l, s), i) <- (labelPartOfFeature zip surfacePartOfFeature).zipWithIndex) {
        val w = weights(i + labelOnlySize)
        if (w != 0.0)
          builder.add(l, s, w)
      }
    } else {
      // otherwise, check everything
      for(l <- 0 until firstIndex.size; s <- 0 until secondIndex.size) {
        val i = mapped(l, s)
        if (i >= 0 && weights(i) != 0) {
          builder.add(l, s, weights(i))
        }
      }
    }

    (builder.result(), vbuilder.toSparseVector(true, true))
  }

  /**
   * Gets rid of crossproduct features (i.e. not hash, not label) that are shouldPrune.
   *
   * Optionally, we can also build a new surfaceFeature index with only those surface features
   * that are used in some part of the new cross product.
   *
   * (Implementation note: the resulting mapping may contain -1's for elements of the cross product that
   * have been removed. This is to note that this feature is known and the hash code should not be computed.)
   *
   * @return
   */
  def prune(shouldPrune: Int=>Boolean, rebuildSurfaceIndex: Boolean = true):CrossProductIndex[A, B] = {
    val newSecondIndex = Index[B]()
    def newIndexOf(b: Int) = if (rebuildSurfaceIndex) newSecondIndex.index(secondIndex.get(b)) else b
    def alreadyInNewIndex(b: Int) = !rebuildSurfaceIndex || newSecondIndex.contains(secondIndex.get(b))
    val mapping = Array.fill(firstIndex.size)(new OpenAddressHashArray[Int](secondIndex.size max 1, -1, 4))
    val newLabelPart, newSurfacePart = new ArrayBuffer[Int]()
    val possibleUnneededBs = new CSCMatrix.Builder[Boolean](firstIndex.size, secondIndex.size)

    def size = newLabelPart.length + labelOnlySize

    for(i <- labelOnlySize until trueSize) {

      val bPart: Int = surfacePart(i)
      val aPart: Int = labelPart(i)
      if (!shouldPrune(i)) {
        val x = size
        val newA = aPart
        val newB = newIndexOf(bPart)
        newLabelPart += newA
        newSurfacePart += newB
        mapping(newA)(newB) = x
      } else if (alreadyInNewIndex(bPart)) {
        // if the surface part has already been used by some real feature we're keeping,
        // then we have to mark it as -1 so we don't try to hash it later.
        val newA = aPart
        val newB = newIndexOf(bPart)
        mapping(newA)(newB) = -2
      } else {
        possibleUnneededBs.add(aPart, bPart, true)
      }
    }

    // todo: add an iterator method to cscmatrix builder
    for ( ((a, b), _) <- possibleUnneededBs.result.activeIterator if alreadyInNewIndex(b)) {
      mapping(a)(newIndexOf(b)) = -1
    }

    new CrossProductIndex(firstIndex,
      if (rebuildSurfaceIndex) newSecondIndex else secondIndex,
      mapping,
      newLabelPart.toArray, newSurfacePart.toArray,
      id, includePlainLabelFeatures,
      numHashFeatures, seenSet)
  }
}

object CrossProductIndex {

  /**
   *
   * @param firstIndex
   * @param secondIndex
   * @param hashFeatures
   * @param id
   * @param includeLabelOnlyFeatures
   * @param minCount minimum count needed to keep a feature
   * @param seenSet
   * @tparam A
   * @tparam B
   */
  class Builder[A, B](firstIndex: Index[A],
                      secondIndex: Index[B],
                      hashFeatures: HashFeature.Scale = HashFeature.Absolute(0),
                      id: String = "CrossProductIndex",
                      val includeLabelOnlyFeatures: Boolean = true,
                      minCount: Int = 1,
                      seenSet: LockableSeenSet[Long] = LockableSeenSet.always) extends SerializableLogging {
    def add(a: A, b: B): Int = add(firstIndex(a), secondIndex(b))

    private val counts = Array.fill(firstIndex.size)(new OpenAddressHashArray[Int](secondIndex.size max 1, 0, 4))
    private val mapping = Array.fill(firstIndex.size)(new OpenAddressHashArray[Int](secondIndex.size max 1, -1, 4))
    private val labelPart, surfacePart = new ArrayBuffer[Int]()
    private val labelOnlySize: Int = if (includeLabelOnlyFeatures) firstIndex.size else 0

    def size = labelPart.size + labelOnlySize

    def add(firstArray: Array[Int], secondArray: Array[Int]):Array[Int] = {
      Arrays.crossProduct(firstArray, secondArray)(add)
    }

    def add(first: Int, secondArray: Array[Int]):Array[Int] = {
      secondArray.map(add(first, _))
    }

    def add(first: Int, second: Int): Int = {
      if (first < 0 || second < 0) {
        -1
      } else {
        val currentIndex: Int = mapping(first)(second)
        if (currentIndex == -1) {
          val currentCount = counts(first)(second)
          if (minCount <= 1 || currentCount + 1 >= minCount) {
            val x = size
            mapping(first)(second) = x
            labelPart += first
            surfacePart += second
            x
          } else {
            counts(first)(second) = currentCount + 1
            currentIndex
          }
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
