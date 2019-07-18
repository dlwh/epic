package epic.features

import breeze.util.Index

import scala.collection.mutable

/**
 * TODO
 *
 * @author dlwh
 **/
class NonRedundantIndexBuilder[F]  extends IndexBuilder[F]  {

  def result():Index[F] = {
    val result = Index[F]()
    val seenContexts = collection.mutable.Set[mutable.Set[Int]]()

    for(f <- 0 until allSeenFeatures.size) {
      val c = contexts(f)
      if (!c.exists(seenContexts)) {
        c.foreach(seenContexts += _)
        result.index(allSeenFeatures.get(f))
      }
    }

    result
  }

  private var allSeenFeatures = Index[F]()
  // None means that we've already committed to having this feature, which we don't use atm, but i mean to
  private var contexts = mutable.ArrayBuffer[Option[mutable.Set[Int]]]()

  private var nextContext: Int = 0


  def add(featuresForContext: TraversableOnce[F]):Unit = {

    for(x <- featuresForContext) {
      val next = allSeenFeatures.index(x)
      if (contexts.length <= next) {
        contexts += Some(mutable.Set[Int](nextContext))
      } else {
        contexts(next).foreach(_ += nextContext)
      }
    }

    nextContext += 1
  }


}
