package epic

import breeze.util.Index

import scala.collection.mutable

/**
 * TODO
 *
 * @author dlwh
 **/
package object features {

  def buildNonRedundantFeatureIndex[T, F](it: TraversableOnce[T], gen: T=>TraversableOnce[F]):Index[F] = {
    // TODO: I should figure out how to one pass this
    val index = Index[F]()
    val contexts = new mutable.HashMap[F, mutable.Set[T]] with mutable.MultiMap[F, T]
    for (t <- it) {
      val genned = gen(t).toSet
      for(v <- genned) {
        contexts.addBinding(v, t)
      }
    }

    val uniqueContextsToFeatures = contexts.keys.groupBy(contexts)

    uniqueContextsToFeatures.values.map(_.head).foreach(index.index)

    index
  }

}
