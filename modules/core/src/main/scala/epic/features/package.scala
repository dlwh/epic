package epic

import breeze.util.Index

/**
 * TODO
 *
 * @author dlwh
 **/
package object features {

  def buildNonRedundantFeatureIndex[T, F](it: TraversableOnce[T], gen: T=>TraversableOnce[F]):Index[F] = {
    val builder = new NonRedundantIndexBuilder[F]
    for(x <- it) {
      builder.add(gen(x))
    }
    builder.result()
  }

}
