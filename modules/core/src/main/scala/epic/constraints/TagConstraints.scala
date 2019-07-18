package epic.constraints

import breeze.util.Index

/**
 *
 * @author dlwh
 */
trait TagConstraints[L] {
  def length: Int
  def allowedTags(pos: Int):Set[Int]
}

object TagConstraints {
  trait Factory[L, W] {
    def labelIndex: Index[L]
    def anchor(words: IndexedSeq[W]):TagConstraints[L]
  }
}
