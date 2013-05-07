package epic.pruning

/**
 *
 * @author dlwh
 */
trait TagConstraints[L] {
  def allowedTags(pos: Int):Set[Int]
}
