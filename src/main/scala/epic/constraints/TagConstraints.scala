package epic.constraints

/**
 *
 * @author dlwh
 */
trait TagConstraints[L] {
  def length: Int
  def allowedTags(pos: Int):Set[Int]
}
