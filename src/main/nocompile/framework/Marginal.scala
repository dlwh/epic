package epic.framework

/**
 * Marginals are created by [[epic.framework.Inference]] objects and used for expected counts and
 * decoding, where applicable. Max Marginals (i.e. a one best list) are totally fine if you want
 * to do max-margin work.
 *
 * They only have one method: logPartition. Most of the work is done with Inference objects directly.
 *
 * @author dlwh
 */
trait Marginal {
  def logPartition: Double
}

trait VisitableMarginal[Visitor] extends Marginal {
  def visit(visitor: Visitor)
}
