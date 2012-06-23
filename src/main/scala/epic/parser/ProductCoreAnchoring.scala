package epic.parser

/**
 * Creates a product of two [[epic.parser.CoreAnchoring]]s (product is in log space, so actually a sum.)
 *
 * @author dlwh
 */

final case class ProductCoreAnchoring[L, W](s1: CoreAnchoring[L, W],
                                            s2: CoreAnchoring[L, W],
                                            alpha: Double = 1.0) extends CoreAnchoring[L, W] {
  val grammar = s1.grammar

  def lexicon = s1.lexicon

  def words = s1.words

  def scoreSpan(begin: Int, end: Int, label: Int) = {
    val r1 = s1.scoreSpan(begin, end, label)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreSpan(begin, end, label)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val r1 = s1.scoreBinaryRule(begin, split, end, rule)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreBinaryRule(begin, split, end, rule)
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val r1 = s1.scoreUnaryRule(begin, end, rule)
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreUnaryRule(begin, end, rule)
  }

}


