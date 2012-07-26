package epic.coref

import epic.framework.Feature

/**
 *
 * @author dlwh
 */
object Features {
}

case class PairFeature(a: AnyRef, b: AnyRef) extends Feature
case class Not(a: AnyRef) extends Feature

/** String match features */
case object ExactStringMatch extends Feature
case object LowerStringMatch extends Feature
case object ContainsString extends Feature
case class PartialStringMatch(kind: Symbol) extends Feature
case object NoOverlap extends Feature
case object HeadMatch extends Feature


/** Distance Features */
case class SentenceDistFeature(bin: Int) extends Feature
