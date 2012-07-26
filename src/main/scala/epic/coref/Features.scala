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

// String Match Features
case object ExactStringMatch extends Feature
case object LowerStringMatch extends Feature
case object ContainsString extends Feature
case class PartialStringMatch(kind: Symbol) extends Feature
case object NoOverlap extends Feature


// Heads
case object HeadMatch extends Feature
case object HeadStemMatch extends Feature
case object ContainsHeadR extends Feature
case object ContainsHeadL extends Feature
case object CoordinationL extends Feature
case object CoordinationR extends Feature



// Discourse Features
case class SentenceDistance(bin: Int) extends Feature

// Other features
case object SpanContained extends Feature

