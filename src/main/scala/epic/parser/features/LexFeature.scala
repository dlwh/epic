package epic.parser.features

import epic.framework.Feature

trait LexFeature extends Feature

case class HeadFeature[P](r: Feature, head: P) extends LexFeature

case class DepFeature[P](r: Feature, dep: P) extends LexFeature
case class HeadDepFeature[P](r: Feature, head: P, dep: P) extends LexFeature

case class BilexicalFeature[W](head: W, dep: W, dir: Symbol) extends LexFeature
case class TagFeature[L, W](tag: L, dep: W) extends LexFeature
case class DistFeature(dist: Int, f: Feature) extends LexFeature
case class PartFeature[P](feature: Feature, part: P) extends LexFeature

case class HashFeature(hashBucket: Int) extends LexFeature
case object LowCount extends LexFeature

