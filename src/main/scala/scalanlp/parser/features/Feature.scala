package scalanlp.parser.features

import scalanlp.trees.Rule
import scalanlp.util.CachedHashCode
import scalanlp.epic._

// Meta Features
/** conjoins some features */
case class SequenceFeature(f: Seq[Feature]) extends Feature with CachedHashCode
/** Associate a symbol with a feature */
case class TaggedFeature(f: Feature, symbol: Symbol) extends Feature with CachedHashCode

// Parer Features
/** A Rule feature is just an indicator on there being this rule */
case class RuleFeature[L](r: Rule[L]) extends Feature with CachedHashCode
/** A simple indicator feature */
case class WeightedFeature(kind: Symbol) extends Feature with CachedHashCode
/** A Lexical feature is just an indicator on there being this word */
case class LexicalFeature[L,W](l: L, w: W) extends Feature with CachedHashCode
/** Wraps a feature with substate information */
case class SubstateFeature[T](f: Feature, states: Seq[T]) extends Feature with CachedHashCode

case class IndicatorFeature(a: Any) extends Feature with CachedHashCode

case class PairFeature(a: Feature, b: Feature) extends Feature with CachedHashCode