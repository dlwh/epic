package epic.parser.features

/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import epic.trees.Rule
import breeze.util.CachedHashCode
import epic.framework._

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

case class PairFeature(a: Feature, b: Any) extends Feature with CachedHashCode

case class LabelFeature[L](l: L) extends Feature with CachedHashCode