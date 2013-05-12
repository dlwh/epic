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
import epic.framework.Feature

trait LexFeature extends Feature

case class HeadFeature[P](r: Feature, head: P) extends LexFeature

case class DepFeature[P](r: Feature, dep: P) extends LexFeature
case class HeadDepFeature[P](r: Feature, head: P, dep: P) extends LexFeature

case class BilexicalFeature[W](head: W, dep: W, dir: Symbol) extends LexFeature
case class TagFeature[L, W](tag: L, dep: W) extends LexFeature
case class DistFeature(dist: Int, f: Feature) extends LexFeature
case class PartFeature[P](feature: Feature, part: P) extends LexFeature


case object LowCount extends LexFeature

