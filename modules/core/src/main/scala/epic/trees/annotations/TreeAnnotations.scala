package epic.trees
package annotations
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


/** TODO */
object TreeAnnotations {

  case class HeadTagAnnotation(tag: String) extends Annotation

  // KM Annotations
  trait KMAnnotation extends Annotation

  trait Aux extends KMAnnotation
  case object AuxBe extends Aux
  case object Aux extends Aux
  case object AuxHave extends Aux

  case object VPisVBF extends KMAnnotation
  case class VPisX(x: String) extends KMAnnotation

  sealed trait INKind extends KMAnnotation
  case object IN_N extends INKind
  case object IN_Q extends INKind
  case object IN_SCC extends INKind
  case object IN_SC extends INKind

  case object NP_Possessive extends KMAnnotation
  case object BaseNP extends KMAnnotation
  case object RightRecNP extends KMAnnotation

  case object DomCCRight extends KMAnnotation
  case object DomCCLeft extends KMAnnotation

  case object RealUnary extends KMAnnotation
  case object ExternalUnary extends KMAnnotation

  case class Dom(str: String) extends KMAnnotation
}
