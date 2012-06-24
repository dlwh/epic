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

/**
 *
 * @author dlwh
 */

trait SpanFeature extends Feature

object StandardSpanFeatures {
  case class SpanLengthFeature(dist: Int) extends SpanFeature
  case class EndSentFeature[L](label: L) extends SpanFeature
  case class BeginSentFeature[L](label: L) extends SpanFeature
  // Huang's WordEdges Feature without distance
  case class WordEdges[L, W](label: L, left: W, right: W) extends SpanFeature
  case class ShortUnary[ W](rule: Int, w: W) extends SpanFeature
}

