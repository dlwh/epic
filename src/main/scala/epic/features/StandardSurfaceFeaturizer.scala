package epic.features

import epic.framework.Feature
import breeze.linalg.Counter
import scala.Array
import scala.collection.mutable.ArrayBuffer
import StandardSpanFeatures._

case class FirstWordCapsAnd(f: Feature) extends Feature
case class NthWordCapsAnd(f: Feature) extends Feature
case class SentenceLengthFeature(length: Int) extends Feature
case object WholeSentenceIsUpperCaseFeature extends Feature
case class WordFeature(word: Any, kind: Symbol) extends Feature

case object BoundaryFeature extends Feature

trait SpanFeature extends Feature

object StandardSpanFeatures {
  case class WordBoundary[L, W](label: L, w: W) extends SpanFeature
  // Huang's WordEdges Feature without distance
  case class WordEdges[L, W](label: L, left: W, right: W) extends SpanFeature
  case class ShortUnary[ W](rule: Int, w: W) extends SpanFeature
}
