package epic.features

import epic.framework.Feature
import breeze.linalg.Counter
import scala.Array
import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
case class FirstWordCapsAnd(f: Feature) extends Feature
@SerialVersionUID(1L)
case class NthWordCapsAnd(f: Feature) extends Feature
@SerialVersionUID(1L)
case class SentenceLengthFeature(length: Int) extends Feature
@SerialVersionUID(1L)
case object WholeSentenceIsUpperCaseFeature extends Feature
@SerialVersionUID(1L)
case class WordFeature(word: Any, kind: Symbol) extends Feature

@SerialVersionUID(1L)
case object BoundaryFeature extends Feature

trait SpanFeature extends Feature

@SerialVersionUID(1L)
object StandardSpanFeatures extends Serializable {
  @SerialVersionUID(1L)
  case class WordBoundary[L, W](label: L, w: W) extends SpanFeature
  // Huang's WordEdges Feature without distance
  @SerialVersionUID(1L)
  case class WordEdges[L, W](label: L, left: W, right: W) extends SpanFeature
  @SerialVersionUID(1L)
  case class ShortUnary[ W](rule: Int, w: W) extends SpanFeature
}
