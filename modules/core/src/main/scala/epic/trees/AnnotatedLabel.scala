package epic.trees
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
import breeze.util.{CachedHashCode, Interner, Lens}
import epic.framework.Feature

import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

/**
 * Something we can throw in an AnnotatedLabel
 * @author dlwh
 */
@SerialVersionUID(1L)
trait Annotation extends Serializable

case class FunctionalTag(tag: String) extends Annotation

/**
 * The standard label used in the parser (used to be String).
 *
 * Useful for Klein-and-Manning style annotated labels and other explicit-annotation strategies
 * @author dlwh
 */
@SerialVersionUID(1L)
case class AnnotatedLabel(label: String,
                          headTag: Option[String] = None,
                          parents: IndexedSeq[String] = IndexedSeq.empty,
                          siblings: IndexedSeq[Either[String, String]] = IndexedSeq.empty,
                          features: Set[Annotation] = Set.empty,
                          index: Int = -1) extends Feature with CachedHashCode {
  def hasAnnotation(f: Annotation): Boolean = features.contains(f)

  def annotate(sym: Annotation*) = copy(features = features ++ sym)

  def isIntermediate = label.nonEmpty && label.charAt(0) == '@'

  def baseLabel = label.dropWhile(_ == '@')

  def baseAnnotatedLabel = AnnotatedLabel(label)
  def clearFeatures = copy(features=Set.empty)

  def treebankString = {
    var x = (label +: features.collect{ case t: FunctionalTag => t.tag}.toIndexedSeq).mkString("-")
    if (index != -1)  {
      x += s"-$index"
    }
    x
  }

  override def toString = {
    val components = new ArrayBuffer[String]()
    headTag.foreach(components += _)
    if (parents.nonEmpty) {
      components += parents.mkString("^","^","")
    }
    if (siblings.nonEmpty) {
      val b = new StringBuilder()
      siblings foreach {
        case Left(sib) =>
          b ++= "\\"
          b ++= sib
        case Right(sib) =>
          b ++= "/"
          b ++= sib
      }
      components += b.toString
    }
    if (features.nonEmpty)
      components ++= features.iterator.map(_.toString)

    if (index != -1)
      components += s"_$index"

    if (components.nonEmpty) components.mkString(label+"[", ", ", "]")
    else label
  }
}

object AnnotatedLabel {

  def parseTreebank(label: String):AnnotatedLabel = try {

    var fields = if (label == "PRT|ADVP") {
      Array("PRT")
    } else if (label.startsWith("-") || label.isEmpty || label == "#") {
      Array(label)
    } else if (label.contains("##")) { // SPMRL uses two ## as the delimiter for this info
      val splits = label.split("##").filter(_.nonEmpty)
      val nonmorphSplits = splits.head.split("[-=]")
      val morphSplits = splits.tail.flatMap(_.split("[|]")).filter("_" != _)
      nonmorphSplits ++ morphSplits
    } else {
      label.split("[-=#]")
    }

    if (label.isEmpty) return AnnotatedLabel.TOP

    val tag = fields.head

    fields = fields.drop(1)

    // TODO: what to do with =
    val (coref, realFields) = fields.partition(l => l.charAt(0).isDigit)

    val index = Try { coref.last.toInt }.toOption.getOrElse(-1)

    val lbl = interner(AnnotatedLabel(tag, index = index).annotate(realFields.map(FunctionalTag).map(functionalTagInterner):_*))

    lbl
  } catch {
    case ex: Exception => throw new RuntimeException("while dealing with the label " + label, ex)
  }

  def apply(label: String):AnnotatedLabel = {
    lblCache.getOrElseUpdate(label, interner.intern(new AnnotatedLabel(label)))
  }

  private val lblCache = new java.util.concurrent.ConcurrentHashMap[String, AnnotatedLabel]().asScala
  private val interner: Interner[AnnotatedLabel] = new Interner[AnnotatedLabel]()
  private val functionalTagInterner = new Interner[FunctionalTag]

  val TOP = AnnotatedLabel("TOP")

  implicit val stringLens:Lens[AnnotatedLabel, String] = new Lens[AnnotatedLabel, String] with Serializable {
    def get(t: AnnotatedLabel) = t.label
    def set(t: AnnotatedLabel, u: String) = t.copy(u)
  }
}
