package epic.everything

import epic.trees.{Span, AnnotatedLabel, Tree}
import breeze.data.Example
import epic.sequences.Segmentation
import collection.mutable.ArrayBuffer

/**
 * represents an annotation ontonotes sentence. Doesn't include raw sentence, for now.
 *
 * @author dlwh
 */
case class Sentence(docId: String, sentId: Int,
                   words: IndexedSeq[String],
                   annotations: OntoAnnotations) extends Example[OntoAnnotations,Seq[String]] {
  def id = docId +":"+sentId
  def features = words
  def label = annotations

  def length = words.length
  def tree = annotations.tree
  def ner = annotations.ner

  lazy val nerSegmentation: Segmentation[NERType.Value, String]  = {
    val sorted = ner.toIndexedSeq.sortBy((_: (DSpan, NERType.Value))._1.begin)
    var out = new ArrayBuffer[(NERType.Value, Span)]()
    var last = 0
    for( (dspan, label) <- sorted ) {
      assert(last <= dspan.begin)
      while(dspan.begin != last) {
        out += (NERType.NotEntity -> Span(last,last+1))
        last += 1
      }
      out += (label -> Span(dspan.begin, dspan.end))
      last = dspan.end
    }
    while(words.length != last) {
      out += (NERType.NotEntity -> Span(last,last+1))
      last += 1
    }

    Segmentation(out, words, id)
  }
  def coref = annotations.coref

  def annotate(tree: Tree[AnnotatedLabel] = tree,
               ner: Map[DSpan,NERType.Value] = ner,
               coref: Map[DSpan,Mention] = coref) = {
    copy(annotations=OntoAnnotations(tree,ner,coref))
  }

}

case class DSpan(doc: String, sent: Int, begin: Int, end: Int) {
  def span = Span(begin, end)

  override def toString = doc + ":" + sent + ":" + begin + "-" + end
}


case class OntoAnnotations(tree: Tree[AnnotatedLabel],
                           ner: Map[DSpan,NERType.Value],
                           coref: Map[DSpan,Mention])



/**
 * A coref mention
 */
case class Mention(id: Int, mentionType: MentionType = MentionType.Ident)

/**
 * A Propbank mention
 */
case class Frame(lemma: String, sense: Int, args: Seq[Argument])
case class Argument(arg: String, fillers: Seq[(Int,Int)]) // leaf:height pairs

/**
 * A wordnet sense
 */
case class Sense(lemma: String, sense: String, pos: String)

/**
 * The kinds of mentions used by Ontonotes
 */
sealed trait MentionType
object MentionType {
  def fromString(str: String): MentionType = str.toLowerCase match {
    case "ident" | "identity" => Ident
    case "head" | "appos head" | "apposhead" => ApposHead
    case "attrib" | "appos attrib" | "apposattrib" => ApposAttrib
  }
  case object Ident extends MentionType
  case object ApposHead extends MentionType
  case object ApposAttrib extends MentionType
}

/**
 * The NER types used in Ontonotes
 */
object NERType extends Enumeration {
  def fromString(str: String): NERType.Value = str.toLowerCase match {
    case "cardinal" => Cardinal
    case "date" => Date
    case "event" => Event
    case "fac" => Fac
    case "gpe" => GPE
    case "language" => Language
    case "law" => Law
    case "loc" => Loc
    case "money" => Money
    case "norp" => NORP
    case "ordinal" => Ordinal
    case "org" => Org
    case "percent" => Percent
    case "person" => Person
    case "product" => Product
    case "quantity" => Quantity
    case "time" => Time
    case "work_of_art" | "workofart" => WorkOfArt
    case "notentity" | "none" => NotEntity
    case "bos" | "eos" | "outsidesentence" => OutsideSentence
  }

  val Cardinal = Value
  val Date = Value
  val Event = Value
  val Fac = Value
  val GPE = Value
  val Language = Value
  val Law = Value
  val Loc = Value
  val Money = Value
  val NORP = Value
  val Ordinal = Value
  val Org = Value
  val Percent = Value
  val Person = Value
  val Product = Value
  val Quantity = Value
  val Time = Value
  val WorkOfArt = Value
  val NotEntity = Value
  val OutsideSentence = Value // useful for chains


}