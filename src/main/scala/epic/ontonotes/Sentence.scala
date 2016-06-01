package epic.ontonotes

import epic.framework.Example
import epic.trees._
import epic.sequences.Segmentation
import collection.mutable.ArrayBuffer
import collection.mutable

/**
 * represents an annotation ontonotes sentence. Doesn't include raw sentence, for now.
 *
 * @author dlwh
 */
@SerialVersionUID(1L)
case class Sentence(docId: String, index: Int,
                   words: IndexedSeq[String],
                   annotations: OntoAnnotations) extends Example[OntoAnnotations,IndexedSeq[String]] {

  def id = docId +":"+index
  def features = words
  def label = annotations

  def length = words.length
  def tree = annotations.tree
  def ner = annotations.ner
  def speaker = annotations.speaker
  def srl = annotations.srl

  def treeInstance(processor: StandardTreeProcessor) = {
    TreeInstance(docId+"-treeInstance", processor(tree), words)
  }

  lazy val nerSegmentation: Segmentation[NerType.Value, String]  = {
    val sorted = ner.toIndexedSeq.sortBy((_: (DSpan, NerType.Value))._1.begin)
    val out = sorted.map { case (dspan, label) => label -> dspan.span}

    Segmentation(out, words, id)
  }
  def coref = annotations.coref

  def annotate(tree: Tree[AnnotatedLabel] = tree,
               ner: Map[DSpan,NerType.Value] = ner,
               coref: Map[DSpan,Mention] = coref,
               srl: IndexedSeq[Frame] = srl,
               speaker: Option[String] = speaker) = {
    copy(annotations=OntoAnnotations(tree, ner, coref, srl, speaker))
  }


  def dspans = for(begin <- 0 until length; end <- (begin+1) to length) yield DSpan(docId, index, begin, end)

}




@SerialVersionUID(1L)
case class OntoAnnotations(tree: Tree[AnnotatedLabel],
                           ner: Map[DSpan,NerType.Value],
                           coref: Map[DSpan,Mention],
                           srl: IndexedSeq[Frame],
                           speaker: Option[String])



/**
 * A coref mention
 */
@SerialVersionUID(1L)
case class Mention(id: Int, mentionType: MentionType = MentionType.Ident)

/**
 * A Propbank mention
 */
@SerialVersionUID(1L)
case class Frame(lemma: String, pos: Int, sense: Int, args: IndexedSeq[Argument]) {
  /**
   * Embedded arguments are very rare in SRL, (.2% on CoNLL 2011 training set),
   * and they're a pain... so...
   * @return
   */
  def stripEmbedded = {
    val newArgs = mutable.Stack[Argument]()
    val sorted = args.sortBy(a => (a.span.begin, -a.span.length))(Ordering.Tuple2)
    for(arg <- sorted) {
      if (newArgs.isEmpty || !newArgs.top.span.contains(arg.span)) { // don't overlap at all
        while (newArgs.nonEmpty && arg.span.contains(newArgs.top.span)) {
          newArgs.pop()
        }
        assert(newArgs.isEmpty || !arg.span.crosses(newArgs.top.span))
        newArgs push arg

        ()
      }
    }
    copy(args=newArgs.toIndexedSeq)
  }

  def asSegments(words: IndexedSeq[String], outside: String = "O"):IndexedSeq[(Option[String], Span)] = {
    val sorted = args.sortBy(_.span.begin)
    var out = new ArrayBuffer[(Option[String], Span)]()
    var last = 0
    for( arg <- sorted ) {
      assert(last <= arg.span.begin)
      while (arg.span.begin != last) {
        out += (Some(outside) -> Span(last,last+1))
        last += 1
      }
      out += (Some(arg.arg) -> Span(arg.span.begin, arg.span.end))
      last = arg.span.end
    }
    while (words.length != last) {
      out += (Some(outside) -> Span(last,last+1))
      last += 1
    }

    out
  }
}

case class Argument(arg: String, span: Span)

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
@SerialVersionUID(1L)
object NerType extends Enumeration {
  def fromString(str: String): NerType.Value = str.toLowerCase match {
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
//    case "notentity" | "none" => NotEntity
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
//  val NotEntity = Value
  val OutsideSentence = Value // useful for chains


}
