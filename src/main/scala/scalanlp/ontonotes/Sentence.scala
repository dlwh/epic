package scalanlp.ontonotes

import scalanlp.data.Example
import scalanlp.trees.{Span, Tree}
import java.io.{PushbackReader, Reader, InputStream}
import collection.mutable.ArrayBuffer

case class Document(id: String, sentences: Seq[Sentence])

object Document {
  def fromXML(node: xml.Node) = {
    Document(node \ "@id" text, node \ "sentence" map {Sentence.fromXML _})
  }
}

/**
 *
 * @author dlwh
 */

case class Sentence(id: String,
                   words: Seq[String],
                   tree: Tree[OntoLabel]) extends Example[Tree[OntoLabel],Seq[String]] {
  def features = words
  def label = tree
}


object Sentence {
  def fromXML(node: xml.Node) = {
    val tree = node \ "T"

    def rec(tree: xml.Node, offset: Int = 0):(Tree[OntoLabel],IndexedSeq[String]) = {
      val tag = (tree \ "@tag").text
      val coref = (tree \ "coref").headOption.map(processCoref _)
      val prop = (tree \ "proposition").headOption.map(processProp _)
      val sense = (tree \ "sense").headOption.map(processSense _)
      val entity = (tree \ "name").headOption.map(processName _)
      val word = (tree \ "@word").headOption.map(_.text)

      var moff = offset
      val words = ArrayBuffer[String]()
      for(w <- word) words += w
      val children = for( (c: xml.Node) <- (tree \ "T").toIndexedSeq) yield {
        val r = rec(c,moff)
        moff += r._2.length
        words ++= r._2
        r._1
      }

      Tree(OntoLabel(tag,sense,entity.getOrElse(NERType.NotEntity), coref, prop), children)(Span(offset,moff)) -> words
    }

    val (t2,words) = rec(tree.head)
    Sentence(node \ "@id" text, words, t2)
  }

  private def processCoref(node: xml.Node):Mention = {
    Mention( (node \ "@chainid").text.toInt, MentionType.fromString(node \ "@link" text))
  }

  private def processProp(node: xml.Node) = {
    val lemma = (node \ "@lemma").text
    val sense = (node \ "@sense").text.toInt

    val args = for(argN <- node \ "analogue") yield {
      val arg = (argN \ "@type").text
      val fillers = for(f <- argN \\ "link") yield {
        val Array(index,height) = f.text.split(":").map(_.toInt)
        index -> height
      }
      Argument(arg,fillers)
    }

    Frame(lemma,sense,args)
  }

  private def processSense(node: xml.Node):Sense = {
    Sense(node \ "@lemma" text, (node \ "@sense").text.toInt, (node \ "@pos").text)
  }

  private def processName(node: xml.Node) = {
    NERType.fromString(node \ "@type" text)
  }
}


case class OntoLabel(tag: String,
                     sense: Option[Sense] = None,
                     entity: NERType = NERType.NotEntity,
                     mention: Option[Mention] = None,
                     frame: Option[Frame] = None)

case class Mention(id: Int, mentionType: MentionType = MentionType.Ident)

case class Frame(lemma: String, sense: Int, args: Seq[Argument])
case class Argument(arg: String, fillers: Seq[(Int,Int)]) // leaf:height pairs
case class Sense(lemma: String, sense: Int, pos: String)

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

sealed trait NERType
object NERType {
  def fromString(str: String): NERType = str.toLowerCase match {
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
  }

  case object Cardinal extends NERType
  case object Date extends NERType
  case object Event extends NERType
  case object Fac extends NERType
  case object GPE extends NERType
  case object Language extends NERType
  case object Law extends NERType
  case object Loc extends NERType
  case object Money extends NERType
  case object NORP extends NERType
  case object Ordinal extends NERType
  case object Org extends NERType
  case object Percent extends NERType
  case object Person extends NERType
  case object Product extends NERType
  case object Quantity extends NERType
  case object Time extends NERType
  case object WorkOfArt extends NERType
  case object NotEntity extends NERType
}
