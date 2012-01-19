package scalanlp.ontonotes

import scalanlp.data.Example
import scalanlp.trees.{Span, Tree}
import java.io.{PushbackReader, Reader, InputStream}

/**
 *
 * @author dlwh
 */

case class Sentence(id: String,
                   rawSentence: String,
                   tokens: Seq[Token],
                   tree: Tree[String],
                   meta: Map[String,Seq[String]] = Map.empty) {
  def words = tokens.map(_.word)
}

case class Token(pos: Int, word: String,
                 sense: Option[String] = None,
                 entity: Option[Entity] = None,
                 mention: Option[Mention] = None,
                 frame: Option[Frame] = None)

case class Mention(id: Int, span: Span, mentionType: MentionType = MentionType.Ident)
case class Entity(entityType: NERType, span: Span)

case class Frame(sense: String, args: Seq[Argument])
case class Argument(arg: String, fillers: Seq[(Int,Int)]) // leaf:height pairs

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
