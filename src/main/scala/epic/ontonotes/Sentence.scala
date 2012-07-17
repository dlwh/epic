package epic.ontonotes

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
import breeze.data.Example
import epic.trees.{Span, Tree}
import java.io.{PushbackReader, Reader, InputStream}
import collection.mutable.ArrayBuffer


/**
 * represents an annotation ontonotes sentence. Doesn't include raw sentence, for now.
 *
 * @author dlwh
 */
case class Sentence(id: String,
                   words: Seq[String],
                   tree: Tree[OntoLabel]) extends Example[Tree[OntoLabel],Seq[String]] {
  def features = words
  def label = tree

  def stripTraces: Sentence = {
    // walk the tree twice. first time patching everything except srl, which
    // has to be fixed later because of the indexed strategy
    val idMap = collection.mutable.Map[(Int,Int),(Int,Int)]()
    def isTrace(t: Tree[OntoLabel]) = t.label.tag.startsWith("-NONE-")
    def rec1(t: Tree[OntoLabel], offset:Int = 0):Option[(Tree[OntoLabel],Seq[String],Int)] = {
      var moff = offset
      val newWords = new ArrayBuffer[String]()
      val oldHeight = t.leftHeight
      if(isTrace(t)) {
        None
      } else {
        if(t.isLeaf) {
          newWords += words(t.span.start)
          moff += 1
        }

        var newHeight = -1
        val newChildren = for( c <- t.children) yield {
          rec1(c,moff) match {
            case None => None
            case Some((t,w,h)) =>
              if(newHeight < 0) newHeight = h + 1
              newWords ++= w
              moff += w.length
              Some(t)
          }
        }


        val finalChildren = newChildren.flatMap(_.iterator)
        if(t.children.length != 0 && finalChildren.length == 0) {
          None
        } else {
          if(newHeight < 0) newHeight = 0
          val newTree = Tree(t.label,finalChildren, Span(offset,moff))
          idMap(t.span.start -> oldHeight) = (offset -> newHeight)
          Some((newTree,newWords,newHeight))
        }
      }
    }

    val Some((newTree,w2,_)) = rec1(tree)
    // second pass: update Semantic role stuff
    val t3 = newTree.map { l =>
      l.frame match {
        case None => l
        case Some(frame) =>
          val newArgs = frame.args.map{a =>
            a.copy(fillers = a.fillers.collect(idMap))
          }
          val newFrame = frame.copy(args = newArgs.filter(_.fillers.nonEmpty))
          l.copy(frame = Some(newFrame))
      }
    }

    Sentence(id,w2,t3)

  }
}


object Sentence {
  /**
   * Reads an Ontonotes dlwh-XML file:
   *
   * example
   * {{{
   *   <sentence coref_section="0" id="4@bc/cnn/00/cnn_0000@all@cnn@bc@en@on">
    <tree>(TOP (FRAG (NP-VOC (NNP Paula)) (NP-VOC (NNP Paula)) (. /.)))</tree>
    <T tag="TOP">
      <T tag="FRAG">
        <T tag="NP-VOC">
          <coref chain="IDENT" chainid="367" link="IDENT" />
          <name string="Paula" tokens="0 1" type="PERSON" words="0 1" />
          <T tag="NNP" word="Paula" />
        </T>
        <T tag="NP-VOC">
          <coref chain="IDENT" chainid="367" link="IDENT" />
          <name string="Paula" tokens="1 2" type="PERSON" words="1 2" />
          <T tag="NNP" word="Paula" />
        </T>
        <T tag="." word="/." />
      </T>
    </T>
  </sentence>
   }}}
   */
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
      for(w <- word) {
        moff += 1
        words += w
      }
      val children = for( (c: xml.Node) <- (tree \ "T").toIndexedSeq) yield {
        val r = rec(c,moff)
        moff += r._2.length
        words ++= r._2
        r._1
      }

      Tree(OntoLabel(tag,sense,entity.getOrElse(NERType.NotEntity), coref, prop), children, Span(offset,moff)) -> words
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
    Sense(node \ "@lemma" text, (node \ "@sense").text, (node \ "@pos").text)
  }

  private def processName(node: xml.Node) = {
    NERType.fromString(node \ "@type" text)
  }
}


/**
 * A label in an Ontonotes annotated tree.
 * Includes tag, word sense, NER tag, coref mention and propbank frame
 */
case class OntoLabel(tag: String,
                     sense: Option[Sense] = None,
                     entity: NERType.Value = NERType.NotEntity,
                     mention: Option[Mention] = None,
                     frame: Option[Frame] = None)

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
}
