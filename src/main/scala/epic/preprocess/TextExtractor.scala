package epic
package preprocess

import java.io.{StringReader, InputStream}
import java.net.URL

import de.l3s.boilerpipe.document.TextDocument
import de.l3s.boilerpipe.extractors.ArticleExtractor
import de.l3s.boilerpipe.sax.BoilerpipeSAXInput
import epic.slab.{Slab, StringSlab}
import epic.trees.Span
import epic.util.{Optional, NotProvided}
import org.apache.tika.Tika
import org.apache.tika.io.TikaInputStream
import org.apache.tika.metadata.{TikaMetadataKeys, Metadata}
import org.apache.tika.parser.ParseContext
import org.apache.xerces.parsers.AbstractSAXParser
import org.cyberneko.html.HTMLConfiguration
import org.xml.sax.{helpers, Attributes, Locator, InputSource}
import org.xml.sax.helpers.DefaultHandler
import scala.collection.JavaConverters._
import scala.collection.mutable.ArrayBuffer

import org.apache.commons.lang3.StringEscapeUtils;

import scala.io.Source
import scala.xml.Elem

/**
 * Just a simple thing for me to learn Tika
 *
 * @author dlwh
 **/
object TextExtractor {

  def extractText(url: URL): String = {

    new Tika().parseToString(url)

  }


  case class Content(labels: Set[String] = Set.empty)

  /**
   * Uses boilerpipe to extract the content from an XHTML document
   * @return
   */
  def loadSlab(url: URL):StringSlab[Content] = {
    val originalxhtml = extractXHTML(url)
    val doc = new BoilerpipeSAXInput(new InputSource(new StringReader(originalxhtml.toString))).getTextDocument

    ArticleExtractor.getInstance().process(doc)

    val textElements = doc.getTextBlocks.asScala.collect { case block if block.isContent =>
      block.getContainedTextElements
    }.foldLeft(new java.util.BitSet()) {(a,b) => a |= b; a}


    val sb = new StringBuilder()
    val contents = new ArrayBuffer[(Span, Content)]()

    val handler = new DefaultHandler {
      var index = 0

      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
        sb.append('<')
        sb.append(qName)
        for(i <- 0 until attributes.getLength) {
          val attr: String = attributes.getQName(i)
          val value: String = attributes.getValue(i)
          sb.append(' ')
          sb.append(attr)
          sb.append("=\"")
          sb.append(StringEscapeUtils.escapeXml11(value))
          sb.append("\"")
        }


        sb.append('>')
      }


      override def endElement(uri: String, localName: String, qName: String): Unit = {
        sb.append("</")
        sb.append(qName)
        sb.append('>')
      }

      override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
        sb.appendAll(ch, start, length)
      }

      override def characters(ch: Array[Char], start: Int, length: Int): Unit = {
        index += 1
        val s = sb.length
        val value = new String(ch, start, length)
        sb.append(StringEscapeUtils.escapeXml11(value))
        if(textElements(index)) {
          contents.append(Span(s, sb.length) -> Content(getLabelsForTextElement(doc, index)))
        }
      }
    }

    val parser = new AbstractSAXParser(new HTMLConfiguration) {setContentHandler(handler)}

    parser.parse(new InputSource(new StringReader(originalxhtml.toString())))

    val content = sb.toString

    Slab(content)++contents

  }


  private def getLabelsForTextElement(doc: TextDocument, index: Int): Set[String] = {
    doc.getTextBlocks.asScala.find(_.getContainedTextElements.get(index)).map(b => Option(b.getLabels).map(_.asScala).iterator.flatten.toSet).getOrElse(Set.empty)
  }

  def extractXHTML(url: URL) = {
    val metadata = new Metadata()
    val stream: InputStream = TikaInputStream.get(url, metadata)

    val loader = new Loader()
    new Tika().getParser.parse(stream, loader, metadata, new ParseContext)

    loader.value

  }

  def foo(url: URL)=  {
    import de.l3s.boilerpipe._
    ArticleExtractor.INSTANCE.getText(url)
  }

  import scala.xml.factory.XMLLoader
  import scala.xml._
  import org.xml.sax._
  import org.xml.sax.helpers.DefaultHandler

  class Loader extends DefaultHandler with XMLLoader[Elem] {
    val newAdapter = adapter
    def value = newAdapter.rootElem.asInstanceOf[Elem]

    override def characters( ch: Array[Char], start: Int, length: Int) {
      newAdapter.characters(ch, start, length)
    }
    override def endDocument() {
      newAdapter.endDocument()
      // the pdf parser sends two end documents...
      if(newAdapter.scopeStack.nonEmpty)
        newAdapter.scopeStack.pop()
    }
    override def endElement(uri: String, localName: String, qName: String) {
      newAdapter.endElement(uri, localName, qName)
    }
    override def processingInstruction(target: String, data: String) {
      newAdapter.processingInstruction(target, data)
    }
    override def startDocument() {
      newAdapter.scopeStack push TopScope
      newAdapter.startDocument()
    }
    override def startElement(uri: String, localName: String, qName: String, atts: Attributes) {
      newAdapter.startElement(uri, localName, qName, atts)
    }

    override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
      characters(ch, start, length)
    }


  }


}
