package epic
package preprocess

import java.io.InputStream
import java.net.URL

import de.l3s.boilerpipe.extractors.ArticleExtractor
import epic.slab.Slab
import epic.trees.Span
import org.apache.tika.Tika
import org.apache.tika.io.TikaInputStream
import org.apache.tika.metadata.Metadata
import org.apache.tika.parser.html.BoilerpipeContentHandler
import org.apache.tika.parser.{ParseContext, Parser}
import org.apache.tika.sax.ToTextContentHandler
import org.xml.sax._

/**
 * Just a simple thing for me to learn Tika
 *
 * @author dlwh
 **/
object TextExtractor {
  if (!hasTika)
    throw new RuntimeException("Apache Tika is an optional dependency and is not on the classpath")

  def extractText(url: URL, extractMainContentOnly: Boolean = true) = loadSlab(url, extractMainContentOnly).content

  def loadSlab(url: URL, extractMainContentOnly: Boolean = true) = {
    val newLineTags = Set(
      "address",
      "blockquote",
      "div",
      "dl",
      "fieldset",
      "form",
      "h1",
      "h2",
      "h3",
      "h4",
      "h5",
      "h6",
      "hr",
      "noscript",
      "ol",
      "p",
      "pre",
      "table",
      "ul",
      "dd",
      "dt",
      "li",
      "tbody",
      "td",
      "tfoot",
      "th",
      "thead",
      "tr",
      // html5
      "article",
      "aside",
      "audio",
      "canvas",
      "figcaption",
      "figure",
      "header",
      "hgroup",
      "output",
      "section",
      "video"
    )

    val textHandler = new ToTextContentHandler() {
      override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = characters(ch, start, length)

      override def startElement(uri: String, localName: String, qName: String, attributes: Attributes): Unit = {
        super.startElement(uri, localName, qName, attributes)
        if (newLineTags(qName.toLowerCase)) {
          ignorableWhitespace(Array('\n'), 0, 1)
        }
      }

      override def endElement(uri: String, localName: String, qName: String): Unit = {
        super.endElement(uri, localName, qName)
        if (newLineTags(qName.toLowerCase)) {
          ignorableWhitespace(Array('\n'), 0, 1)
        }

      }
    }
    val handler = if (extractMainContentOnly) {
      new BoilerpipeContentHandler(textHandler, ArticleExtractor.getInstance()) {
        // stupid handler doesn't pass whitespace
        /*
        override def ignorableWhitespace(ch: Array[Char], start: Int, length: Int): Unit = {
          try {
            characters(ch, start, length)
          } catch {
            case ex: ArrayIndexOutOfBoundsException =>
          }
        }
        */
        setIncludeMarkup(true)
      }
    } else {
      textHandler
    }
    val tk = new Tika()
    val parser = tk.getParser

    val metadata: Metadata = new Metadata
    val stream: InputStream = TikaInputStream.get(url, metadata)

    try {
      val context: ParseContext = new ParseContext
      context.set(classOf[Parser], parser)
      parser.parse(stream, handler, metadata, context)
    } finally {
      stream.close()
    }

    val content = textHandler.toString.trim

    Slab(content).addLayer(Span(0, content.length) -> epic.slab.Source(url))
  }

  /* TODO: I'd like to be able to keep the XHTML formatting in the text, but right now that looks like it's going to
  cause problems with the way slabs work. (Namely, we'll get discontiguous blocks of text, even in the middle of words.
   * Uses boilerpipe to extract the content from an XHTML document
   * @return
  case class Content(labels: Set[String] = Set.empty)
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
        if (textElements(index)) {
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

   */
  def extractXHTML(url: URL) = {
    val metadata = new Metadata()
    val stream: InputStream = TikaInputStream.get(url, metadata)
    val loader = new Loader()
    new Tika().getParser.parse(stream, loader, metadata, new ParseContext)
    loader.value
  }

  def foo(url: URL)=  {
    ArticleExtractor.INSTANCE.getText(url)
  }

  import org.xml.sax._
  import org.xml.sax.helpers.DefaultHandler

import scala.xml._
  import scala.xml.factory.XMLLoader

  class Loader extends DefaultHandler with XMLLoader[Elem] {
    val newAdapter = adapter
    def value = newAdapter.rootElem.asInstanceOf[Elem]

    override def characters( ch: Array[Char], start: Int, length: Int) {
      newAdapter.characters(ch, start, length)
    }
    override def endDocument() {
      newAdapter.endDocument()
      // the pdf parser sends two end documents...
      if (newAdapter.scopeStack.nonEmpty)
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

  def hasTika = {
    try {
      Class.forName(classOf[Tika].getName)
      true
    }
    catch {
      case _: Any => false
    }
  }

}
