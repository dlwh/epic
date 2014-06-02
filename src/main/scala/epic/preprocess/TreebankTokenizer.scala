package epic.preprocess

import chalk.text.tokenize.Tokenizer
import breeze.util.Iterators
import java.io.StringReader
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable

@SerialVersionUID(1L)
class TreebankTokenizer() extends Tokenizer with Serializable {
  def apply(text: String):Iterable[String] = {
    new Iterable[String]() {
      def iterator: Iterator[String] = {
        val impl = new TreebankTokenizerImpl(new StringReader(text + "\n"))
        Iterators.fromProducer{
          try {
            Option(impl.getNextToken())
          } catch {
            case e: Throwable => throw new RuntimeException("Could not tokenize " + text, e)
          }
        }.takeWhile(_ != null)
      }
    }

  }


}

object TreebankTokenizer extends TreebankTokenizer {
  private val treebankMappings = Map("(" -> "-LRB-", ")" -> "-RRB-", "{" -> "-LCB-", "}" -> "-RCB-", "[" -> "-LSB-", "]" -> "-RSB-")
  private val reverseTreebankMappings = treebankMappings.map(_.swap)

  /** Replaces symbols like ( with their penn treebank equivalent */
  def tokensToTreebankTokens(toks: IndexedSeq[String]): IndexedSeq[String] = {
    // have to deal with quotes, so we can't just use map.
    val output =  new ArrayBuffer[String]()

    var yyquote = false

    for(t <- toks) t match {
      case "\"" if yyquote => yyquote = false; output += "''"
      case "\"" => yyquote = true; output += "``"
      case _ => output += treebankMappings.getOrElse(t, t)
    }
    
    output
  }
}