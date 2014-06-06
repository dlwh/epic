package epic.preprocess

import breeze.util.Iterators
import java.io.StringReader
import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable
import epic.slab._
import epic.slab.Sentence
import epic.slab.Token

@SerialVersionUID(1L)
class TreebankTokenizer() extends Tokenizer with Serializable {

  override def apply[In <: Sentence](slab: StringSlab[In]): StringSlab[In with Token] = {
    slab.++[Token](slab.iterator[Sentence].flatMap { s =>
      val content = s.in(slab).content
      val impl = new TreebankTokenizerImpl(new StringReader(content))
      Iterators.fromProducer{
        try {
          Option(impl.getNextToken()).map { token =>
            token.copy(token.begin + s.begin, token.end + s.begin)
          }
        } catch {
          case e: Throwable => throw new RuntimeException("Could not tokenize " + s, e)
        }
      }
    })
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