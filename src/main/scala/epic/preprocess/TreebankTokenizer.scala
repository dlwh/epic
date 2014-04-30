package epic.preprocess

import chalk.text.tokenize.Tokenizer
import breeze.util.Iterators
import java.io.StringReader

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

object TreebankTokenizer extends TreebankTokenizer