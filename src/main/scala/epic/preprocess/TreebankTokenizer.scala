package epic.preprocess

import java.io.{File, FilenameFilter, StringReader}

import breeze.util.Iterators
import epic.corpora.MascSlab
import epic.slab._
import epic.slab.annotators.Tokenizer
import epic.corpora.MascSlab
import epic.trees.Span
import scalaz.std.list._

import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
class TreebankTokenizer[S <: Sentence]() extends Tokenizer[S, ContentToken] with Serializable {
  def apply(sentence: String): Vector[ContentToken] = {
    val impl = new TreebankTokenizerImpl(new StringReader(sentence))
    Iterators.fromProducer{
      try {
        Option(impl.getNextToken())
      } catch {
        case e: Throwable => throw new RuntimeException("Could not tokenize " + sentence, e)
      }
    }.toVector
  }
}

object TreebankTokenizer extends TreebankTokenizer[Sentence] {
  def treebankTokenToToken(s: String): String = reverseTreebankMappings.getOrElse(s, s)

  private val treebankMappings = Map("(" -> "-LRB-", ")" -> "-RRB-", "{" -> "-LCB-", "}" -> "-RCB-", "[" -> "-LSB-", "]" -> "-RSB-")
  private val reverseTreebankMappings = treebankMappings.map(_.swap)

  /** Replaces symbols like ( with their penn treebank equivalent */
  def tokensToTreebankTokens(toks: IndexedSeq[String]): IndexedSeq[String] = {
    // have to deal with quotes, so we can't just use map.
    val output =  new ArrayBuffer[String]()

    var inOpenQuote = false

    for(t <- toks) t match {
      case "“" => inOpenQuote = true; output += "``"
      case "‘" => inOpenQuote = true; output += "`"
      case "’" => inOpenQuote = true; output += "`"
      case "”" => inOpenQuote = true; output += "``"
      case "\"" if inOpenQuote => inOpenQuote = false; output += "''"
      case "\"" => inOpenQuote = true; output += "``"
      case _ => output += treebankMappings.getOrElse(t, t)
    }
    
    output
  }

  // Just to check how the tokenizer does.
  def main(args: Array[String]) = {
    val mascDir = new java.io.File(args(0))
    val comps = for(dir <- new File(new File(mascDir,"data"), "written").listFiles();
                    f <- dir.listFiles(new FilenameFilter {
                      override def accept(dir: File, name: String): Boolean = name.endsWith(".txt")
                    })) yield {
      val slab = MascSlab(f.toURI.toURL)
      val slabWithSentences = MascSlab.s(slab)
      val slabWithTokens = MascSlab.seg(slabWithSentences)
      slabWithTokens.select[Sentence].map{sent =>
        val gold = slabWithTokens.covered[Segment](sent.span).toIndexedSeq.map { token => slab.substring(token.span)}
        val guess = TreebankTokenizer(slab.substring(sent.span))

        (gold, guess, slab.substring(sent.span))
      }
    }

    for( (gold, guess, orig) <- comps.iterator.flatten if gold != guess) {
      val gg = gold.map(treebankMappings.withDefault(identity[String])).mkString(" ").replaceAll("”","\"").replaceAll("“", "\"")
      val gs = guess.mkString(" ").replaceAll("(``|'')","\"").replaceAll("`","'")
      if (gg != gs) {
        println(gg)
        println(gs)
        println(orig)
        println("=====================")
      }
    }
  }
}

