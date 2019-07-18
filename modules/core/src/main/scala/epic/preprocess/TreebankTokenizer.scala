package epic.preprocess

import java.io.{File, FilenameFilter, StringReader}

import breeze.util.Iterators
import epic.corpora.MascSlab
import epic.slab._
import epic.trees.Span

import scala.collection.mutable.ArrayBuffer

@SerialVersionUID(1L)
class TreebankTokenizer() extends Tokenizer with Serializable {

  override def apply[In <: Sentence](slab: StringSlab[In]): StringSlab[In with Token] = {
    slab.addLayer[Token](slab.iterator[Sentence].flatMap { s =>
      val content = slab.spanned(s._1)
      val impl = new TreebankTokenizerImpl(new StringReader(content))
      Iterators.fromProducer{
        try {
          Option(impl.getNextToken()).map { case (region, token) =>
            val res = Span(region.begin + s._1.begin, region.end + s._1.begin) -> token
            res
          }
        } catch {
          case e: Throwable => throw new RuntimeException("Could not tokenize " + s, e)
        }
      }
    })
  }

}

object TreebankTokenizer extends TreebankTokenizer {
  def treebankTokenToToken(s: String): String = reverseTreebankMappings.getOrElse(s, s)

  private val treebankMappings = Map("(" -> "-LRB-", ")" -> "-RRB-", "{" -> "-LCB-", "}" -> "-RCB-", "[" -> "-LSB-", "]" -> "-RSB-")
  private val reverseTreebankMappings = treebankMappings.map(_.swap)

  /** Replaces symbols like ( with their penn treebank equivalent */
  def tokensToTreebankTokens(toks: Seq[String]): IndexedSeq[String] = {
    // have to deal with quotes, so we can't just use map.
    val output =  new ArrayBuffer[String]()

    var inOpenQuote = false

    for(t <- toks) t match {
      case "“" => inOpenQuote = true; output += "``"
      case "‘" => inOpenQuote = true; output += "`"
      case "’" => inOpenQuote = false; output += "'"
      case "”" => inOpenQuote = false; output += "''"
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
      val slab: StringSlab[Source] = MascSlab(f.toURI.toURL)
      val slabWithSentences: Slab[String, Span, Source with Sentence] = MascSlab.s[Source](slab)
      val slabWithTokens = MascSlab.seg(slabWithSentences)
      slabWithTokens.iterator[Sentence].map{sent =>
        val gold = slabWithTokens.covered[Segment](sent._1).map { case (span, tok) => slab.spanned(span)}
        val guess = TreebankTokenizer(slab.spanned(sent._1))
        (gold, guess, slab.spanned(sent._1))
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

