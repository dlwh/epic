package epic.parser

import epic.trees.{AnnotatedLabel, Tree}
import epic.util.ProcessTextMain
import epic.models.ParserSelector

/**
 * Simple class that reads in a bunch of files and parses them. Output is dumped to standard out.
 * @author dlwh
 */
object ParseText extends ProcessTextMain[Parser[AnnotatedLabel, String], Tree[AnnotatedLabel]] {

  override def render(model: Parser[AnnotatedLabel, String], ann: Tree[AnnotatedLabel], tokens: IndexedSeq[String]): String = {
    ann.render(tokens, newline = false)
  }

  override def annotate(model: Parser[AnnotatedLabel, String], text: IndexedSeq[String]): Tree[AnnotatedLabel] = model(text)

  override def classPathLoad(language: String): Parser[AnnotatedLabel, String] = {
    ParserSelector.loadParser(language).get
  }
}
