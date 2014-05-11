package epic.parser

import epic.trees.Tree
import epic.util.ProcessTextMain

/**
 * Simple class that reads in a bunch of files and parses them. Output is dumped to standard out.
 * @author dlwh
 */
object ParseText extends ProcessTextMain[Parser[Any, String], Tree[Any]] {


  override def render(model: Parser[Any, String], ann: Tree[Any], tokens: IndexedSeq[String]): String = {
    ann.render(tokens, newline = false)
  }

  override def annotate(model: Parser[Any, String], text: IndexedSeq[String]): Tree[Any] = model(text)

}
