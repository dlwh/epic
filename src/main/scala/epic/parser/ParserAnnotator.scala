package epic.parser

import epic.slab._
import epic.trees.Tree
import epic.slab.annotators.Annotator

/**
 * A ParserAnnotator is a slab-ified [[epic.parser.Parser]]. The analogy is that a [[epic.sequences.Tagger]] is
 * to [[epic.sequences.CRF]] as this class is to [[epic.parser.Parser]].
 *
 * @author dlwh
 **/

class ParserAnnotator[L](val parser: Parser[L, String]) extends Annotator[Tree[L]](ParserAnnotator.annotate(parser))

object ParserAnnotator {
  def annotate[L](parser: Parser[L, String])(content: String, tokens: Vector[Token]) = {
    Vector(parser(tokens.map(t => content.substring(t.span.begin, t.span.end))))
  }
  def apply[L](parser: Parser[L, String]): ParserAnnotator[L] = new ParserAnnotator[L](parser)
}
