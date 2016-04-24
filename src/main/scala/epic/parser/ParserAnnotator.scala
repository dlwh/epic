package epic.parser

import epic.slab.{StringSlab, Sentence, Token, StringAnalysisFunction}
import epic.trees.Tree

/**
 * A ParserAnnotator is a slab-ified [[epic.parser.Parser]]. The analogy is that a [[epic.sequences.Tagger]] is
 * to [[epic.sequences.CRF]] as this class is to [[epic.parser.Parser]].
 *
 * @author dlwh
 **/
class ParserAnnotator[L](parser: Parser[L, String]) extends StringAnalysisFunction[Token with Sentence, Tree[L]] {

  def apply[In <: Token with Sentence](slab: StringSlab[In]):StringSlab[In with epic.trees.Tree[L]] = {
    val annotatedSentences = for((span, sent) <- slab.iterator[Sentence].toIndexedSeq.par) yield {
      val tokens = slab.covered[Token](span)
      val tree = parser(tokens.map(_._2.token))
      span -> tree
    }
    slab.addLayer[Tree[L]](annotatedSentences.seq)
  }

}
