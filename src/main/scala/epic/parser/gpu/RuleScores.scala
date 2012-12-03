package epic.parser.gpu

import epic.parser.{SimpleRefinedGrammar, RefinedGrammar}
import epic.trees.BinaryRule

case class RuleScores(binaries: Array[Double], unaries: Array[Double]) {

}

object RuleScores {
  def fromRefinedGrammar[L, W](grammar: SimpleRefinedGrammar[L, _, W]) = {
    val (bin, un) = grammar.grammar.index.toIndexedSeq.zipWithIndex.partition(_._1.isInstanceOf[BinaryRule[_]])
    val binaries = new Array[Double](bin.length)
    val unaries = new Array[Double](un.length)
    for( (b, i) <- bin) {
      binaries(i) = grammar.ruleScore(i, 0)
    }
    for( (b, i) <- un) {
      unaries(i - bin.length) = grammar.ruleScore(i, 0)
    }

    RuleScores(binaries, unaries)
  }
}