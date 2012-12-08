package epic.parser.gpu

import epic.parser.{BaseGrammar, SimpleRefinedGrammar, RefinedGrammar}
import epic.trees.{AnnotatedLabel, BinaryRule}

case class RuleScores(binaries: Array[Double], unaries: Array[Double])

object RuleScores {
  def zeros[L](grammar: BaseGrammar[L]) = {
    val (bin, un) = grammar.index.toIndexedSeq.zipWithIndex.partition(_._1.isInstanceOf[BinaryRule[_]])
    val binaries = new Array[Double](bin.length)
    val unaries = new Array[Double](un.length)

    RuleScores(binaries, unaries)
  }

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