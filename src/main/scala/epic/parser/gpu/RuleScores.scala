package epic.parser.gpu

import epic.parser.{SimpleRefinedGrammar, RefinedGrammar}
import epic.trees.BinaryRule

case class RuleScores(binaries: Array[Array[Double]], unaries: Array[Array[Double]])

object RuleScores {
  def fromRefinedGrammar[L, W](grammar: SimpleRefinedGrammar[L, _, W], numBits: Int = 0) = {
    val numStates = 1 << numBits
    val (bin, un) = grammar.grammar.index.toIndexedSeq.zipWithIndex.partition(_._1.isInstanceOf[BinaryRule[_]])
    val binaries = Array.ofDim[Double](bin.length, numStates * numStates * numStates)
    val unaries = Array.ofDim[Double](un.length, numStates * numStates)
    for( (b, i) <- bin; ref <- 0 until binaries(i).length) {
      binaries(i)(ref) = grammar.ruleScore(i, ref)
    }
    for( (b, i) <- un; ref <- 0 until unaries(i-bin.length).length) {
      unaries(i - bin.length)(ref) = grammar.ruleScore(i, 0)
    }

    RuleScores(binaries, unaries)
  }
}