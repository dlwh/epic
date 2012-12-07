package epic.parser.gpu

import epic.trees._
import epic.parser.BaseGrammar
import collection.mutable.ArrayBuffer

case class RuleStructure[L](grammar: BaseGrammar[L]) {
  import grammar._
  def numSyms = grammar.labelIndex.size
  def root = grammar.rootIndex
  val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))

  def numBinaries = binaryRules.length
  def numUnaries = unaryRules.length

  val unaryRulesWithIndices = unaryRules.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> (r-binaryRules.length)}
  val binaryRulesWithIndices = binaryRules.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> r }

  val terminalSymbols = {
    val onLHS = Set.empty ++ binaryRulesWithIndices.map(_._1.parent)
    (0 until labelIndex.size).filterNot(onLHS).toSet - grammar.rootIndex
  }

  val (ntRules, leftTermRules, rightTermRules, bothTermRules) = {
    val ntRules, leftTermRules, rightTermRules, bothTermRules = ArrayBuffer[(BinaryRule[Int], Int)]()
    for(r <- binaryRulesWithIndices) {
      val leftTerm =  terminalSymbols(r._1.left)
      val rightTerm =  terminalSymbols(r._1.right)
      if(leftTerm && rightTerm) {
        bothTermRules += r
      } else if (rightTerm) {
        rightTermRules += r
      } else if(leftTerm) {
        leftTermRules += r
      } else {
        ntRules += r
      }
    }

    (ntRules: IndexedSeq[(BinaryRule[Int], Int)], leftTermRules: IndexedSeq[(BinaryRule[Int], Int)], rightTermRules: IndexedSeq[(BinaryRule[Int], Int)], bothTermRules: IndexedSeq[(BinaryRule[Int], Int)])
  }

  val (termUnaries, ntermUnaries) = {
    val termUnaries, ntermUnaries = ArrayBuffer[(UnaryRule[Int], Int)]()
    for(r <- unaryRulesWithIndices) {
      val leftTerm =  terminalSymbols(r._1.child)
      if(leftTerm) {
        if(r._1.child != r._1.parent)
        termUnaries += r
      } else {
        ntermUnaries += r
      }
    }

    (termUnaries: IndexedSeq[(UnaryRule[Int], Int)]) -> (ntermUnaries: IndexedSeq[(UnaryRule[Int], Int)])
  }
}

