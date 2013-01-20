package epic.parser.gpu

import epic.trees._
import epic.parser.BaseGrammar
import collection.mutable.ArrayBuffer
import collection.immutable.BitSet
import breeze.linalg.DenseVector
import epic.parser.projections.GrammarRefinements

case class RuleStructure[C, L](refinements: GrammarRefinements[C, L], grammar: BaseGrammar[L]) {


  import grammar._
  def numSyms = grammar.labelIndex.size
  def numCoarseSyms = refinements.labels.coarseIndex.size
  def root = grammar.rootIndex
  val (binaryRules, unaryRules) = (0 until index.size).partition(isBinary(_))

  def numBinaries = binaryRules.length
  def numUnaries = unaryRules.length
  def numRules: Int = numBinaries + numUnaries

  val unaryRulesWithIndices = unaryRules.map { r => indexedRule(r).asInstanceOf[UnaryRule[Int]] -> (r-binaryRules.length)}
  val binaryRulesWithIndices = binaryRules.map { r => indexedRule(r).asInstanceOf[BinaryRule[Int]] -> r }

  val terminalSymbols = {
    val onLHS = Set.empty ++ binaryRulesWithIndices.map(_._1.parent)
    val onURHS = Set.empty ++ unaryRulesWithIndices.map(_._1.child)
    BitSet.empty ++ (0 until labelIndex.size).filterNot(onLHS).filter(onURHS)
  }
  val numNonTerms = numSyms - terminalSymbols.size

  val terminalMap = Array.tabulate(numSyms)(i => i)
  val nonterminalMap = Array.tabulate(numSyms)(i => i)

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
  lazy val partitionsParent: IndexedSeq[IndexedSeq[(BinaryRule[Int], Int)]] = GrammarPartitioner.partition(ntRules, targetLabel = GrammarPartitioner.Parent).toIndexedSeq
  lazy val partitionsLeft: IndexedSeq[IndexedSeq[(BinaryRule[Int], Int)]] = partitionsParent
  lazy val partitionsRight: IndexedSeq[IndexedSeq[(BinaryRule[Int], Int)]] = partitionsParent
  lazy val partitionsSmall: IndexedSeq[IndexedSeq[(BinaryRule[Int], Int)]] = GrammarPartitioner.partition(ntRules, maxPartitionLabelSize = 30, targetLabel = GrammarPartitioner.Parent).toIndexedSeq

  val (termUnaries, ntermUnaries, termIdentUnaries) = {
    val termUnaries, ntermUnaries, termIdentUnaries = ArrayBuffer[(UnaryRule[Int], Int)]()
    for(r <- unaryRulesWithIndices) {
      val childTerm =  terminalSymbols(r._1.child)
      val pTerm =  terminalSymbols(r._1.parent)
      if(pTerm) assert(r._1.child == r._1.parent, grammar.labelIndex.get(r._1.parent) + " " + grammar.labelIndex.get(r._1.child))
      if(childTerm) {
        if(r._1.child != r._1.parent)
          termUnaries += r
        else termIdentUnaries += r
      } else {
        ntermUnaries += r
      }
    }


    (termUnaries: IndexedSeq[(UnaryRule[Int], Int)], ntermUnaries: IndexedSeq[(UnaryRule[Int], Int)], termIdentUnaries.toIndexedSeq)
  }


  /**
   * 1 if the corresponding unary rule is not a terminal->terminal unary rule.
   */
  val nonIdentityMask = {
    val arr = Array.fill(numUnaries)(1.0)
    for(t <- termIdentUnaries) {
      arr(t._2) = 0.0
    }
    arr
  }

  // pruning stuff
  val pruningMaskFieldSize = numCoarseSyms/64 + {if(numCoarseSyms % 64 != 0) 1 else 0}

  def pruningMaskForSyms(syms: Iterable[Int]) = {
    val coarsened = syms.map(refinements.labels.project(_)).toSet
    val mask = Array.fill(pruningMaskFieldSize)("")
    for( (field, coarses) <- coarsened.groupBy(_ / 64)) {
      mask(field) = coarses.map(c => "(1L << %d)".format(c-64*field)).mkString("(", "|", ")")
    }
    mask
  }

  def pruningCheckForSyms(syms: Iterable[Int], id: Int) = {
    val mask = pruningMaskForSyms(syms)
    val checks = mask.zipWithIndex.filter(_._1.nonEmpty).map{ case (mask, field) => "(((mask)).allowed[" + field + "] &(" + mask +"))"}
    checks.mkString("#define IS_ANY_IN_BLOCK_" +id +"(mask)  (", "||", ")")

  }
}

