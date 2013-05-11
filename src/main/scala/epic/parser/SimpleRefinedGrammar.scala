package epic.parser
/*
 Copyright 2012 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/
import projections.GrammarRefinements
import epic.trees.{BinaryRule, UnaryRule}
import collection.mutable.ArrayBuffer
import java.io.{PrintWriter, Writer}
import epic.lexicon.Lexicon

/**
 *
 * @author dlwh
 */
@SerialVersionUID(2)
class SimpleRefinedGrammar[L, L2, W](val grammar: BaseGrammar[L],
                                     val lexicon: Lexicon[L, W],
                                     val refinements: GrammarRefinements[L, L2],
                                     val refinedGrammar: BaseGrammar[L2],
                                     ruleScoreArray: Array[Array[Double]],
                                     spanScoreArray: Array[Array[Double]],
                                     parentCompatibleRefinements: Array[Array[Array[Int]]],
                                     childCompatibleRefinements: Array[Array[Array[Int]]],
                                     tagScorer: TagScorer[L2, W]) extends RefinedGrammar[L, W] with Serializable {
  def ruleScore(r: Int, ruleRef: Int):Double = ruleScoreArray(r)(ruleRef)
  def spanScore(l: Int, ref: Int):Double = spanScoreArray(l)(ref)

  def ruleScore(refinedRule: Int): Double = {
    val ref = refinements.rules.localize(refinedRule)
    val parent = refinements.rules.project(refinedRule)
    ruleScoreArray(parent)(ref)
  }


  private val coarseRulesGivenParentRefinement = Array.tabulate(grammar.labelIndex.size) { p =>
    // refinement -> rules
    val result = Array.fill(refinements.labels.refinementsOf(p).size)(ArrayBuffer[Int]())
    for(r <- grammar.indexedBinaryRulesWithParent(p); ref <- 0 until result.length) {
      if(parentCompatibleRefinements(r)(ref).nonEmpty) {
        result(ref) += r
      }
    }

    result.map(_.toArray)
  }


  private val parentRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
  // rules -> parent refinements
    refinements.rules.refinementsOf(r).map(refinedGrammar.parent(_)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val leftChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.leftChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val rightChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else  refinements.rules.refinementsOf(r).map(r => refinedGrammar.rightChild(r)).toSet.toArray.map(refinements.labels.localize(_)).sorted
  }

  private val leftChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val leftChild = grammar.leftChild(r)
      val leftChildRefs = Array.fill(refinements.labels.refinementsOf(leftChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].left)
        leftChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      leftChildRefs.map(_.toArray)
    }
  }

  private val rightChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(grammar.index.size) { r =>
    if(grammar.index.get(r).isInstanceOf[UnaryRule[L]]) {
      null
    } else {

      val rightChild = grammar.rightChild(r)
      val rightChildRefs = Array.fill(refinements.labels.refinementsOf(rightChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- refinements.rules.refinementsOf(r)) {
        val refParent = refinements.labels.localize(refinements.rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[L2]].right)
        rightChildRefs(refParent) += refinements.rules.localize(ruleRef)
      }
      rightChildRefs.map(_.toArray)
    }
  }

  def anchor(w: IndexedSeq[W]) = new RefinedAnchoring[L, W] {
    val tagAnchoring = tagScorer.anchor(w)
    override def toString() = "SimpleAnchoring(...)"
    val grammar = SimpleRefinedGrammar.this.grammar
    val lexicon = SimpleRefinedGrammar.this.lexicon
    def words = w

    def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
      val baseScore = if(begin + 1 == end) {
        val fullId = refinements.labels.globalize(label, ref)
        tagAnchoring.scoreTag(begin, refinements.labels.fineIndex.get(fullId))
      } else {
        0.0
      }
      baseScore + spanScoreArray(label)(ref)
    }

    def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
      ruleScoreArray(rule)(ref)
    }

    def validLabelRefinements(begin: Int, end: Int, label: Int) = {
      refinements.labels.localRefinements(label)
    }

    def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
      parentCompatibleRefinements(rule)(parentRef)
    }

    def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, childRef: Int): Array[Int] = {
      leftChildCompatibleRefinements(rule)(childRef)
    }

    def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, childRef: Int): Array[Int] = {
      rightChildCompatibleRefinements(rule)(childRef)
    }

    def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
      childCompatibleRefinements(rule)(childRef)
    }

    def leftChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.leftChild(refinedRuleId))
    }

    def rightChildRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.rightChild(refinedRuleId))
    }

    def parentRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.parent(refinedRuleId))
    }

    def childRefinement(rule: Int, ruleRef: Int) = {
      val refinedRuleId = refinements.rules.globalize(rule, ruleRef)
      refinements.labels.localize(refinedGrammar.child(refinedRuleId))
    }

    // TODO: make this not so slow! Doesn't really matter, but still..
    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
      val a = grammar.parent(r)
      val b = grammar.child(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val rule = UnaryRule(refinements.labels.fineIndex.get(a2), refinements.labels.fineIndex.get(b2), grammar.chain(r))
      val refinedRuleIndex = refinements.rules.fineIndex(rule)
      if(refinedRuleIndex < 0) {
        -1
      } else {
        refinements.rules.localize(refinedRuleIndex)
      }
    }

    def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
      val a = grammar.parent(r)
      val b = grammar.leftChild(r)
      val c = grammar.rightChild(r)
      val a2 = refinements.labels.globalize(a, refA)
      val b2 = refinements.labels.globalize(b, refB)
      val c2 = refinements.labels.globalize(c, refC)
      refinements.rules.localize(refinements.rules.fineIndex(BinaryRule(refinements.labels.fineIndex.get(a2),
        refinements.labels.fineIndex.get(b2),
        refinements.labels.fineIndex.get(c2)
      ))  )
    }

    def numValidRefinements(label: Int) = refinements.labels.refinementsOf(label).length
    def numValidRuleRefinements(rule: Int) = refinements.rules.refinementsOf(rule).length

    def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = coarseRulesGivenParentRefinement(a)(refA)

    def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
      parentRefinementsGivenCoarseRule(rule)
    }

    def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
      leftChildRefinementsGivenCoarseRule(rule)
    }

    def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
      rightChildRefinementsGivenCoarseRule(rule)
    }
  }


  /**
   * Writes a text representation of the grammar to the output.
   * @param out
   * @param includeSpanScores
   */
  def prettyPrint(out: Writer = new PrintWriter(System.out), includeSpanScores: Boolean = false) = {
    val printout = new PrintWriter(out)
    import printout._

    for( (cr,index) <- refinements.rules.coarseIndex.zipWithIndex; ref <- refinements.rules.refinementsOf(index)) {
      refinements.rules.fineIndex.get(ref) match {
        case BinaryRule(a,b,c) =>
          println(s"$a -> $b $c ${ruleScoreArray(ref)}")
        case UnaryRule(a,b,chain) if chain.isEmpty =>
          println(s"$a -> $b ${ruleScoreArray(ref)}")
        case UnaryRule(a,b,chain) =>
          println(s"$a --${chain.mkString("[","-","]")}--> $b ${ruleScoreArray(ref)}")
      }

    }
  }
}
