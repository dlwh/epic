package epic.parser
package projections

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
import breeze.util.Index
import epic.trees._
import scala.collection.mutable.ArrayBuffer

/**
 * Handles the mapping between a coarser grammar and a finer grammar. (For example, an xbar
 * grammar and an xbar grammar with latent variables.) It mostly just aggregates two
 * [[epic.projections.ProjectionIndexer]]s, one for labels and one for rules. It also provides
 * convenience methods for, e.g., getting all rule refinements that have a given parent refinment.
 *
 * Refined symbols and rules are given a "local" integer index that is specific to a coarse symbol.
 * For example, NP-4 might have "global index" 37 for all symbols, but will have NP-specific local index 4.
 * Analogously, NP-4 -> NP-3 PP-2 will have a NP -> NP PP specific local index.
 *
 * @author dlwh
 */
@SerialVersionUID(2L)
final case class GrammarRefinements[C, F](labels: ProjectionIndexer[C, F], rules: ProjectionIndexer[Rule[C], Rule[F]]) {
  def compose[F2](other: GrammarRefinements[F, F2]):GrammarRefinements[C, F2] = new GrammarRefinements(labels compose other.labels, rules compose other.rules)

  def ruleRefinementsCompatibleWithParentRef(r: Int, parentRef: Int):Array[Int] = {
    parentCompatibleRefinements(r)(parentRef)
  }

  def ruleRefinementsCompatibleWithLeftRef(r: Int, leftChildRef: Int):Array[Int] = {
    leftChildCompatibleRefinements(r)(leftChildRef)
  }

  def ruleRefinementsCompatibleWithRightRef(r: Int, rightChildRef: Int):Array[Int] = {
    rightChildCompatibleRefinements(r)(rightChildRef)
  }

  def ruleRefinementsCompatibleWithChildRef(r: Int, childRef: Int):Array[Int] = {
    childCompatibleRefinements(r)(childRef)
  }

  def coarseRulesGivenParentRef(parent: Int, parentRef: Int):Array[Int] = {
    this.coarseRulesGivenParentRefinement(parent)(parentRef)
  }
  
  def parentRefinementsCompatibleWithRule(r: Int):Array[Int] = {
    parentRefinementsGivenCoarseRule(r)
  }

  def leftChildRefinementsCompatibleWithRule(r: Int):Array[Int] = {
    leftChildRefinementsGivenCoarseRule(r)
  }

  def rightChildRefinementsCompatibleWithRule(r: Int):Array[Int] = {
    rightChildRefinementsGivenCoarseRule(r)
  }

  /** Gives the localized refinement of each parent */
  def parentRefinement(r: Int, ref: Int): Int = parentRefinements(r)(ref)

  private val parentRefinements: Array[Array[Int]] = Array.tabulate(rules.coarseIndex.size) { r =>
    val parent = labels.coarseIndex(rules.coarseIndex.get(r).parent)
    rules.refinementsOf(r).map { ref =>
      labels.localize(rules.fineIndex.get(ref).parent)._2
    }
  }

  // rule -> parentRef -> [ruleRef]
  private val parentCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(rules.coarseIndex.size) { r =>
    val parent = labels.coarseIndex(rules.coarseIndex.get(r).parent)
    val parentRefs = Array.fill(labels.refinementsOf(parent).length){ArrayBuffer[Int]()}
    for(ruleRef <- rules.refinementsOf(r)) {
      val refParent = labels.localize(rules.fineIndex.get(ruleRef).parent)._2
      parentRefs(refParent) += rules.localize(ruleRef)
    }
    parentRefs.map(_.toArray)
  }

  private val leftChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(rules.coarseIndex.size) { r =>
    if (rules.coarseIndex.get(r).isInstanceOf[UnaryRule[C]]) {
      null
    } else {
      val leftChild = labels.coarseIndex(rules.coarseIndex.get(r).asInstanceOf[BinaryRule[C]].left)
      val leftChildRefs = Array.fill(labels.refinementsOf(leftChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- rules.refinementsOf(r)) {
        val refParent = labels.localize(rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[F]].left)._2
        leftChildRefs(refParent) += rules.localize(ruleRef)
      }
      leftChildRefs.map(_.toArray)
    }
  }

  private val rightChildCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(rules.coarseIndex.size) { r =>
    if (rules.coarseIndex.get(r).isInstanceOf[UnaryRule[C]]) {
      null
    } else {
      val rightChild = labels.coarseIndex(rules.coarseIndex.get(r).asInstanceOf[BinaryRule[C]].right)
      val rightChildRefs = Array.fill(labels.refinementsOf(rightChild).length){ArrayBuffer[Int]()}
      for(ruleRef <- rules.refinementsOf(r)) {
        val refParent = labels.localize(rules.fineIndex.get(ruleRef).asInstanceOf[BinaryRule[F]].right)._2
        rightChildRefs(refParent) += rules.localize(ruleRef)
      }
      rightChildRefs.map(_.toArray)
    }
  }

  // rule -> parentRef -> [ruleRef]
  private val childCompatibleRefinements: Array[Array[Array[Int]]] = Array.tabulate(rules.coarseIndex.size) { r =>
    if (rules.coarseIndex.get(r).isInstanceOf[UnaryRule[C]]) {
      val child = labels.coarseIndex(rules.coarseIndex.get(r).asInstanceOf[UnaryRule[C]].child)
      val childRefs = Array.fill(labels.refinementsOf(child).length){ArrayBuffer[Int]()}
      for(ruleRef <- rules.refinementsOf(r)) {
        val refChild = labels.localize(rules.fineIndex.get(ruleRef).asInstanceOf[UnaryRule[F]].child)._2
        childRefs(refChild) += rules.localize(ruleRef)
      }
      childRefs.map(_.toArray)
    } else {
      null
    }
  }

  private val coarseRulesGivenParentRefinement = Array.tabulate(labels.coarseIndex.size) { p =>
    // refinement -> rules
    val result = Array.fill(labels.refinementsOf(p).length)(ArrayBuffer[Int]())
    for( (rule, r) <- rules.coarseIndex.pairs if labels.coarseIndex(rule.parent) == p && rule.isInstanceOf[BinaryRule[_]]; ref <- result.indices) {
      if (parentCompatibleRefinements(r)(ref).nonEmpty) {
        result(ref) += r
      }
    }
    result.map(_.toArray)
  }

  private val parentRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(rules.coarseIndex.size) { r =>
  // rules -> parent refinements
    def fineParent(r: Int): Int = labels.fineIndex(rules.fineIndex.get(r).parent)
    rules.refinementsOf(r).map(fineParent).toSet.toArray.map(labels.localize).sorted
  }

  private val leftChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(rules.coarseIndex.size) { r =>
    if (rules.coarseIndex.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else {
      def fineLeftChild(r: Int) = labels.fineIndex(rules.fineIndex.get(r).asInstanceOf[BinaryRule[F]].left)
      rules.refinementsOf(r).map(fineLeftChild).toSet.toArray.map(labels.localize).sorted
    }
  }

  private val rightChildRefinementsGivenCoarseRule:Array[Array[Int]] = Array.tabulate(rules.coarseIndex.size) { r =>
    if (rules.coarseIndex.get(r).isInstanceOf[UnaryRule[_]]) Array.empty
    else {
      def fineRightChild(r: Int) = labels.fineIndex(rules.fineIndex.get(r).asInstanceOf[BinaryRule[F]].right)
      rules.refinementsOf(r).map(fineRightChild).toSet.toArray.map(labels.localize).sorted
    }
  }

}

object GrammarRefinements {
  def identity[C](topology: RuleTopology[C]): GrammarRefinements[C, C] = {
    apply(topology, topology, Predef.identity[C] _)
  }

  def apply[C, F](coarse: RuleTopology[C], fine: RuleTopology[F], proj: F=>C, skipMissingCoarseRules: Boolean = false): GrammarRefinements[C, F] = {
    def projRule(r: Rule[F]) = r map proj
    val rules = ProjectionIndexer(coarse.index, fine.index, projRule, skipMissingCoarse = skipMissingCoarseRules)
    val labels = ProjectionIndexer(coarse.labelIndex, fine.labelIndex, proj)

    new GrammarRefinements(labels, rules)
  }

  def apply[C, F](coarse: RuleTopology[C], split: C=>Seq[F], proj: F=>C): GrammarRefinements[C, F] = {
    def splitRule(r: Rule[C]) = r match {
      case BinaryRule(a, b, c) => for(a_ <- split(a); b_ <- split(b); c_ <- split(c)) yield BinaryRule(a_, b_, c_)
      case UnaryRule(a, b, chain) => for(a_ <- split(a); b_ <- split(b)) yield UnaryRule(a_, b_, chain)
    }
    apply(coarse, split, splitRule _, proj)
  }

  def apply[C, F](coarse: RuleTopology[C], split: C=>Seq[F], splitRule: Rule[C]=>Seq[Rule[F]], proj: F=>C): GrammarRefinements[C, F] = {
    val fineIndex = {
      val index = Index[F]()
      for( l <- coarse.labelIndex; l2 <- split(l)) {
        index.index(l2)
      }
      index
    }
    val ruleIndex = {
      val index = Index[Rule[F]]()
      for( r <- 0 until coarse.index.size; split <- splitRule(coarse.index.get(r))) {
        index.index(split)
      }
      index
    }

    def projRule(r: Rule[F]) = r map proj

    val rules = ProjectionIndexer(coarse.index, ruleIndex, projRule)
    val labels = ProjectionIndexer(coarse.labelIndex, fineIndex, proj)
    new GrammarRefinements(labels, rules)
  }
}
