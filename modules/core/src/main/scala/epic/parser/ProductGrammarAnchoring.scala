package epic.parser

import collection.immutable.BitSet
import epic.constraints.ChartConstraints

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
/**
 * Creates a product of two derivation scorers, seamlessly combining their
 * refinements as appropriate.
 *
 * This class is the main motivation for the "annotationTag" on
 * [[epic.parser.GrammarAnchoring]] instances. If one of the annotation tags is "0"
 * then it does not use refinements, and so we can avoid clever games.
 *
 * Similarly, if the tags matched, then we can use the same tags. I'm not 100% convinced
 * this is necessary any more. But I have it for now.
 *
 * @author dlwh
 */

final case class ProductGrammarAnchoring[L,W](s1: GrammarAnchoring[L, W],
                                              s2: GrammarAnchoring[L, W],
                                              alpha: Double = 1.0) extends ProductRefinementsHandler(s1, s2) with GrammarAnchoring[L, W] {
  val topology = s1.topology
  def lexicon = s1.lexicon
  def words = s1.words

  override def addConstraints(constraints: ChartConstraints[L]): GrammarAnchoring[L, W] = copy(s1.addConstraints(constraints))

  override val sparsityPattern: ChartConstraints[L] = s1.sparsityPattern & s2.sparsityPattern

  override def annotationTag = {
    if (refinementController == null) -1
    else refinementController.annotationTag
  }

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    val r1 = s1.scoreSpan(begin, end, label, label1Ref(label, ref))
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreSpan(begin, end, label, label2Ref(label, ref))
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    val r1 = s1.scoreBinaryRule(begin, split, end, rule, rule1Ref(rule, ref))
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreBinaryRule(begin, split, end, rule, rule2Ref(rule, ref))
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    val r1 = s1.scoreUnaryRule(begin, end, rule, rule1Ref(rule, ref))
    if (r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreUnaryRule(begin, end, rule, rule2Ref(rule, ref))
  }

  def validLabelRefinements(begin: Int, end: Int, label: Int) = {
    if (refinementController ne null) refinementController.validLabelRefinements(begin, end, label)
    else for(a <- s1.validLabelRefinements(begin, end, label);
             b <- s2.validLabelRefinements(begin, end, label))
            yield a * s2.numValidRefinements(label) + b
  }

  def numValidRefinements(label: Int) = {
    if (refinementController ne null) refinementController.numValidRefinements(label)
    else s1.numValidRefinements(label) * s2.numValidRefinements(label)
  }

  def numValidRuleRefinements(rule: Int) = {
    if (refinementController ne null) refinementController.numValidRuleRefinements(rule)
    else s1.numValidRuleRefinements(rule) * s2.numValidRuleRefinements(rule)
  }

  def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
    if (refinementController ne null) refinementController.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
    else {
      val parent = topology.parent(rule)
      val bRefinements = s2.validRuleRefinementsGivenParent(begin, end, rule, label2Ref(parent, parentRef))
      for(a <- s1.validRuleRefinementsGivenParent(begin, end, rule, label1Ref(parent, parentRef));
          b <- bRefinements)
      yield a * s2.numValidRuleRefinements(rule) + b
    }
  }

  def validRuleRefinementsGivenLeftChild(begin: Int, split: Int, completionBegin: Int, completionEnd: Int, rule: Int, leftChildRef: Int): Array[Int] = {
    if (refinementController ne null) refinementController.validRuleRefinementsGivenLeftChild(begin, split, completionBegin, completionEnd, rule, leftChildRef)
    else {
      val leftChild = topology.leftChild(rule)
      val bRefinements = s2.validRuleRefinementsGivenLeftChild(begin, split, completionBegin, completionEnd, rule, label2Ref(leftChild, leftChildRef))
      for(a <- s1.validRuleRefinementsGivenLeftChild(begin, split, completionBegin, completionEnd, rule, label1Ref(leftChild, leftChildRef));
          b <- bRefinements)
      yield a * s2.numValidRuleRefinements(rule) + b
    }
  }

  def validRuleRefinementsGivenRightChild(completionBegin: Int, completionEnd: Int, split: Int, end: Int, rule: Int, rightChildRef: Int): Array[Int] = {
    if (refinementController ne null) refinementController.validRuleRefinementsGivenRightChild(completionBegin, completionEnd, split, end, rule, rightChildRef)
    else {
      val rightChild = topology.rightChild(rule)
      val bRefinements = s2.validRuleRefinementsGivenRightChild(completionBegin, completionEnd, split, end, rule, label2Ref(rightChild, rightChildRef))
      for(a <- s1.validRuleRefinementsGivenRightChild(completionBegin, completionEnd, split, end, rule, label1Ref(rightChild, rightChildRef));
          b <- bRefinements)
      yield a * s2.numValidRuleRefinements(rule) + b
    }
  }

  def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
    if (refinementController ne null) refinementController.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
    else {
      val child = topology.child(rule)
      val bRefinements = s2.validUnaryRuleRefinementsGivenChild(begin, end, rule, label2Ref(child, childRef))
      for(a <- s1.validUnaryRuleRefinementsGivenChild(begin, end, rule, label1Ref(child, childRef));
          b <- bRefinements)
      yield a * s2.numValidRuleRefinements(rule) + b
    }
  }

  def leftChildRefinement(rule: Int, ruleRef: Int) = {
    if (refinementController ne null) refinementController.leftChildRefinement(rule,ruleRef)
    else {
      val l1 = s1.leftChildRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.leftChildRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s2.numValidRefinements(topology.leftChild(rule)) + l2
    }
  }


  def rightChildRefinement(rule: Int, ruleRef: Int) = {
    if (refinementController ne null) refinementController.rightChildRefinement(rule,ruleRef)
    else {
      val l1 = s1.rightChildRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.rightChildRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s2.numValidRefinements(topology.rightChild(rule)) + l2
    }
  }


  def parentRefinement(rule: Int, ruleRef: Int) = {
    if (refinementController ne null) refinementController.parentRefinement(rule,ruleRef)
    else {
      val l1 = s1.parentRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.parentRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s2.numValidRefinements(topology.parent(rule)) + l2
    }
  }

  def childRefinement(rule: Int, ruleRef: Int) = {
    if (refinementController ne null) refinementController.childRefinement(rule,ruleRef)
    else {
      val l1 = s1.childRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.childRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s2.numValidRefinements(topology.child(rule)) + l2
    }
  }

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
    if (refinementController ne null) refinementController.ruleRefinementFromRefinements(r, refA, refB)
    else {
      val a1 = label1Ref(topology.parent(r), refA)
      val a2 = label2Ref(topology.parent(r), refA)
      val b1 = label1Ref(topology.child(r), refB)
      val b2 = label2Ref(topology.child(r), refB)
      val l1 = s1.ruleRefinementFromRefinements(r, a1, b1)
      val l2 = s2.ruleRefinementFromRefinements(r, a2, b2)
      if (l1 < 0 || l2 < 0) -1
      else l1 * s2.numValidRuleRefinements(r) + l2
    }
  }

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
    if (refinementController ne null) refinementController.ruleRefinementFromRefinements(r, refA, refB, refC)
    else {
      val a1 = label1Ref(topology.parent(r), refA)
      val a2 = label2Ref(topology.parent(r), refA)
      val b1 = label1Ref(topology.leftChild(r), refB)
      val b2 = label2Ref(topology.leftChild(r), refB)
      val c1 = label1Ref(topology.rightChild(r), refC)
      val c2 = label2Ref(topology.rightChild(r), refC)
      val l1 = s1.ruleRefinementFromRefinements(r, a1, b1, c1)
      val l2 = s2.ruleRefinementFromRefinements(r, a2, b2, c2)
      if (l1 < 0 || l2 < 0) -1
      else l1 * s2.numValidRuleRefinements(r) + l2
    }

  }

  def validCoarseRulesGivenParentRefinement(a: Int, refA: Int) = {
    if (refinementController ne null) refinementController.validCoarseRulesGivenParentRefinement(a, refA)
    else {
      val a1 = label1Ref(a, refA)
      val a2 = label2Ref(a, refA)
      s1.validCoarseRulesGivenParentRefinement(a, a1).filter(BitSet.empty ++ s2.validCoarseRulesGivenParentRefinement(a, a2))
    }
  }


  def validParentRefinementsGivenRule(begin: Int, splitBegin: Int, splitEnd: Int, end: Int, rule: Int): Array[Int] = {
    if (refinementController ne null) refinementController.validParentRefinementsGivenRule(begin, splitBegin, splitEnd, end, rule)
    else {
      val r1arr = s1.validParentRefinementsGivenRule(begin, splitBegin, splitEnd, end, rule)
      val r2arr = s2.validParentRefinementsGivenRule(begin, splitBegin, splitEnd, end, rule)
      val num2 = s2.numValidRefinements(topology.parent(rule))
      for (r1 <- r1arr; r2 <- r2arr) yield r1 * num2 + r2
    }

  }


  def validLeftChildRefinementsGivenRule(begin: Int, end: Int, completionBegin: Int, completionEnd: Int, rule: Int): Array[Int] = {
    if (refinementController ne null) refinementController.validLeftChildRefinementsGivenRule(begin, end, completionBegin, completionEnd, rule)
    else {
      val r1arr = s1.validLeftChildRefinementsGivenRule(begin, end, completionBegin, completionEnd, rule)
      val r2arr = s2.validLeftChildRefinementsGivenRule(begin, end, completionBegin, completionEnd, rule)
      val num2 = s2.numValidRefinements(topology.parent(rule))
      for (r1 <- r1arr; r2 <- r2arr) yield r1 * num2 + r2
    }
  }

  def validRightChildRefinementsGivenRule(completionBegin: Int, completionEnd: Int, begin: Int, end: Int, rule: Int): Array[Int] = {
    if (refinementController ne null) refinementController.validRightChildRefinementsGivenRule(completionBegin, completionEnd, begin, end, rule)
    else {
      val r1arr = s1.validRightChildRefinementsGivenRule(completionBegin, completionEnd, begin, end, rule)
      val r2arr = s2.validRightChildRefinementsGivenRule(completionBegin, completionEnd, begin, end, rule)
      val num2 = s2.numValidRefinements(topology.parent(rule))
      for (r1 <- r1arr; r2 <- r2arr) yield r1 * num2 + r2
    }
  }
}

abstract class ProductRefinementsHandler[L, W](s1: GrammarAnchoring[L, W], s2: GrammarAnchoring[L, W]) {
  protected final val refinementController: GrammarAnchoring[L, W] = {
    if (s1.annotationTag == 0) s2
    else if (s2.annotationTag == 0) s1
    else if (s1.annotationTag < 0 || s2.annotationTag < 0) null
    else if (s1.annotationTag == s2.annotationTag) s1
    else null
  }

  @inline
  protected final def label1Ref(label: Int, ref: Int): Int = {
    if (refinementController != null) ref
    else {
      val num = s1.numValidRefinements(label)
      ref / num
    }
  }

  @inline
  protected final def label2Ref(label: Int, ref: Int): Int = {
    if (refinementController != null) ref
    else {
      val num = s1.numValidRefinements(label)
      ref % num
    }
  }

  @inline
  protected final def rule1Ref(rule: Int, ref: Int): Int = {
    if (refinementController != null) ref
    else {
      val num = s1.numValidRuleRefinements(rule)
      ref / num
    }
  }

  @inline
  protected final def rule2Ref(rule: Int, ref: Int): Int = {
    if (refinementController != null) ref
    else {
      val num = s1.numValidRuleRefinements(rule)
      ref % num
    }
  }
}
