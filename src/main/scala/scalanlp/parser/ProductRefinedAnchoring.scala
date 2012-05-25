package scalanlp.parser

/**
 * Creates a product of two derivation scorers, seamlessly combining their
 * refinements as appropriate.
 *
 * This class is the main motivation for the "annotationTag" on
 * [[scalanlp.parser.RefinedAnchoring]] instances. If one of the annotation tags is "0"
 * then it does not use refinements, and so we can avoid clever games.
 *
 * Similarly, if the tags matched, then we can use the same tags. I'm not 100% convinced
 * this is necessary any more. But I have it for now.
 *
 * @author dlwh
 */

final case class ProductRefinedAnchoring[L,W](s1: RefinedAnchoring[L, W],
                                              s2: RefinedAnchoring[L, W],
                                              alpha: Double = 1.0) extends ProductRefinementsHandler(s1, s2) with RefinedAnchoring[L, W] {
  val grammar = s1.grammar
  def lexicon = s1.lexicon
  def words = s1.words

  override def annotationTag = {
    if(refinementController == null) -1
    else refinementController.annotationTag
  }

  def scoreSpan(begin: Int, end: Int, label: Int, ref: Int) = {
    val r1 = s1.scoreSpan(begin, end, label, label1Ref(label, ref))
    if(r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreSpan(begin, end, label, label2Ref(label, ref))
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int) = {
    val r1 = s1.scoreBinaryRule(begin, split, end, rule, rule1Ref(rule, ref))
    if(r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreBinaryRule(begin, split, end, rule, rule2Ref(rule, ref))
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int, ref: Int) = {
    val r1 = s1.scoreUnaryRule(begin, end, rule, rule1Ref(rule, ref))
    if(r1 == Double.NegativeInfinity) r1
    else r1 + alpha * s2.scoreUnaryRule(begin, end, rule, rule2Ref(rule, ref))
  }

  def validLabelRefinements(begin: Int, end: Int, label: Int) = {
    if(refinementController ne null) refinementController.validLabelRefinements(begin, end, label)
    else for(a <- s1.validLabelRefinements(begin, end, label);
             b <- s2.validLabelRefinements(begin, end, label))
            yield a * s1.numValidRefinements(label) + b
  }

  def numValidRefinements(label: Int) = {
    if(refinementController ne null) refinementController.numValidRefinements(label)
    else s1.numValidRefinements(label) * s2.numValidRefinements(label)
  }

  def numValidRuleRefinements(rule: Int) = {
    if(refinementController ne null) refinementController.numValidRuleRefinements(rule)
    else s1.numValidRuleRefinements(rule) * s2.numValidRuleRefinements(rule)
  }

  def validRuleRefinementsGivenParent(begin: Int, end: Int, rule: Int, parentRef: Int) = {
    if(refinementController ne null) refinementController.validRuleRefinementsGivenParent(begin, end, rule, parentRef)
    else {
      val parent = grammar.parent(rule)
      val bRefinements = s2.validRuleRefinementsGivenParent(begin, end, rule, label2Ref(parent, parentRef));
      for(a <- s1.validRuleRefinementsGivenParent(begin, end, rule, label1Ref(parent, parentRef));
          b <- bRefinements)
      yield a * s1.numValidRuleRefinements(rule) + b
    }
  }

  def validUnaryRuleRefinementsGivenChild(begin: Int, end: Int, rule: Int, childRef: Int) = {
    if(refinementController ne null) refinementController.validUnaryRuleRefinementsGivenChild(begin, end, rule, childRef)
    else {
      val child = grammar.child(rule)
      val bRefinements = s2.validUnaryRuleRefinementsGivenChild(begin, end, rule, label2Ref(child, childRef));
      for(a <- s1.validUnaryRuleRefinementsGivenChild(begin, end, rule, label1Ref(child, childRef));
          b <- bRefinements)
      yield a * s1.numValidRuleRefinements(rule) + b
    }
  }

  def leftChildRefinement(rule: Int, ruleRef: Int) = {
    if(refinementController ne null) refinementController.leftChildRefinement(rule,ruleRef)
    else {
      val l1 = s1.leftChildRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.leftChildRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s1.numValidRefinements(grammar.leftChild(rule)) + l2
    }
  }


  def rightChildRefinement(rule: Int, ruleRef: Int) = {
    if(refinementController ne null) refinementController.rightChildRefinement(rule,ruleRef)
    else {
      val l1 = s1.rightChildRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.rightChildRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s1.numValidRefinements(grammar.rightChild(rule)) + l2
    }
  }


  def parentRefinement(rule: Int, ruleRef: Int) = {
    if(refinementController ne null) refinementController.parentRefinement(rule,ruleRef)
    else {
      val l1 = s1.parentRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.parentRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s1.numValidRefinements(grammar.parent(rule)) + l2
    }
  }

  def childRefinement(rule: Int, ruleRef: Int) = {
    if(refinementController ne null) refinementController.childRefinement(rule,ruleRef)
    else {
      val l1 = s1.childRefinement(rule, rule1Ref(rule, ruleRef))
      val l2 = s2.childRefinement(rule, rule2Ref(rule, ruleRef))
      l1 * s1.numValidRefinements(grammar.child(rule)) + l2
    }
  }

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int) = {
    if(refinementController ne null) refinementController.ruleRefinementFromRefinements(r, refA, refB)
    else {
      val a1 = label1Ref(grammar.parent(r), refA)
      val a2 = label2Ref(grammar.parent(r), refA)
      val b1 = label1Ref(grammar.child(r), refB)
      val b2 = label2Ref(grammar.child(r), refB)
      val l1 = s1.ruleRefinementFromRefinements(r, a1, b1)
      val l2 = s2.ruleRefinementFromRefinements(r, a2, b2)
      l1 * s1.numValidRuleRefinements(r) + l2
    }
  }

  def ruleRefinementFromRefinements(r: Int, refA: Int, refB: Int, refC: Int) = {
    if(refinementController ne null) refinementController.ruleRefinementFromRefinements(r, refA, refB, refC)
    else {
      val a1 = label1Ref(grammar.parent(r), refA)
      val a2 = label2Ref(grammar.parent(r), refA)
      val b1 = label1Ref(grammar.leftChild(r), refB)
      val b2 = label2Ref(grammar.leftChild(r), refB)
      val c1 = label1Ref(grammar.rightChild(r), refC)
      val c2 = label2Ref(grammar.rightChild(r), refC)
      val l1 = s1.ruleRefinementFromRefinements(r, a1, b1, c1)
      val l2 = s2.ruleRefinementFromRefinements(r, a2, b2, c2)
      l1 * s1.numValidRuleRefinements(r) + l2
    }

  }
}

abstract class ProductRefinementsHandler[L, W](s1: RefinedAnchoring[L, W], s2: RefinedAnchoring[L, W]) {
  protected final val refinementController: RefinedAnchoring[L, W] = {
    if(s1.annotationTag == 0) s2
    else if(s2.annotationTag == 0) s1
    else if (s1.annotationTag < 0 || s2.annotationTag < 0) null
    else if(s1.annotationTag == s2.annotationTag) s1
    else null
  }

  @inline
  protected final def label1Ref(label: Int, ref: Int): Int = {
    if(refinementController != null) ref
    else {
      val num = s1.numValidRefinements(label)
      ref / num
    }
  }

  @inline
  protected final def label2Ref(label: Int, ref: Int): Int = {
    if(refinementController != null) ref
    else {
      val num = s1.numValidRefinements(label)
      ref % num
    }
  }

  @inline
  protected final def rule1Ref(rule: Int, ref: Int): Int = {
    if(refinementController != null) ref
    else {
      val num = s1.numValidRuleRefinements(rule)
      ref / num
    }
  }

  @inline
  protected final def rule2Ref(rule: Int, ref: Int): Int = {
    if(refinementController != null) ref
    else {
      val num = s1.numValidRuleRefinements(rule)
      ref % num
    }
  }
}
