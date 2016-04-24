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
import epic.trees._
import breeze.numerics.{logI, Scaling}
import breeze.linalg.{Counter, Counter2}

/**
 * This isn't really a marginal, except in a degenerate sense.
 * It gives the likelihood of a known fixed tree under the
 * Anchoring as well as expected counts (i.e. count the
 * occurrences of each rule.)
 *
 * @param anchoring The grammar anchoring
 * @param tree A tree that has been decorated with
 *             the gold refinements at each leaf
 * @author dlwh
 */
case class TreeMarginal[L, W](anchoring: GrammarAnchoring[L, W],
                              tree: BinarizedTree[(L,Int)]) extends ParseMarginal[L, W] {

  val logPartition = {
    var score = 0.0
    def rec(t: BinarizedTree[(L,Int) ]):Unit = t match {
      case n@NullaryTree( (a, ref), span ) =>
        val aI = topology.labelIndex(a)
        score += anchoring.scoreSpan(span.begin, span.end, aI, ref)
        if (score.isInfinite) throw new Exception(s"Could not score the terminal with tag ${a -> ref} at $span. $words")
      case UnaryTree( (a, refA), child@Tree((b, refB), _, _), chain,  span) =>
        val r = topology.index(UnaryRule(a, b, chain))
        assert(r != -1, "Could not find rule " + UnaryRule(a, b, chain))
        val ruleRef = anchoring.ruleRefinementFromRefinements(r, refA, refB)
        if (ruleRef < 0) throw new Exception(s"Bad refined rule in gold tree!: ${UnaryRule(a, b, chain)} aRef: $refA  bRef: $refB")

        score += anchoring.scoreUnaryRule(t.span.begin, t.span.end, r, ruleRef)

        if (score.isInfinite) throw new Exception(s"Could not score gold tree!\n Partial Tree: ${t.render(words)}\n Full Tree: ${tree.render(words)}\n ")
        rec(child)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _, _), ct@Tree((c, refC), _, _), span) =>
        val aI = topology.labelIndex(a)
        val rule = topology.index(BinaryRule(a, b, c))
        val ruleRef = anchoring.ruleRefinementFromRefinements(rule, refA, refB, refC)
        score += anchoring.scoreSpan(t.span.begin, t.span.end, aI, refA)
        score += anchoring.scoreBinaryRule(t.span.begin, bt.span.end, t.span.end, rule, ruleRef)
        if (score.isInfinite) throw new Exception("Could not score gold tree!" + t.render(words))
        rec(bt)
        rec(ct)
    }
    rec(tree)

    score
  }

  def visitPostorder(visitor: AnchoredVisitor[L], threshold: Double = Double.NegativeInfinity) {
    tree.postorder foreach {
      case n@NullaryTree( (a, ref), span ) =>
        val aI = topology.labelIndex(a)
        visitor.visitSpan(n.span.begin, n.span.end, aI, ref, 1.0)
      case t@UnaryTree( (a, refA), Tree((b, refB), _, _), chain, span) =>
        val r = topology.index(UnaryRule(a, b, chain))
        val ruleRef = anchoring.ruleRefinementFromRefinements(r, refA, refB)
        if (ruleRef < 0) throw new Exception(s"Bad refined rule in gold tree!: ${UnaryRule(a, b, chain)}  aRef: $refA  bRef: $refB")
        visitor.visitUnaryRule(t.span.begin, t.span.end, r, ruleRef, 1.0)
      case t@BinaryTree( (a, refA), bt@Tree( (b, refB), _, _), Tree((c, refC), _, _), span) =>
        val aI = topology.labelIndex(a)
        val rule = topology.index(BinaryRule(a, b, c))
        val ruleRef = anchoring.ruleRefinementFromRefinements(rule, refA, refB, refC)
        visitor.visitSpan(t.span.begin, t.span.end, aI, refA, 1.0)
        visitor.visitBinaryRule(t.span.begin, bt.span.end, t.span.end, rule, ruleRef, 1.0)
    }
  }

  def isMaxMarginal: Boolean = true

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    tree.findSpan(begin, end) match {
      case Some(UnaryTree(a, b@BinaryTree(_, _, _, _), chain, _)) => IndexedSeq(b.splitPoint)
      case Some(b@BinaryTree(_, _, _, _)) => IndexedSeq(b.splitPoint)
      case _ => IndexedSeq.empty
    }
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    tree.findSpan(begin, end) match {
      case Some(UnaryTree(_, Tree(a, _, span2), chain, span)) => logI(a == (sym -> ref))
      case _ => Double.NegativeInfinity
    }
  }

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = {
    tree.findSpan(begin, end) match {
      case Some(UnaryTree(a, _, chain, span)) => logI(a == (sym -> ref))
      case _ => Double.NegativeInfinity
    }
  }

  def marginalAt(begin: Int, end: Int): Counter2[L, Int, Double] = {
    tree.findSpan(begin, end) match {
      case None => Counter2[L, Int, Double]()
      case Some(Tree((a, ref), _, _)) => Counter2((a, ref, 1.0))
    }
  }
}

object TreeMarginal {
  def apply[L, W](grammar: Grammar[L, W],
                  words: IndexedSeq[W],
                  tree: BinarizedTree[(L,Int)]):TreeMarginal[L, W] = {
    TreeMarginal(grammar.anchor(words), tree)
  }
}