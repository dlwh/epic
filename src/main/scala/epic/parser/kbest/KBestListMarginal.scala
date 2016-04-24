package epic.parser.kbest

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
import epic.parser._
import breeze.linalg._
import breeze.numerics._

case class KBestListMarginal[L, W](anchoring: GrammarAnchoring[L, W],
                                   marginals: IndexedSeq[ParseMarginal[L, W]]) extends ParseMarginal[L, W] {

  def isMaxMarginal: Boolean = false

  def this(marg: ParseMarginal[L, W]) = this(marg.anchoring, IndexedSeq(marg))

  val probsPerTree = DenseVector(marginals.map(_.logPartition):_*)
  val logPartition: Double = softmax(probsPerTree)
  probsPerTree -= logPartition
  exp.inPlace(probsPerTree)

  /**
   * Forest traversal that visits spans in a "bottom up" order.
   * @param spanVisitor
   */
  def visitPostorder(spanVisitor: AnchoredVisitor[L], spanThreshold: Double) {
    for ((m, i) <- marginals.zipWithIndex if probsPerTree(i) >= math.exp(spanThreshold)) {
      m.visitPostorder(new AnchoredVisitor[L] {
        def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitUnaryRule(begin, end, rule, ref, score * probsPerTree(i))
        }

        def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
          spanVisitor.visitSpan(begin, end, tag, ref, score * probsPerTree(i))
        }

        def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {
          spanVisitor.visitBinaryRule(begin, split, end, rule, ref, score * probsPerTree(i))
        }
      }, spanThreshold - math.log(probsPerTree(i)))
    }
  }

  override def insideBotScore(begin: Int, end: Int, sym: Int, ref: Int): Double = ???

  override def insideTopScore(begin: Int, end: Int, sym: Int, ref: Int): Double = ???

  override def feasibleSplitPoints(begin: Int, end: Int, leftChild: Int, leftChildRef: Int, rightChild: Int, rightChildRef: Int): IndexedSeq[Int] = {
    marginals.flatMap(_.feasibleSplitPoints(begin, end, leftChild, leftChildRef, rightChild, rightChildRef)).toSet.toIndexedSeq.sorted
  }
}
