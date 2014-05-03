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
import java.io._
import projections.AnchoredRuleProjector.AnchoredData
import breeze.collection.mutable.{OpenAddressHashArray, TriangularArray}
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints

/**
 * Creates a locally-normalized anchored PCFG from some refined forest.
 * @author dlwh
 */
case class AnchoredPCFGProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {

  type MyAnchoring = SimpleAnchoring[L, W]
  private def normalize(grammar: RuleTopology[L], ruleScores: OpenAddressHashArray[Double], totals: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if(ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        val parent = grammar.parent(rule)
        if(score > 0)
          r(rule) = math.log(score) - math.log(totals(parent))
      }
      r
    }
  }

  private def logify(ruleScores: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if(ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  protected def createAnchoring(charts: ParseMarginal[L, W], ruleData: AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totalsBinaries) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = for((ruleScores, totals) <- unaryScores zip totalsUnaries) yield {
      normalize(charts.topology, ruleScores, totals)
    }

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for ((splits, totals) <- binaryScores zip totalsBinaries) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(charts.topology, ruleScores, totals)
    }
//    val sparsity = charts.anchoring.sparsityPattern
    new SimpleAnchoring(charts.topology, charts.lexicon, charts.words, lexicalScores.map(logify), normUnaries, normBinaries)
  }

}


/**
 * Creates anchorings for a set of trees from some parser using p(rule | sentence) marginals.
 * @author dlwh
 */
case class AnchoredRuleMarginalProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {
  private def normalize(ruleScores: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if(ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  type MyAnchoring = SimpleAnchoring[L, W]

  protected def createAnchoring(charts: ParseMarginal[L, W],
                                ruleData: AnchoredData,
                                sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, _, binaryScores, _) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = unaryScores.map(normalize)

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for (splits <- binaryScores) yield {
      if(splits eq null) null
      else splits.map(normalize)
    }
//    val sparsity = charts.anchoring.sparsityPattern
    new SimpleAnchoring(charts.topology, charts.lexicon, charts.words, lexicalScores.map(normalize), normUnaries, normBinaries)//, sparsity)
  }
}

@SerialVersionUID(3)
case class SimpleAnchoring[L, W](topology: RuleTopology[L],
                            lexicon: Lexicon[L, W],
                            words: IndexedSeq[W],
                            spanScores: Array[OpenAddressHashArray[Double]], // triangular index -> label -> score
                            // (begin, end) -> rule -> score
                            unaryScores: Array[OpenAddressHashArray[Double]],
                            // (begin, end) -> (split-begin) -> rule -> score
                            binaryScores: Array[Array[OpenAddressHashArray[Double]]]) extends CoreAnchoring[L, W] with Serializable {
//  def addConstraints(cs: ChartConstraints[L]): CoreAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & cs)

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val forSpan = binaryScores(TriangularArray.index(begin, end))
    if(forSpan eq null) Double.NegativeInfinity
    else {
      val forSplit = forSpan(split - begin)
      if(forSplit eq null) Double.NegativeInfinity
      else forSplit(rule)
    }
  }
  def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if(scores ne null) scores(tag)
    else Double.NegativeInfinity
  }
}
