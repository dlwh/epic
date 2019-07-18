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
import projections.AnchoredForestProjector.ForestData
import breeze.collection.mutable.{OpenAddressHashArray, TriangularArray}
import epic.lexicon.Lexicon
import epic.constraints.ChartConstraints
import epic.trees.Span
import java.util

/**
 * Creates a locally-normalized anchored PCFG from some refined forest.
 * @author dlwh
 */
case class AnchoredPCFGProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {

  type MyAnchoring = EnumeratedAnchoring[L, W]
  private def normalize(grammar: RuleTopology[L], ruleScores: OpenAddressHashArray[Double], totals: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if (ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        val parent = grammar.parent(rule)
        if (score > 0)
          r(rule) = math.log(score) - math.log(totals(parent))
      }
      r
    }
  }

  private def logify(ruleScores: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if (ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  protected def createAnchoring(charts: ParseMarginal[L, W], ruleData: ForestData, sentProb: Double) = {
    val AnchoredForestProjector.ForestData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totalsBinaries) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = for((ruleScores, totals) <- unaryScores zip totalsUnaries) yield {
      normalize(charts.topology, ruleScores, totals)
    }

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for ((splits, totals) <- binaryScores zip totalsBinaries) yield {
      if (splits eq null) null
      else for(ruleScores <- splits) yield normalize(charts.topology, ruleScores, totals)
    }
    val sparsity = charts.anchoring.sparsityPattern
    new EnumeratedAnchoring(charts.topology, charts.lexicon, charts.words, lexicalScores.map(logify), normUnaries, normBinaries, sparsity)
  }

}

/**
 * Creates anchorings for a set of trees from some parser using p(rule | sentence) marginals.
 * @author dlwh
 */
@SerialVersionUID(469174684243960202L)
case class AnchoredRuleMarginalProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {
  private def normalize(ruleScores: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if (ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.activeIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  type MyAnchoring = EnumeratedAnchoring[L, W]

  protected def createAnchoring(charts: ParseMarginal[L, W],
                                ruleData: ForestData,
                                sentProb: Double) = {
    val AnchoredForestProjector.ForestData(lexicalScores, unaryScores, _, binaryScores, _) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = unaryScores.map(normalize)

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for (splits <- binaryScores) yield {
      if (splits eq null) null
      else splits.map(normalize)
    }
    val sparsity = charts.anchoring.sparsityPattern
    new EnumeratedAnchoring(charts.topology, charts.lexicon, charts.words, lexicalScores.map(normalize), normUnaries, normBinaries, sparsity)
  }
}

/**
 * An [[epic.parser.projections.EnumeratedAnchoring]] is a compressed forest, with potentially distinct
 * rule and span scores for every possible anchoring into the sentence. Commonly produced by either
 * an [[epic.parser.projections.AnchoredPCFGProjector]] (for PCFG forests) or an [[epic.parser.projections.AnchoredRuleMarginalProjector]]
 * (for rule marginal forests).
 * @tparam L
 * @tparam W
 */
@SerialVersionUID(3)
case class EnumeratedAnchoring[L, W](topology: RuleTopology[L],
                                     lexicon: Lexicon[L, W],
                                     words: IndexedSeq[W],
                                     spanScores: Array[OpenAddressHashArray[Double]], // triangular index -> label -> score
                                     // (begin, end) -> rule -> score
                                     unaryScores: Array[OpenAddressHashArray[Double]],
                                     // (begin, end) -> (split-begin) -> rule -> score
                                     binaryScores: Array[Array[OpenAddressHashArray[Double]]],
                                     sparsityPattern: ChartConstraints[L]) extends UnrefinedGrammarAnchoring[L, W] with Serializable {


  override def addConstraints(cs: ChartConstraints[L]): UnrefinedGrammarAnchoring[L, W] = copy(sparsityPattern = sparsityPattern & cs)

  /**
   * Computes the pointwise division of two grammars, augmenting
   * their refinement space to reflect this. If they share the same annotationTag,
   * (assuming it's non-negative) they will share their state space. (That is,
   * they will have the same annotations.)
   * @param other
   * @return
   */
  override def /(other: GrammarAnchoring[L, W]): GrammarAnchoring[L, W] = {
    other match {
      case that: EnumeratedAnchoring[L, W] => EnumeratedAnchoring.divide(this, that)
      case _ => super./(other)
    }
  }

  /**
   * Computes the point-wise division of this grammar with some other grammar.
   *
   * Note that scores are in log space, so we actually subtract scores.
   * @param other
   * @return
   */
  override def /(other: UnrefinedGrammarAnchoring[L, W]): UnrefinedGrammarAnchoring[L, W] = {
    other match {
      case that: EnumeratedAnchoring[L, W] => EnumeratedAnchoring.divide(this, that)
      case _ => super./(other)
    }
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val forSpan = unaryScores(TriangularArray.index(begin, end))
    if (forSpan eq null) Double.NegativeInfinity
    else forSpan(rule)
  }

  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val ti = TriangularArray.index(begin, end)
    val forSpan = binaryScores(ti)
    val cached = checkCache(split, rule, ti)
    if (!java.lang.Double.isNaN(cached)) {
      cached
    } else if (forSpan eq null) {
      Double.NegativeInfinity
    } else {
      val forSplit = forSpan(split - begin)
      val result =  if (forSplit eq null) Double.NegativeInfinity
      else forSplit(rule)
      updateCache(split, rule, ti, result)
      result
    }
  }

  def scoreSpan(begin: Int, end: Int, tag: Int): Double = {
    val scores = spanScores(TriangularArray.index(begin, end))
    if (scores ne null) scores(tag)
    else Double.NegativeInfinity
  }

  // (1 entry for each position (a split point), an entry has a rule index, a begin/end pair, and a score
  private val cache = new Array[Int](length * (1 + 1 + 2))
  util.Arrays.fill(cache, -1) //

  private def checkCache(splitPoint: Int, rule: Int, ti: Int) = {
    val crule = cache(splitPoint * 4)
    val cti = cache(splitPoint * 4 + 1)
    if (rule == crule && cti == ti) {
      java.lang.Double.longBitsToDouble(Span(cache(splitPoint * 4 + 2), cache(splitPoint * 4 + 3)).encoded)
    } else {
      Double.NaN
    }
  }

  private def updateCache(splitPoint: Int, rule: Int, ti: Int, score: Double):Unit = {
    cache(splitPoint * 4) = rule
    cache(splitPoint * 4 + 1) = ti
    val asSpan = new Span(java.lang.Double.doubleToRawLongBits(score))
    cache(splitPoint * 4 + 2) = asSpan.begin
    cache(splitPoint * 4 + 3) = asSpan.end
  }

}

object EnumeratedAnchoring {
  def divide[L, W](a: EnumeratedAnchoring[L, W], b: EnumeratedAnchoring[L, W]): EnumeratedAnchoring[L, W] =  {
    val newSpanScores = Array.tabulate(a.spanScores.length) { i =>
      val oldA = a.spanScores(i)
      val oldB = b.spanScores(i)
      if (null == oldA || null == oldB) {
        null
      } else {
        doDivide(oldA, oldB)
      }
    }

    val newUnaryScores = Array.tabulate(a.unaryScores.length) { i =>
      val oldA = a.unaryScores(i)
      val oldB = b.unaryScores(i)
      if (null == oldA || null == oldB) {
        null
      } else {
        doDivide(oldA, oldB)
      }
    }

    val newBinaryScores = Array.tabulate(a.binaryScores.length) { i =>
      val aArray = a.binaryScores(i)
      val bArray = b.binaryScores(i)
      if (null == aArray || null == bArray) {
        null
      } else {
        Array.tabulate(aArray.length) { split =>
          val oldA = aArray(split)
          val oldB = bArray(split)
          if (null == oldA || null == oldB) {
            null
          } else {
            doDivide(oldA, oldB)
          }
        }
      }
    }

    a.copy(spanScores = newSpanScores, unaryScores=newUnaryScores, binaryScores = newBinaryScores)

  }

  private def doDivide(a: OpenAddressHashArray[Double], b: OpenAddressHashArray[Double]) = {
    if (a == null || b == null) {
      null
    } else {
      val oah = new OpenAddressHashArray[Double](a.size, a.default, a.activeSize min b.activeSize)
      var off = 0
      while (off < a.iterableSize) {
        if (a.isActive(off)) {
          val aa = a.valueAt(off)
          val ii = a.indexAt(off)
          val bb = b(ii)
          if (aa != Double.NegativeInfinity && bb != Double.NegativeInfinity) {
            oah(ii) = aa - bb
          }
        }
        off += 1
      }
      oah
    }
  }
}
