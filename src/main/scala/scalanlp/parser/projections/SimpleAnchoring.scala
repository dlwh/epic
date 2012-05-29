package scalanlp.parser
package projections

import java.io._
import projections.AnchoredRuleProjector.AnchoredData
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}

/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
case class AnchoredPCFGProjector[L, W](grammar: BaseGrammar[L], threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {

  type MyScorer = SimpleAnchoring[L, W]
  private def normalize(ruleScores: OpenAddressHashArray[Double], totals: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if(ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize * 3 / 2)
      for( (rule, score) <- ruleScores.pairsIterator) {
        val parent = grammar.parent(rule)
        if(score > 0)
          r(rule) = math.log(score) - math.log(totals(parent))
//        r(rule) = score - totals(parent)
      }
      r
    }
  }

  protected def createSpanScorer(charts: Marginal[L, W], ruleData: AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, totalsUnaries, binaryScores, totalsBinaries) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = for((ruleScores, totals) <- unaryScores zip totalsUnaries) yield {
      normalize(ruleScores, totals)
    }

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for ((splits, totals) <- binaryScores zip totalsBinaries) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores, totals)
    }
    new SimpleAnchoring(charts.grammar, charts.lexicon, charts.words, lexicalScores, normUnaries, normBinaries)
  }

}


/**
 * Creates labeled span scorers for a set of trees from some parser. Projects from L to C.
 * @author dlwh
 */
case class AnchoredRuleMarginalProjector[L, W](threshold: Double = Double.NegativeInfinity) extends ChartProjector[L, W] {
  private def normalize(ruleScores: OpenAddressHashArray[Double]):OpenAddressHashArray[Double] = {
    if(ruleScores eq null) null
    else {
      val r = new OpenAddressHashArray[Double](ruleScores.length, Double.NegativeInfinity, ruleScores.activeSize)
      for( (rule, score) <- ruleScores.pairsIterator) {
        r(rule) = math.log(score)
      }
      r
    }
  }

  type MyScorer = SimpleAnchoring[L, W]


  protected def createSpanScorer(charts: Marginal[L, W], ruleData: AnchoredData, sentProb: Double) = {
    val AnchoredRuleProjector.AnchoredData(lexicalScores, unaryScores, _, binaryScores, _) = ruleData
    val normUnaries:Array[OpenAddressHashArray[Double]] = for(ruleScores <- unaryScores) yield {
      normalize(ruleScores)
    }

    val normBinaries:Array[Array[OpenAddressHashArray[Double]]] = for (splits <- binaryScores) yield {
      if(splits eq null) null
      else for(ruleScores <- splits) yield normalize(ruleScores)
    }
    new SimpleAnchoring(charts.grammar, charts.lexicon, charts.words, lexicalScores, normUnaries, normBinaries)
  }
}


@SerialVersionUID(3)
class SimpleAnchoring[L, W](val grammar: BaseGrammar[L],
                               val lexicon: Lexicon[L, W],
                               val words: Seq[W],
                               spanScores: Array[OpenAddressHashArray[Double]], // triangular index -> label -> score
                               // (begin, end) -> rule -> score
                               unaryScores: Array[OpenAddressHashArray[Double]],
                               // (begin, end) -> (split-begin) -> rule -> score
                               binaryScores: Array[Array[OpenAddressHashArray[Double]]]) extends CoreAnchoring[L, W] with Serializable {

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

