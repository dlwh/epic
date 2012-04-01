package scalanlp.parser
package projections

import collection.immutable.BitSet
import scalala.library.Numerics
import java.util.Arrays
import scalanlp.config.Configuration
import java.io._
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}
import scalanlp.trees.AnnotatedLabel

/**
 * 
 * @author dlwh
 */

class ConstraintScorer[L](val scores: Array[BitSet], topScores: Array[BitSet]) extends SpanScorer[L] {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val set = topScores(TriangularArray.index(begin,end))
    if(set == null || !set.contains(rule)) Double.NegativeInfinity
    else 0.0
//    0.0
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val set = scores(TriangularArray.index(begin,end))
    if(set == null || !set.contains(tag)) Double.NegativeInfinity
    else 0.0
  }
}

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class ConstraintScorerFactory[C,L,W](parser: SimpleChartParser[C,L,W], threshold: Double) extends SpanScorer.Factory[C,L,W] {
  def indexedProjections = parser.projections.labels
  private val coarseUnaryIndex = new OpenAddressHashArray(indexedProjections.coarseIndex.size * indexedProjections.coarseIndex.size, -1)
  for(r@UnaryRule(a,b) <- parser.projections.rules.coarseIndex) {
    val ai = indexedProjections.coarseIndex(a)
    val bi = indexedProjections.coarseIndex(b)
    val ri = parser.projections.rules.coarseIndex(r)
    coarseUnaryIndex(bi + indexedProjections.coarseIndex.size * ai) = ri
  }

  private def ruleIndex(ai: Int, bi: Int) = {
    coarseUnaryIndex(bi + indexedProjections.coarseIndex.size * ai)
  }

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity, goldTags: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]) = {
    val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(charts,sentProb, scorer, goldTags)

    chartScorer
  }

  def scoresForCharts(scoresForLocation: Array[Double], begin: Int, end: Int, ichart: ParseChart[L]#ChartScores, ochart: ParseChart[L]#ChartScores, sentProb: Double, goldTags: GoldTagPolicy[C], scores: Array[collection.mutable.BitSet]) {
    val active = new collection.mutable.BitSet()
    Arrays.fill(scoresForLocation, Double.NegativeInfinity)
    val index = TriangularArray.index(begin, end)
    for (l <- ichart.enteredLabelIndexes(begin, end)) {
      val pL = indexedProjections.project(l)
      val myScore = ichart.labelScore(begin, end, l) + ochart.labelScore(begin, end, l) - sentProb
      val currentScore = scoresForLocation(pL)
      scoresForLocation(pL) = Numerics.logSum(currentScore, myScore)
      active += pL
    }

    for (c <- active) {
      val v = scoresForLocation(c)
      if (v > threshold || goldTags.isGoldTag(begin, end, c)) {
        if (scores(index) eq null) {
          scores(index) = new collection.mutable.BitSet()
        }
        scores(index) += c
      }
    }
  }

  def buildSpanScorer(charts: ChartPair[ParseChart,L], sentProb: Double,
                      coarseScorer: SpanScorer[C] = SpanScorer.identity,
                      goldTags: GoldTagPolicy[C] = GoldTagPolicy.noGoldTags[C]):ConstraintScorer[C] = {
    import charts._

    val scores = TriangularArray.raw(inside.length+1,null:collection.mutable.BitSet)
    val topScores = TriangularArray.raw(inside.length+1,null:collection.mutable.BitSet)
    val scoresForLocation = indexedProjections.coarseEncoder.fillArray(Double.NegativeInfinity)

    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      scoresForCharts(scoresForLocation, begin, end, inside.bot, outside.bot, sentProb, goldTags, scores)
      scoresForCharts(scoresForLocation, begin, end, inside.top, outside.top, sentProb, goldTags, topScores)

    }
    
    val unaryRulesAllowed = for( (t,b) <- topScores zip scores) yield {
      if( (t eq null) || (b eq null)) null
      else {
        val bitset = BitSet.empty ++ {for { a <- t; c <- b; r = ruleIndex(a,c) if r != -1} yield r}
        if(bitset.nonEmpty)
          bitset
        else null
      }
    }
//    println("Density: " + density * 1.0 / scores.length)
//    println("Label Density:" + labelDensity * 1.0 / scores.length / parser.projections.labels.coarseIndex.size)
    new ConstraintScorer[C](scores.map { e => if(e eq null) null else BitSet.empty ++ e}, unaryRulesAllowed)
  }

}

object ProjectTreebankToConstraints {

  import SpanBroker._

  def main(args: Array[String]) {
    val (baseConfig,files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProjectionParams]("")
    val treebank = params.processedTreebank.copy(treebank = params.processedTreebank.treebank.copy(maxLength = 100000))
    println(params)
    val parser = loadParser[Any](params.parser)

    val outDir = params.out
    outDir.mkdirs()

    val proj = new GrammarProjections(ProjectionIndexer.simple(parser.projections.labels.coarseIndex), ProjectionIndexer.simple(parser.projections.rules.coarseIndex))
    val trueProj = parser.projections
    if(params.project) {
      val factory = new ConstraintScorerFactory[AnnotatedLabel,Any,String](parser, -7)
      scalanlp.util.writeObject(new File(outDir,SPAN_INDEX_NAME),parser.projections.labels.coarseIndex)
      serializeSpans(mapTrees(factory,treebank.trainTrees, proj, true, params.maxParseLength),new File(outDir,TRAIN_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.testTrees, proj, false, 10000),new File(outDir,TEST_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.devTrees, proj, false, 10000),new File(outDir,DEV_SPANS_NAME))
    } else {
      val fineProj = new GrammarProjections(ProjectionIndexer.simple(parser.projections.labels.fineIndex), ProjectionIndexer.simple(parser.projections.rules.fineIndex))
      val nonprojectingParser = new SimpleChartParser(parser.builder,new SimpleViterbiDecoder[Any,String](parser.builder.grammar), fineProj)
      val factory = new ConstraintScorerFactory[Any,Any,String](nonprojectingParser, -7)
      scalanlp.util.writeObject(new File(outDir, SPAN_INDEX_NAME), parser.projections.labels.fineIndex)
      serializeSpans(mapTrees(factory,treebank.trainTrees, trueProj, true, params.maxParseLength),new File(outDir,TRAIN_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.testTrees, trueProj, false, 10000),new File(outDir,TEST_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.devTrees, trueProj, false, 10000),new File(outDir,DEV_SPANS_NAME))
    }
  }

  def loadParser[T](loc: File) = {
    val parser = scalanlp.util.readObject[SimpleChartParser[AnnotatedLabel,T,String]](loc)
    parser
  }

  def mapTrees[L](factory: ConstraintScorerFactory[L,Any,String], trees: IndexedSeq[TreeInstance[AnnotatedLabel,String]],
                  proj: GrammarProjections[AnnotatedLabel,L], useTree: Boolean, maxL: Int) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[AnnotatedLabel,String]) =>
      val TreeInstance(id,tree,words,preScorer) = ti
      println(id,words)
      try {
        val pruner:GoldTagPolicy[L] = if(useTree) {
          val mappedTree = tree.map(l => proj.labels.refinementsOf(l.baseAnnotatedLabel).map(proj.labels.fineIndex))
          GoldTagPolicy.candidateTreeForcing(mappedTree)
        } else {
          GoldTagPolicy.noGoldTags
        }
        val scorer = factory.mkSpanScorer(words,new ProjectingSpanScorer(proj, preScorer), pruner)
        id -> scorer
      } catch {
        case e: Exception => e.printStackTrace(); id -> SpanScorer.identity[L]
      }
    }.seq
  }


}