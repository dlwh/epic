package scalanlp.parser
package projections

import collection.immutable.BitSet
import scalala.library.Numerics
import java.util.Arrays
import scalanlp.config.Configuration
import java.io._
import scalanlp.collection.mutable.{OpenAddressHashArray, TriangularArray}
import scalanlp.trees._
import scalanlp.util.TypeTags._
import scalanlp.util.Index

/**
 * 
 * @author dlwh
 */

class ConstraintScorer[L](val scores: Array[BitSet], topScores: Array[BitSet]) extends SpanScorer[L] {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val set = topScores(TriangularArray.index(begin, end))
    if(set == null || !set.contains(rule)) Double.NegativeInfinity
    else 0.0
//    0.0
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val set = scores(TriangularArray.index(begin, end))
    if(set == null || !set.contains(tag)) Double.NegativeInfinity
    else 0.0
  }
}

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class ConstraintScorerFactory[L, W](parser: ChartBuilder[ParseChart, L, W], threshold: Double) extends SpanScorer.Factory[L, W] {

  def mkSpanScorer(s: Seq[W], goldTags: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]) = {
    val charts = parser.charts(s)
    val chartScorer = buildSpanScorer(charts, goldTags)
    chartScorer
  }

  private def scoresForCharts(marg: ChartMarginal[ParseChart, L, W], gold: GoldTagPolicy[L]) = {
    val length = marg.length
    val scores = TriangularArray.raw(length+1, null: Array[Double])
    val topScores = TriangularArray.raw(length+1, null: Array[Double])
    val visitor = new AnchoredSpanVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {}

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if(score != 0.0) {
          if(topScores(index) eq null) {
            topScores(index) = new Array[Double](parser.grammar.index.size)
          }
          topScores(index)(rule) += score
        }
      }
      

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if(score != 0.0) {
          if(scores(index) eq null) {
            scores(index) = new Array[Double](parser.grammar.labelIndex.size)
          }
          scores(index)(tag) += score
        }
      }
    }

    // TODO: gold tags!
    val labelThresholds = scores.map { case arr =>
      if(arr eq null) null
      else BitSet(0 until arr.length filter {s => math.log(arr(s)) > threshold}:_*)
    }

    val unaryThresholds = topScores.map { arr =>
      if(arr eq null) null
      else BitSet(0 until arr.length filter {s => math.log(arr(s)) > threshold}:_*)
    }

    (labelThresholds, unaryThresholds)
  }

  def buildSpanScorer(charts: ChartMarginal[ParseChart, L, W],
                      goldTags: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):ConstraintScorer[L] = {
    import charts._

    val (label,unary) = scoresForCharts(charts, goldTags)

    new ConstraintScorer[L](label, unary)
  }

}


case class ProjectionParams(treebank: ProcessedTreebank,
                            parser: File, out: File = new File("spans"), maxParseLength: Int = 40, project: Boolean = true) {
}

object ProjectTreebankToConstraints {

  import SpanBroker._

  def main(args: Array[String]) {
    val (baseConfig, files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProjectionParams]("")
    val treebank = params.treebank.copy(maxLength = 1000000)
    println(params)
    val parser = loadParser[Any](params.parser)

    val outDir = params.out
    outDir.mkdirs()

    val factory = new ConstraintScorerFactory[AnnotatedLabel, String](parser.builder, -7)
    scalanlp.util.writeObject(new File(outDir, XBAR_GRAMMAR_NAME), parser.builder.grammar.grammar)
    serializeSpans(mapTrees(factory, treebank.trainTrees, parser.builder.grammar.labelIndex, true, params.maxParseLength), new File(outDir, TRAIN_SPANS_NAME))
    serializeSpans(mapTrees(factory, treebank.testTrees, parser.builder.grammar.labelIndex, false, 10000), new File(outDir, TEST_SPANS_NAME))
    serializeSpans(mapTrees(factory, treebank.devTrees, parser.builder.grammar.labelIndex, false, 10000), new File(outDir, DEV_SPANS_NAME))
  }

  def loadParser[T](loc: File) = {
    val parser = scalanlp.util.readObject[SimpleChartParser[AnnotatedLabel, String]](loc)
    parser
  }

  def mapTrees[L](factory: ConstraintScorerFactory[L, String], 
                  trees: IndexedSeq[TreeInstance[L, String]],
                  index: Index[L],
                  useTree: Boolean, maxL: Int) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[L, String]) =>
      val TreeInstance(id, tree, words) = ti
      println(id, words)
      try {
        val pruner:GoldTagPolicy[L] = if(useTree) {
          val mappedTree = tree.map(l => index(l))
          GoldTagPolicy.goldTreeForcing(mappedTree)
        } else {
          GoldTagPolicy.noGoldTags
        }
        val scorer = factory.mkSpanScorer(words, pruner)
        id -> scorer
      } catch {
        case e: Exception => e.printStackTrace(); id -> SpanScorer.identity[L]
      }
    }.seq
  }


}