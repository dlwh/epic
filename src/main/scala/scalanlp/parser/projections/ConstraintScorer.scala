package scalanlp.parser
package projections

import collection.immutable.BitSet
import scalanlp.config.Configuration
import java.io._
import scalanlp.collection.mutable.TriangularArray
import scalanlp.trees._
import scalanlp.util.Index

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1L)
class ConstraintScorer[L, W](val grammar: BaseGrammar[L],
                             val lexicon: Lexicon[L, W],
                             val words: Seq[W],
                             scores: Array[BitSet],
                             topScores: Array[BitSet]) extends CoreAnchoring[L, W] with Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    if (topScores eq null) 0.0
    else {
      val set = topScores(TriangularArray.index(begin, end))
      if(set == null || !set.contains(rule)) Double.NegativeInfinity
      else 0.0
    }
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    if(scores eq null) 0.0
    else {
      val set = scores(TriangularArray.index(begin, end))
      if(set == null || !set.contains(tag)) Double.NegativeInfinity
      else 0.0
    }
  }
}

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class ConstraintScorerCoreGrammar[L, W](augmentedGrammar: AugmentedGrammar[L, W], threshold: Double) extends CoreGrammar[L, W] {
  def grammar = augmentedGrammar.grammar
  def lexicon = augmentedGrammar.lexicon


  def specialize(words: Seq[W]) = {
    val charts = ChartMarginal(augmentedGrammar, words, ParseChart.logProb)
    val chartScorer = buildScorer(charts)
    chartScorer
  }

  def buildScorer(charts: Marginal[L, W],
                  goldTags: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):ConstraintScorer[L, W] = {

    val (label,unary) = scoresForCharts(charts, goldTags)

    new ConstraintScorer[L, W](charts.scorer.grammar, charts.scorer.lexicon, charts.scorer.words, label, unary)
  }

  def buildScorer(words: Seq[W],
                  goldTags: GoldTagPolicy[L]):ConstraintScorer[L, W] = {

    val charts = ChartMarginal(augmentedGrammar, words, ParseChart.logProb)
    buildScorer(charts, goldTags)
  }

  private def scoresForCharts(marg: Marginal[L, W], gold: GoldTagPolicy[L]) = {
    val length = marg.length
    val scores = TriangularArray.raw(length+1, null: Array[Double])
    val topScores = TriangularArray.raw(length+1, null: Array[Double])
    val visitor = new AnchoredVisitor[L] {
      def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, ref: Int, score: Double) {}

      def visitUnaryRule(begin: Int, end: Int, rule: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if(score != 0.0) {
          if(topScores(index) eq null) {
            topScores(index) = new Array[Double](grammar.index.size)
          }
          topScores(index)(rule) += score
        }
      }
      

      def visitSpan(begin: Int, end: Int, tag: Int, ref: Int, score: Double) {
        val index = TriangularArray.index(begin, end)
        if(score != 0.0) {
          if(scores(index) eq null) {
            scores(index) = new Array[Double](grammar.labelIndex.size)
          }
          scores(index)(tag) += score
        }
      }
    }

    marg.visit(visitor)

    val labelThresholds = new TriangularArray[BitSet](length+1, {(i, j) =>
      val arr = scores(TriangularArray.index(i, j))
      val thresholdedGoldTags = if(arr eq null) {
        BitSet.empty
      } else BitSet.empty ++ (0 until arr.length filter {s => math.log(arr(s)) > threshold})
      val result = thresholdedGoldTags ++ (0 until grammar.labelIndex.size).filter{gold.isGoldTag(i, j, _)}
      if(result.nonEmpty) result
      else null
    }).data

    val unaryThresholds = new TriangularArray[BitSet](length+1, {(i, j) =>
      val arr = topScores(TriangularArray.index(i, j))
      val thresholdedGoldTags = if(arr eq null) {
        BitSet.empty
      } else BitSet.empty ++ (0 until arr.length filter {s => math.log(arr(s)) > threshold})
      val result = thresholdedGoldTags ++ (0 until grammar.index.size).filter{r => gold.isGoldTag(i, j, grammar.parent(r))}
      if (result.nonEmpty) result
      else null
    }).data

    (labelThresholds, unaryThresholds)
  }



}


case class ProjectionParams(treebank: ProcessedTreebank,
                            parser: File, out: File = new File("constraints.ser.gz"), maxParseLength: Int = 80, project: Boolean = true) {
}

object ProjectTreebankToConstraints {

  def main(args: Array[String]) {
    val (baseConfig, files) = scalanlp.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProjectionParams]("")
    val treebank = params.treebank.copy(maxLength = 1000000)
    println(params)
    val parser = loadParser[Any](params.parser)

    val out = params.out
    out.getAbsoluteFile.getParentFile.mkdirs()

    val factory = new ConstraintScorerCoreGrammar[AnnotatedLabel, String](parser.augmentedGrammar, -7)
    val train = mapTrees(factory, treebank.trainTrees, parser.grammar.labelIndex, useTree = true, maxL = params.maxParseLength)
    val test = mapTrees(factory, treebank.testTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val dev = mapTrees(factory, treebank.devTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val map = Map.empty ++ train ++ test ++ dev
    scalanlp.util.writeObject(out, map)
  }

  def loadParser[T](loc: File) = {
    val parser = scalanlp.util.readObject[SimpleChartParser[AnnotatedLabel, String]](loc)
    parser
  }

  def mapTrees(factory: ConstraintScorerCoreGrammar[AnnotatedLabel, String],
               trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
               index: Index[AnnotatedLabel],
               useTree: Boolean, maxL: Int) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[AnnotatedLabel, String]) =>
      val TreeInstance(id, tree, words) = ti
      println(id, words)
      try {
        val policy = if(useTree) {
          GoldTagPolicy.goldTreeForcing[AnnotatedLabel](tree.map(_.baseAnnotatedLabel).map(index))
        } else {
          GoldTagPolicy.noGoldTags[AnnotatedLabel]
        }
        val scorer = factory.buildScorer(words, policy)
        words -> scorer
      } catch {
        case e: Exception => e.printStackTrace();
        words -> RefinedAnchoring.identity[AnnotatedLabel, String](factory.grammar, factory.lexicon, words)
      }
    }.seq
  }


}