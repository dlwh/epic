package epic.parser
package projections

import breeze.collection.mutable.TriangularArray
import breeze.config.Configuration
import breeze.util.Index
import collection.immutable.BitSet
import java.io._
import ConstraintAnchoring.RawConstraints
import epic.trees._

/**
 * 
 * @author dlwh
 */
@SerialVersionUID(1L)
class ConstraintAnchoring[L, W](val grammar: BaseGrammar[L],
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

object ConstraintAnchoring {
  @SerialVersionUID(1)
  case class RawConstraints(bottom: Array[BitSet], top: Array[BitSet]) {
    def toAnchoring[L, W](grammar: BaseGrammar[L], lexicon: Lexicon[L, W], words: Seq[W]) = {
      new ConstraintAnchoring(grammar, lexicon, words, bottom, top)
    }
  }
}

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class ConstraintCoreGrammar[L, W](augmentedGrammar: AugmentedGrammar[L, W], threshold: Double) extends CoreGrammar[L, W] {
  def grammar = augmentedGrammar.grammar
  def lexicon = augmentedGrammar.lexicon


  def anchor(words: Seq[W]) = {
    val charts = ChartMarginal(augmentedGrammar, words, ParseChart.logProb)
    val chartScorer = buildConstraints(charts)
    chartScorer
  }

  def buildConstraints(charts: Marginal[L, W],
                  goldTags: GoldTagPolicy[L] = GoldTagPolicy.noGoldTags[L]):ConstraintAnchoring[L, W] = {

    val RawConstraints(label,unary) = rawConstraints(charts, goldTags)

    new ConstraintAnchoring[L, W](charts.anchoring.grammar, charts.anchoring.lexicon, charts.anchoring.words, label, unary)
  }

  def buildConstraints(words: Seq[W],
                       goldTags: GoldTagPolicy[L]):ConstraintAnchoring[L, W] = {

    val charts = ChartMarginal(augmentedGrammar, words, ParseChart.logProb)
    buildConstraints(charts, goldTags)
  }

  def rawConstraints(words: Seq[W], gold: GoldTagPolicy[L]):RawConstraints = {
    val charts = ChartMarginal(augmentedGrammar, words, ParseChart.logProb)
    rawConstraints(charts, gold)
  }

  def rawConstraints(marg: Marginal[L, W], gold: GoldTagPolicy[L]): RawConstraints = {
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

    RawConstraints(labelThresholds, unaryThresholds)
  }



}


case class ProjectionParams(treebank: ProcessedTreebank,
                            parser: File, out: File = new File("constraints.ser.gz"), maxParseLength: Int = 80, project: Boolean = true) {
}

object ProjectTreebankToConstraints {

  def main(args: Array[String]) {
    val (baseConfig, files) = breeze.config.CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = config.readIn[ProjectionParams]("")
    val treebank = params.treebank.copy(maxLength = 1000000)
    println(params)
    val parser = loadParser[Any](params.parser)

    val out = params.out
    out.getAbsoluteFile.getParentFile.mkdirs()

    val factory = new ConstraintCoreGrammar[AnnotatedLabel, String](parser.augmentedGrammar, -7)
    val train = mapTrees(factory, treebank.trainTrees, parser.grammar.labelIndex, useTree = true, maxL = params.maxParseLength)
    val test = mapTrees(factory, treebank.testTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val dev = mapTrees(factory, treebank.devTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val map: Map[Seq[String], RawConstraints] = Map.empty ++ train ++ test ++ dev
    breeze.util.writeObject(out, map)
  }

  def loadParser[T](loc: File): SimpleChartParser[AnnotatedLabel, String] = {
    val parser = breeze.util.readObject[SimpleChartParser[AnnotatedLabel, String]](loc)
    parser
  }

  def mapTrees(factory: ConstraintCoreGrammar[AnnotatedLabel, String],
               trees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
               index: Index[AnnotatedLabel],
               useTree: Boolean, maxL: Int): Seq[(Seq[String], RawConstraints)] = {
    trees.toIndexedSeq.par.flatMap { (ti:TreeInstance[AnnotatedLabel, String]) =>
      val TreeInstance(id, tree, words) = ti
      println(id, words)
      try {
        val policy = if(useTree) {
          GoldTagPolicy.goldTreeForcing[AnnotatedLabel](tree.map(_.baseAnnotatedLabel).map(index))
        } else {
          GoldTagPolicy.noGoldTags[AnnotatedLabel]
        }
        val scorer = factory.rawConstraints(words, policy)
        Seq(words -> scorer)
      } catch {
        case e: Exception => e.printStackTrace()
        Seq.empty[(Seq[String],RawConstraints)]
      }
    }.seq
  }


}
