package scalanlp.parser
package projections

import scalanlp.trees._
import scalanlp.collection.mutable.TriangularArray
import scalala.library.Numerics._
import scalanlp.util.Index
import scalanlp.io.FileIterable

import java.io._
import scalanlp.trees.{Trees,DenseTreebank}
import scalanlp.tensor.sparse.OldSparseVector
import scalala.library.Numerics
import scalanlp.config.Configuration

/**
 * Creates labeled span scorers for a set of trees from some parser.
 * @author dlwh
 */
class LabeledSpanScorerFactory[C,L,W](parser: ChartParser[C,L,W]) extends SpanScorer.Factory[C,L,W] {
  def indexedProjections = parser.projections.labels

  def mkSpanScorer(s: Seq[W], scorer: SpanScorer[C] = SpanScorer.identity, thresholdScorer: SpanScorer[C] = SpanScorer.constant(Double.NegativeInfinity)) = {
    val charts = parser.charts(s,scorer)

    val sentProb = charts.inside.top.labelScore(0,s.length,parser.root)
    if(sentProb.isInfinite) {
      sys.error("Couldn't parse " + s + " " + sentProb)
    }

    val chartScorer = buildSpanScorer(charts,sentProb, scorer, thresholdScorer)

    chartScorer
  }

  def buildSpanScorer(charts: ChartPair[ParseChart,L], sentProb: Double,
                      coarseScorer: SpanScorer[C] = SpanScorer.identity,
                      thresholdScorer: SpanScorer[C] = SpanScorer.constant(Double.NegativeInfinity)):LabeledSpanScorer[C] = {
    import charts._

    val scores = TriangularArray.raw(inside.length+1,null:OldSparseVector)
    for(begin <-  0 until inside.length; end <- begin+1 to (inside.length)) {
      val index = TriangularArray.index(begin, end)
      val scoresForLocation = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity)
      for(l <- inside.bot.enteredLabelIndexes(begin,end)) {
        val pL = indexedProjections.project(l)
        val myScore = inside.bot.labelScore(begin, end, l) + outside.bot.labelScore(begin, end, l) + scorer.scoreSpan(begin,end,l) - sentProb
        val currentScore = scoresForLocation(pL)
        scoresForLocation(pL) = Numerics.logSum(currentScore,myScore)
      }

      for( (c,v) <- scoresForLocation.nonzero.pairs) {
        if(v > thresholdScorer.scoreSpan(begin,end,c)) {
          if(scores(index) eq null) {
            scores(index) = indexedProjections.coarseEncoder.mkOldSparseVector(Double.NegativeInfinity)
          }
          scores(index)(c) = v
        }
      }

    }
    //println("Density: " + density * 1.0 / scores.length)
    //println("Label Density:" + labelDensity * 1.0 / scores.length / parser.grammar.index.size)
    new LabeledSpanScorer[C](scores)
  }

}

@SerialVersionUID(1)
class LabeledSpanScorer[L](scores: Array[OldSparseVector]) extends SpanScorer[L] with Serializable {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = 0.0

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = 0.0

  def scoreSpan(begin: Int, end: Int, label: Int): Double = {
    if(scores(TriangularArray.index(begin,end)) eq null) Double.NegativeInfinity
    else scores(TriangularArray.index(begin,end))(label)
  }
}

case class ProjectionParams(treebank: TreebankParams,
                            spans: SpanParams[String],
                            parser: File, out: File = new File("spans"), maxParseLength: Int = 40, project: Boolean = true) {
  def processedTreebank = ProcessedTreebank(treebank,spans)
}

object ProjectTreebankToLabeledSpans {
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
      val factory = new LabeledSpanScorerFactory[String,Any,String](parser)
      writeObject(parser.projections.labels.coarseIndex,new File(outDir,SPAN_INDEX_NAME))
      SpanBroker.serializeSpans(mapTrees(factory,treebank.trainTrees, proj, true, params.maxParseLength),new File(outDir,TRAIN_SPANS_NAME))
      SpanBroker.serializeSpans(mapTrees(factory,treebank.testTrees, proj, false, 10000),new File(outDir,TEST_SPANS_NAME))
      SpanBroker.serializeSpans(mapTrees(factory,treebank.devTrees, proj, false, 10000),new File(outDir,DEV_SPANS_NAME))
    } else {
      val fineProj = new GrammarProjections(ProjectionIndexer.simple(parser.projections.labels.fineIndex), ProjectionIndexer.simple(parser.projections.rules.fineIndex))
      val nonprojectingParser = new SimpleChartParser(parser.builder,new SimpleViterbiDecoder[Any,String](parser.builder.grammar), fineProj)
      val factory = new LabeledSpanScorerFactory[Any,Any,String](nonprojectingParser)
      writeObject(parser.projections.labels.fineIndex,new File(outDir,SPAN_INDEX_NAME))
      serializeSpans(mapTrees(factory,treebank.trainTrees, trueProj, true, params.maxParseLength),new File(outDir,TRAIN_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.testTrees, trueProj, false, 10000),new File(outDir,TEST_SPANS_NAME))
      serializeSpans(mapTrees(factory,treebank.devTrees, trueProj, false, 10000),new File(outDir,DEV_SPANS_NAME))

    }
  }

  def loadParser[T](loc: File) = {
    val oin = new ObjectInputStream(new BufferedInputStream(new FileInputStream(loc)))
    val parser = oin.readObject().asInstanceOf[SimpleChartParser[String,T,String]]
    oin.close()
    parser
  }

  def mapTrees[L](factory: LabeledSpanScorerFactory[L,Any,String], trees: IndexedSeq[TreeInstance[String,String]],
                  proj: GrammarProjections[String,L], useTree: Boolean, maxL: Int) = {
    // TODO: have ability to use other span scorers.
    trees.toIndexedSeq.par.map { (ti:TreeInstance[String,String]) =>
      val TreeInstance(id,tree,words,preScorer) = ti
      println(id,words)
      try {
        val pruningThreshold = -7.0
        val pruner:SpanScorer[L] = if(useTree) {
          val mappedTree = tree.map(l => proj.labels.refinementsOf(l).map(proj.labels.fineIndex))
          SpanScorer.sum(AnchoredRuleProjector.candidateTreeForcing(mappedTree), SpanScorer.constant(pruningThreshold))
        } else {
          SpanScorer.constant[L](pruningThreshold)
        }
        val scorer =factory.mkSpanScorer(words,new ProjectingSpanScorer(proj, preScorer), pruner)
        id -> scorer
      } catch {
        case e: Exception => e.printStackTrace(); id -> SpanScorer.identity[L]
      }
    }.seq
  }

  def writeObject(o: AnyRef, file: File) {
    val oout = new ObjectOutputStream(new BufferedOutputStream(new FileOutputStream(file)))
    oout.writeObject(o)
    oout.close()
  }



}
