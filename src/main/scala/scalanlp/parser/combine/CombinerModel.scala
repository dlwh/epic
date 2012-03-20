package scalanlp.parser
package combine

import scalala.tensor.mutable.Counter
import scalanlp.parser.features.Feature
import scalala.tensor.dense.DenseVector
import scalanlp.util.{Encoder, Index, TODO}
import epic.{ModelObjective, MarginalInference, Model}
import collection.immutable.{Map, IndexedSeq}
import scalanlp.trees.BinarizedTree
import actors.threadpool.AtomicInteger
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.collection.mutable.TriangularArray
import projections.{ConstraintScorerFactory, LabeledSpanScorerFactory}
import java.util.concurrent.ConcurrentHashMap
import collection.JavaConversions
import java.io.{PrintStream, BufferedOutputStream, FileOutputStream}
import scalanlp.optimize.{RandomizedGradientCheckingFunction, CachedBatchDiffFunction}

/**
 * 
 * @author dlwh
 */

class CombinerModel[L,W](val builder: CKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                         val factory: CombinerFeaturizerFactory[L,W],
                         initFeatValue: Feature=>Option[Double]) extends Model[TreeBundle[L,W]] {
  type ExpectedCounts = CombinerECounts
  type Inference = CombinerParserInference[L,W]

  val featureIndex = factory.featureIndex

  def emptyCounts = CombinerECounts(0.0, DenseVector.zeros(numFeatures))

  def expectedCountsToObjective(ecounts: CombinerECounts) = {
    ecounts.loss -> ecounts.counts.asCol
  }

  def inferenceFromWeights(weights: DenseVector[Double]) = {
    new CombinerParserInference(builder, factory, weights, true)
  }

  def extractParser(weights: DenseVector[Double], testTBs: IndexedSeq[TreeBundle[L,W]]) = {
    CombinerModel.extractParser(this, weights, testTBs)
  }

  def initialValueForFeature(f: Feature) = initFeatValue(f).getOrElse(0.0)
}

object CombinerModel {
  def extractParser[L,W](model: CombinerModel[L,W], weights: DenseVector[Double], testTBs: IndexedSeq[TreeBundle[L,W]]) ={
    val sentToDataMap: Map[Seq[W], Map[String, BinarizedTree[L]]] = testTBs.iterator.map(tb => tb.words -> (Map.empty ++ tb.outputs)).toMap
    val builder = model.builder
    val factory = model.factory
    val inf =  new CombinerParserInference(builder, factory, weights, false)

    def sentToScorer(s: Seq[W]): CombinerSpanScorer[L, W] = {
      val outputs = sentToDataMap(s)
      val tb = new TreeBundle("?", null, outputs, s)
      inf.baseAugment(tb)
    }

    val zeroBuilder = inf.zeroBuilder

    val parser = new Parser[L,W] with Serializable {
      def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
        val scorer = SpanScorer.sum[L](sentToScorer(s),spanScorer)
        val inside = zeroBuilder.buildInsideChart(s, scorer)
        val outside = zeroBuilder.buildOutsideChart(inside, scorer)
        val best = decoder.extractBestParse(zeroBuilder.root, zeroBuilder.grammar, inside, outside, s, scorer)
        val exScore = inf.goldCounts(new TreeBundle("?",best,sentToDataMap(s),s)).loss
        val canScore = inf.goldCounts(new TreeBundle("?",sentToDataMap(s).values.head,sentToDataMap(s),s)).loss
        println(exScore + " " + canScore + " " + (best == sentToDataMap(s).values.head))
        best
      }
//      val decoder = new SimpleViterbiDecoder[L,W](zeroBuilder.grammar)
//      val decoder = MaxConstituentDecoder.simple[L,W](builder.grammar)
      val decoder = MaxRuleProductDecoder.simple(zeroBuilder)

    }
    parser
  }
}

case class CombinerECounts(var loss: Double, counts: DenseVector[Double]) extends epic.ExpectedCounts[CombinerECounts] {
  def +=(other: CombinerECounts) = {
    loss += other.loss
    counts += other.counts
    this
  }

  def -=(other: CombinerECounts) = {
    loss -= other.loss
    counts -= other.counts
    this
  }

}


object CombinerCache {
  // I feel dirty, but meh.
  val mapCache = new JavaConversions.JConcurrentMapWrapper(new ConcurrentHashMap[Seq[Any],SpanScorer[_]])
}

class CombinerParserInference[L,W](builder: CKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                                   factory: CombinerFeaturizerFactory[L,W],
                                   weights: DenseVector[Double],
                                   forceGoldTree: Boolean) extends MarginalInference[TreeBundle[L,W], CombinerSpanScorer[L, W]] {
  type Marginal = ChartPair[ParseChart.LogProbabilityParseChart, L]
  type ExpectedCounts = CombinerECounts

  def baseAugment(v: TreeBundle[L, W]) = {
    val filter = makeFilter(v)
    new CombinerSpanScorer(filter, factory.featurizerFor(v), weights)
  }

  private val scorerFactory = new ConstraintScorerFactory[L,L, W](SimpleChartParser(builder),-5)

  private def makeFilter(tb: TreeBundle[L, W]):SpanScorer[L] =  {
    CombinerCache.mapCache.get(tb.words) match {
      case Some(scorer) => scorer.asInstanceOf[SpanScorer[L]]
      case None =>
        val goldTreePolicy =
          GoldTagPolicy.goldTreeForcing[L](tb.treeInstances(withGold=forceGoldTree && tb.goldTree != null).map(ti => ti.tree.map(builder.grammar.labelIndex)).toSeq:_*)
        val scorer = scorerFactory.mkSpanScorer(tb.words, SpanScorer.identity, goldTreePolicy)
        CombinerCache.mapCache.putIfAbsent(tb.words, scorer)
        scorer
    }
  }

  def goldCounts(value: TreeBundle[L, W], augment: CombinerSpanScorer[L, W]) = {
    val visitor = new CombinerFeaturizerECVisitor(augment.feat, DenseVector.zeros[Double](weights.size))
    val score = AnchoredSpanVisitor.visitBinarizedTree(builder.grammar.labelIndex,
      builder.grammar.index,
      value.goldTree,
      visitor,
      augment)
    CombinerECounts(score, visitor.featureCounts)
  }
  
  val zeroBuilder = new CKYChartBuilder(builder.root, new ZeroLexicon(builder.lexicon), Grammar.zero(builder.grammar), ParseChart.logProb)

  def marginal(v: TreeBundle[L, W], aug: CombinerSpanScorer[L, W]) = {
    val inside = zeroBuilder.buildInsideChart(v.words,aug)
    val outside = zeroBuilder.buildOutsideChart(inside,aug)
    val root_score = inside.top.labelScore(0,v.words.length,builder.root)
//    println("Done parsing... " + v.id)
    new ChartPair[ParseChart.LogProbabilityParseChart,L](inside,outside,root_score,aug) -> root_score
  }


  def guessCountsFromMarginals(v: TreeBundle[L, W], marg: CombinerParserInference[L, W]#Marginal, aug: CombinerSpanScorer[L, W]) = {
    val root_score = marg.inside.top.labelScore(0,v.words.length,builder.root)
    val visitor = new CombinerFeaturizerECVisitor(aug.feat, DenseVector.zeros[Double](weights.size))
    new InsideOutside(zeroBuilder).expectedCounts(v.words, marg.inside, marg.outside, root_score, marg.scorer, visitor)
    CombinerECounts(root_score, visitor.featureCounts)
  }


}

trait CombinerFeaturizerFactory[L,W] extends Serializable {
  val featureIndex: Index[Feature]
  def featurizerFor(tb: TreeBundle[L, W]):CombinerFeaturizer[L,W]
}

trait CombinerFeaturizer[L,W] extends Serializable {
  def featuresForBinary(begin: Int, split: Int, end: Int, rule: Int):OldSparseVector
  def featuresForUnary(begin: Int, end: Int, rule: Int):OldSparseVector
  def featuresForSpan(begin: Int, end: Int, label: Int):OldSparseVector
}

class CombinerSpanScorer[L,W](filter: SpanScorer[L], val feat: CombinerFeaturizer[L,W], weights: DenseVector[Double]) extends SpanScorer[L] {
  def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
    val fscore = filter.scoreBinaryRule(begin, split, end, rule)
    if(fscore.isInfinite) fscore
    else  feat.featuresForBinary(begin,split,end,rule) dot weights
  }

  def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
    val fscore = filter.scoreUnaryRule(begin, end, rule)
    if(fscore.isInfinite) fscore
    else feat.featuresForUnary(begin,end,rule) dot weights
  }

  def scoreSpan(begin: Int, end: Int, tag: Int) = {
    val fscore = filter.scoreSpan(begin, end, tag)
    if(fscore.isInfinite) fscore
    else  feat.featuresForSpan(begin,end,tag) dot weights
  }
}


class CombinerFeaturizerECVisitor[L,W](f: CombinerFeaturizer[L,W], val featureCounts: DenseVector[Double]) extends AnchoredSpanVisitor {
  def visitBinaryRule(begin: Int, split: Int, end: Int, rule: Int, score: Double) {
    if(score != 0.0)
      for( (k,v) <- f.featuresForBinary(begin, split, end, rule).activeIterator) {
        featureCounts(k) += v * score
      }
  }

  def visitUnaryRule(begin: Int, end: Int, rule: Int, score: Double) {
    if(score != 0.0)
      for( (k,v) <- f.featuresForUnary(begin, end, rule).activeIterator) {
        featureCounts(k) += v * score
      }
  }

  def visitSpan(begin: Int, end: Int, tag: Int, score: Double) {
    if(score != 0.0)
      for( (k,v) <- f.featuresForSpan(begin, end, tag).activeIterator) {
        featureCounts(k) += v * score
      }
  }
}

object DiscrimCombinePipeline extends CombinePipeline {
  def trainParser(trainTrees: IndexedSeq[TreeBundle[String, String]],
                  testTrees: IndexedSeq[TreeBundle[String, String]],
                  params: DiscrimCombinePipeline.Params) = {
    val basicParser = {
      val allTrees = testTrees.flatMap(_.treeInstances(withGold=false)).toArray ++ trainTrees.flatMap(_.treeInstances(withGold=true))
      val (words,binary,unary) = GenerativeParser.extractCounts(allTrees)
      val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
      val lexicon = new UnsmoothedLexicon(Library.logAndNormalizeRows(words))
      val parser = new CKYChartBuilder("ROOT", lexicon, grammar, ParseChart.logProb)
      // erase rule counts
//      val zeroParser = new CKYChartBuilder("ROOT", new ZeroLexicon(lexicon), Grammar.zero(grammar), ParseChart.logProb)
      parser
    }

    val allSystems = trainTrees.iterator.flatMap(_.outputs.keysIterator).toSet
    val featurizerFactory = new StandardCombinerFeaturizerFactory(allSystems, basicParser.grammar, params.useRuleFeatures, params.useLabelFeatures)

    val featureCounter = if(params.oldWeights ne null) {
      scalanlp.util.readObject[Counter[Feature,Double]](params.oldWeights)
    } else {
      Counter[Feature,Double]()
    }

    val model = new CombinerModel(basicParser, featurizerFactory, {featureCounter.get(_)})
    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val checking = new RandomizedGradientCheckingFunction(cachedObj,1.0)
    val init = obj.initialWeightVector
//    Iterator("Combine-x" -> model.extractParser(init * 0.0 + 20.0, testTrees))
    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(params.opt.maxIterations).zipWithIndex
         if iter != 0 && iter % params.iterationsPerEval == 0) yield try {
      val features:Counter[Feature,Double] = Encoder.fromIndex(featurizerFactory.featureIndex).decode(state.x)
      val out = new PrintStream(new BufferedOutputStream(new FileOutputStream("combine-" + iter +".feats.txt")))
      for( (f,v) <- features.pairsIterator.toSeq.sortBy(-_._2.abs)) {
        out.println(f + "\t" + v)
      }
      out.close()
      val parser = model.extractParser(state.x, testTrees)
      ("Combine-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }

  }
}

