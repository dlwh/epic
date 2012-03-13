package scalanlp.parser
package combine

import scalala.tensor.mutable.Counter
import scalanlp.parser.features.Feature
import scalala.tensor.dense.DenseVector
import scalanlp.util.{Encoder, Index, TODO}
import epic.{ModelObjective, MarginalInference, Model}
import scalanlp.optimize.CachedBatchDiffFunction
import collection.immutable.{Map, IndexedSeq}
import scalanlp.trees.BinarizedTree
import actors.threadpool.AtomicInteger
import scalala.tensor.sparse.SparseVector
import scalala.library.Library
import scalanlp.tensor.sparse.OldSparseVector
import scalanlp.collection.mutable.TriangularArray
import projections.{ConstraintScorerFactory, LabeledSpanScorerFactory}

/**
 * 
 * @author dlwh
 */

class CombinerModel[L,W](val builder: CKYChartBuilder[ParseChart.LogProbabilityParseChart, L, W],
                         val factory: CombinerFeaturizerFactory[L,W]) extends Model[TreeBundle[L,W]] {
  type ExpectedCounts = CombinerECounts
  type Inference = CombinerParserInference[L,W]

  val featureIndex = factory.featureIndex
  def numFeatures = featureIndex.size

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

    val parser = new Parser[L,W] with Serializable {
      def bestParse(s: Seq[W], spanScorer: SpanScorer[L]) = {
        val scorer = SpanScorer.sum[L](sentToScorer(s),spanScorer)
        val inside = builder.buildInsideChart(s, scorer)
        val outside = builder.buildOutsideChart(inside, scorer)
        decoder.extractBestParse(builder.root, builder.grammar, inside, outside, s, scorer)
      }
//      val decoder = new SimpleViterbiDecoder[String,String](basicParser.grammar)
      val decoder = MaxConstituentDecoder.simple[L,W](builder.grammar)
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

  val scorerFactory = new ConstraintScorerFactory[L,L, W](SimpleChartParser(builder),-14)

  def makeFilter(tb: TreeBundle[L, W]):SpanScorer[L] = {
    val data = LabeledSpanExtractor.extractSpans(builder.grammar.labelIndex, tb.outputs.values)
    val init = new SpanScorer[L] {
      def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
        0.0
      }

      def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
        0.0
      }

      def scoreSpan(begin: Int, end: Int, tag: Int) = {
        val arr = data(TriangularArray.index(begin,end))
        if( (arr eq null) || arr(tag) <= 0) 0.0
        else arr(tag) * 3
      }
    }

    val goldTreePolicy = if(forceGoldTree && tb.goldTree != null) {
      GoldTagPolicy.goldTreeForcing[L](tb.goldTree.map(builder.grammar.labelIndex))
    } else {
      GoldTagPolicy.noGoldTags[L]
    }
    scorerFactory.mkSpanScorer(tb.words, init, goldTreePolicy)
//    val inside = builder.buildInsideChart(tb.words, init)
//    val outside = builder.buildOutsideChart(outside, init)
//    val scorer = new
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


  def marginal(v: TreeBundle[L, W], aug: CombinerSpanScorer[L, W]) = {
    val inside = builder.buildInsideChart(v.words,aug)
    val outside = builder.buildOutsideChart(inside,aug)
    val root_score = inside.top.labelScore(0,v.words.length,builder.root)
    println("Done parsing... " + v.id)
    new ChartPair[ParseChart.LogProbabilityParseChart,L](inside,outside,root_score,aug) -> root_score
  }


  def guessCountsFromMarginals(v: TreeBundle[L, W], marg: CombinerParserInference[L, W]#Marginal, aug: CombinerSpanScorer[L, W]) = {
    val root_score = marg.inside.top.labelScore(0,v.words.length,builder.root)
    val visitor = new CombinerFeaturizerECVisitor(aug.feat, DenseVector.zeros[Double](weights.size))
    new InsideOutside(builder).expectedCounts(v.words, marg.inside, marg.outside, root_score, marg.scorer, visitor)
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
      val (lexicon,grammar) = GenerativeParser.extractLexiconAndGrammar(allTrees)
      // erase rule counts
      //      val zeroParser = new CKYChartBuilder("ROOT", lexicon, grammar, ParseChart.viterbi)
      val zeroParser = new CKYChartBuilder("ROOT", new ZeroLexicon(lexicon), Grammar.zero(grammar), ParseChart.logProb)
      zeroParser
    }

//    println("Extracting features... will take some time...")

    val allSystems = trainTrees.iterator.flatMap(_.outputs.keysIterator).toSet
    val featurizerFactory = new StandardCombinerFeaturizerFactory(allSystems, basicParser.grammar)

    /*
    val processed = new AtomicInteger(0)
    val featureIndex = {
      val allFeatures = trainTrees.par.aggregate(Set.empty[Feature])({ (features, tb) =>
        val myFeats = collection.mutable.Set[Feature]()
        val featurizer = featurizerFactory.featurizerFor(tb)
        val extractiveSpanScorer = new SpanScorer[String] {
          def scoreBinaryRule(begin: Int, split: Int, end: Int, rule: Int) = {
            myFeats ++= featurizer.featuresForBinary(begin, split, end, rule).keys
            0.0
          }

          def scoreUnaryRule(begin: Int, end: Int, rule: Int) = {
            myFeats ++= featurizer.featuresForUnary(begin, end, rule).keys
            0.0
          }

          def scoreSpan(begin: Int, end: Int, tag: Int) = {
            myFeats ++= featurizer.featuresForSpan(begin, end, tag).keys
            0.0
          }
        }

        basicParser.buildInsideChart(tb.words, extractiveSpanScorer)
        val total = processed.incrementAndGet()
        println(total)
        if(total % 100 == 0) {
          println(total+ "/"+trainTrees.length)
        }
        features ++ myFeats
      }, (_ ++ _))

      Index[Feature](allFeatures)
    }*/

    val model = new CombinerModel(basicParser, featurizerFactory)
    val obj = new ModelObjective(model, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val init = obj.initialWeightVector
    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(params.opt.maxIterations).zipWithIndex
         if iter != 0 && iter % params.iterationsPerEval == 0) yield try {
      val parser = model.extractParser(state.x, testTrees)
      ("Combine-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e
    }

  }
}

