package scalanlp.parser.discrim

import scalanlp.config._
import scalanlp.util._
import scalala.tensor.dense.DenseVector
import scalala.tensor.Counter
import java.io.File
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.parser.Grammar._
import scalala.library.Library
import scalanlp.parser.ParseChart._
import scalanlp.parser._
import projections.GrammarProjections
import projections.GrammarProjections._
import scalala.library.Library._
import scalanlp.optimize.CachedBatchDiffFunction
import scalala.tensor.::
import collection.mutable.ArrayBuffer

/**
 * 
 * @author dlwh
 */


case class FilterWeightsParams(parser: ParserParams.BaseParser,
                               featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                               weightsPath: File)
object FilterWeights extends ParserTrainer {
  protected val paramManifest = implicitly[Manifest[FilterWeightsParams]]
  type Params = FilterWeightsParams

  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i)
  }

  def unsplit(x: (String,Int)) = x._1

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  devTrees: IndexedSeq[TreeInstance[String, String]],
                  unaryReplacer: ChainReplacer[String], params: FilterWeights.Params) = {

    import params._
    val weights = readObject[(DenseVector[Double],Counter[Feature[(String,Int),String],Double])](weightsPath)._2

    val result = Counter[Feature[(String,Int),String],Double]()
    val result2 = Counter[Feature[(String,Int),String], Double]()
    for( feat <-  weights.keysIterator) feat match {
      case TaggedFeature(s@SubstateFeature(f,states), 'High) =>
        result(SubstateFeature(f,ArrayBuffer(states:_*))) = weights(feat)
      case TaggedFeature(s@SubstateFeature(f,states), 'Low) =>
       result2(SubstateFeature(f,ArrayBuffer(states:_*))) = weights(feat)
    }

      val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees)
    val numStates = 2

    val xbarParser: ChartBuilder[ParseChart.LogProbabilityParseChart, String, String] = params.parser.optParser.getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries))
      val lexicon = new SimpleLexicon(initLexicon)
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb)
    }


    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates), unsplit)

    val featurizer = featurizerFactory.getFeaturizer(initLexicon, initBinaries, initUnaries)
    val latentFactory = new SlavLatentFeaturizerFactory
    val baseFeaturizer = latentFactory.getFeaturizer(featurizer, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates).iterator ) yield t2
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1)
    }

    val obj = new LatentDiscrimObjective(baseFeaturizer, trainTrees, indexedProjections, xbarParser, openTags, closedWords)
    println(obj.indexedFeatures.index)
    val init = obj.indexedFeatures.encodeDense(result)
    println(norm(init,2))
    val parser = obj.extractParser(init)
    val init2 = obj.indexedFeatures.encodeDense(result2)
    println(norm(init2,3))
    val parser2 = obj.extractParser(init2)

    val ep = new EPParser(Seq(parser,parser2).map(_.builder.withCharts(ParseChart.logProb)), xbarParser, Seq(indexedProjections, indexedProjections), 4)
    val adf = new EPParser(Seq(parser,parser2).map(_.builder.withCharts(ParseChart.logProb)), xbarParser, Seq(indexedProjections, indexedProjections), 1)
    val product = new ProductParser(Seq(parser,parser2).map(_.builder.withCharts(ParseChart.logProb)), xbarParser, Seq(indexedProjections, indexedProjections))
    val ep8 = new EPParser(Seq(parser,parser2).map(_.builder.withCharts(ParseChart.logProb)), xbarParser, Seq(indexedProjections, indexedProjections), 8)

    Iterator("LowOrder" -> parser, "HighOrder" -> parser2, "EP"-> ep, "ADF" -> adf, "Product" -> product, "EP-8" -> ep8)
  }




}
