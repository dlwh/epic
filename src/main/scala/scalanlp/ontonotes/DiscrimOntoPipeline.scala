package scalanlp.ontonotes

import scalanlp.parser.ParseEval.Statistics
import scalanlp.parser.Grammar._
import scalala.library.Library
import scalanlp.parser._

import discrim.{KMDiscrimObjective, DiscrimObjective, FeaturizerFactory, PlainFeaturizerFactory}
import projections.{ProjectingSpanScorer, GrammarProjections}
import scalanlp.optimize.FirstOrderMinimizer._
import scalanlp.parser.ParserParams
import java.io.File
import scalanlp.parser.TreeInstance
import scalanlp.parser.Parser
import scalanlp.parser.GenerativeParser
import scalala.tensor.::
import scalala.library.Library
import scalanlp.parser.SimpleLexicon
import scalanlp.parser.CKYChartBuilder
import scalanlp.parser.ParseChart._
import scalanlp.parser.ParseChart
import scalala.library.Library._
import scalanlp.util.logging.ConfiguredLogging
import scalanlp.optimize.RandomizedGradientCheckingFunction
import scalanlp.optimize.CachedBatchDiffFunction

object DiscrimOntoPipeline extends BasicOntoPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[OntoLabel],
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[AnnotatedLabel,String] = new PlainFeaturizerFactory[AnnotatedLabel],
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    pipeline: KMPipeline,
                    oldWeights: File = null)



  def trainParser(trainTrees2: IndexedSeq[TreeInstance[OntoLabel, String]], validate: (Parser[OntoLabel, String]) => Statistics, params: Params) = {
    val trainTrees = for( ti <- trainTrees2) yield ti.copy(ti.id, ti.tree.map(stripAnnotations _))
    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,OntoLabel,String](OntoLabel("TOP"),lexicon,grammar,ParseChart.logProb);
    }

    import params._;
    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree.map(_.tag),ti.words)
      TreeInstance(ti.id,t,ti.words)
    }.seq
    val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val builder = CKYChartBuilder(AnnotatedLabel("TOP"),lexicon,grammar).withCharts(ParseChart.logProb)
    val proj = GrammarProjections(xbarParser.grammar,grammar,{(a:AnnotatedLabel) => OntoLabel(a.label)})

    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(words, binary, unary);

    val openTags = Set.empty ++ {
      for(t <- words.nonzero.keys.iterator.map(_._1) if words(t, ::).size > 50) yield t;
    }


    val newTrees = (trainTrees zip transformed).toArray.par.map { case (ti,newTree) =>
      val scorer = new ProjectingSpanScorer(proj,ti.spanScorer)
      TreeInstance(ti.id,newTree.tree,newTree.words,scorer)
    }.seq

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }

    val obj = new KMDiscrimObjective[AnnotatedLabel,String](featurizer, newTrees,  builder, openTags, closedWords) with ConfiguredLogging;

    val init = obj.initialWeightVector;
    val rand = new RandomizedGradientCheckingFunction(obj, 0.1);

    for( (state,iter) <- params.opt.iterations(new CachedBatchDiffFunction(obj),init).take(maxIterations).zipWithIndex;
         if iter != 0 && iter % iterationsPerEval == 0) yield {
      val parser = obj.extractParser(state.x);
      val decoder = new MaxConstituentDecoder[OntoLabel,AnnotatedLabel,String](proj)
      val newParser = new SimpleChartParser[OntoLabel,AnnotatedLabel,String](parser.builder,decoder, proj)
       ("Onto-"+iter, newParser);
    }

  }
}