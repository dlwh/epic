package scalanlp.parser
package discrim

import scalala.tensor.dense.DenseVector
import scalanlp.trees._
import scalanlp.optimize._

import InsideOutside._
import projections._
import scalanlp.trees.UnaryChainRemover.ChainReplacer;

import ParseChart.LogProbabilityParseChart;

import scalanlp.util._;
import logging._
import java.io._
import logging.ConfiguredLogging
import scalala.library.Library
import Library.sum
import scalala.tensor.{Counter,::}
import projections.GrammarProjections
import collection.immutable.Set
import splitting.StateSplitting

import scalanlp.optimize.FirstOrderMinimizer._;
object KMLatentPipeline extends ParserPipeline {

  protected val paramManifest = manifest[Params];
  case class Params(parser: ParserParams.BaseParser[String],
                    numStates: Int,
                    opt: OptParams,
                    featurizerFactory: FeaturizerFactory[AnnotatedLabel,String] = new PlainFeaturizerFactory[AnnotatedLabel](),
                    latentFactory: LatentFeaturizerFactory = new SlavLatentFeaturizerFactory(),
                    iterationsPerEval: Int = 50,
                    maxIterations: Int = 201,
                    iterPerValidate: Int = 10,
                    oldWeights: File = null,
                    splitFactor:Int = 1);


    def split(x: AnnotatedLabel, numStates: Int) = {
    if(x.label.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i);
  }

  def unsplit(x: (AnnotatedLabel,Int)) = x._1;

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params): Iterator[(String, SimpleChartParser[String, (AnnotatedLabel,Int), String])] = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);

    val xbarParser = params.parser.optParser getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }

    import params._;

    val factory = params.featurizerFactory;
    val latentFactory = params.latentFactory;

    val pipeline = new KMPipeline()

    val transformed = trainTrees.par.map { ti =>
      val t = pipeline(ti.tree,ti.words)
      TreeInstance(ti.id,t,ti.words)
    }.seq
    val (words,binary,unary) = GenerativeParser.extractCounts(transformed);
    val grammar = Grammar(Library.logAndNormalizeRows(binary),Library.logAndNormalizeRows(unary));
    println(grammar.labelIndex)
    val lexicon = new SignatureLexicon(words, EnglishWordClassGenerator, 5);
    val builder = CKYChartBuilder(AnnotatedLabel(""),lexicon,grammar).withCharts(ParseChart.logProb)
    val proj = GrammarProjections(xbarParser.grammar,grammar,{(_:AnnotatedLabel).label})

    val featurizer = factory.getFeaturizer(words, binary, unary);
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);

    val newTrees = (trainTrees zip transformed).toArray.par.map { case (ti,newTree) =>
      val scorer = new ProjectingSpanScorer(proj,ti.spanScorer)
      TreeInstance(ti.id,newTree.tree,newTree.words,scorer)
    }.seq

    val proj2: GrammarProjections[AnnotatedLabel, (AnnotatedLabel, Int)] = GrammarProjections(grammar, split(_:AnnotatedLabel,numStates), unsplit)

    val openTags: Set[(AnnotatedLabel,Int)] = Set.empty ++ {
      for(t <- words.nonzero.keys.iterator.map(_._1) if words(t, ::).size > 50;
          t2 <- split(t,numStates)) yield t2;
    }

    val closedWords:Set[String] = Set.empty ++ {
      val wordCounts = sum(words)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 10).map(_._1);
    }




    val obj = new LatentDiscrimObjective[AnnotatedLabel,(AnnotatedLabel,Int),String](latentFeaturizer, newTrees, proj2, builder, openTags, closedWords) with ConfiguredLogging;

    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter);
        println("Validating...");
        val parser: SimpleChartParser[AnnotatedLabel, (AnnotatedLabel, Int), String] = obj.extractParser(weights);
        val newProj: GrammarProjections[String, (AnnotatedLabel,Int)] = proj compose proj2
        val decoder = new MaxConstituentDecoder[String,(AnnotatedLabel,Int),String](newProj)
        val newParser = new SimpleChartParser[String,(AnnotatedLabel,Int),String](parser.builder,decoder,newProj)
        println(validate(newParser))
      }
    }

    val init = obj.initialWeightVector;

    for( (state,iter) <- params.opt.iterations(obj,init).take(maxIterations).zipWithIndex.tee(evalAndCache);
         if iter != 0 && iter % iterationsPerEval == 0) yield {
      val parser = obj.extractParser(state.x);
      val newProj: GrammarProjections[String, (AnnotatedLabel,Int)] = proj compose proj2
      val decoder = new MaxConstituentDecoder[String,(AnnotatedLabel,Int),String](newProj)
      val newParser = new SimpleChartParser[String,(AnnotatedLabel,Int),String](parser.builder,decoder,newProj)
        ("KM-disc"+iter, newParser);
    }

  }


  def cacheWeights(params: Params, obj: LatentDiscrimObjective[AnnotatedLabel,(AnnotatedLabel,Int),String], weights: DenseVector[Double], iter: Int) = {
    val name = if(iter % 20 == 0) {
      new File("weights-a.ser")
    } else {
      new File("weights-b.ser")
    }
    writeObject( name, weights -> obj.indexedFeatures.decode(weights));
  }
}
