package scalanlp.parser
package discrim

import scalanlp.parser.ParseChart._
import scalanlp.parser.projections.GrammarProjections
import collection.mutable.ArrayBuffer
import scalanlp.parser._
import projections.GrammarProjections._
import scalala.tensor.{Counter, Counter2}
import scalala.tensor.::
import java.io.File
import scalanlp.trees.UnaryChainRemover.ChainReplacer
import scalanlp.util._
import logging.ConfiguredLogging
import scalala.library.Library
import Library.{sum,norm}
import scalala.tensor.dense.DenseVector
import scalanlp.optimize.{BatchDiffFunction, FirstOrderMinimizer, CachedBatchDiffFunction}

/**
 * 
 * @author dlwh
 */

object ExactParserExtractor {

  type Label[L,L2] = (L,Seq[L2])

  def extractParser[L,L2,W](parsers: Seq[ChartBuilder[LogProbabilityParseChart,L2,W]],
                            coarseParser: ChartBuilder[LogProbabilityParseChart,L,W],
                            projections: Seq[GrammarProjections[L,L2]]):SimpleChartParser[L,(L,Seq[L2]),W] = {

    type MyLabel = Label[L,L2]

    def project(l: MyLabel) = l._1
    def refine(l: L):Seq[MyLabel] = {
      val options = IndexedSeq[IndexedSeq[L2]](Vector.empty)
      val r = projections.foldLeft(options){ (options,p) =>
        for(l2 <- p.labels.refinementsOf(l); o <- options) yield {
          val r: IndexedSeq[L2] = o :+ l2
          r
        }
      }
      r.map(l ->  _)
    }

    val myProjections = GrammarProjections(coarseParser.grammar, refine _,  project _)
    val grammars = parsers.map(_.grammar)

    val brules = Counter2[MyLabel,BinaryRule[(L,Seq[L2])], Double]()
    val urules = Counter2[MyLabel,UnaryRule[(L,Seq[L2])], Double]()
    for(r <- myProjections.rules.fineIndex) r match {
      case br@BinaryRule(a,b,c) =>
        val scores = for {
          i <- 0 until grammars.length;
          aa = a._2 apply i
          bb = b._2 apply i
          cc = c._2 apply i
          g = grammars(i)
          score = g.ruleScore(BinaryRule(aa,bb,cc))
        } yield score

        brules(a,br) = scores.sum
      case ur@UnaryRule(a,b) =>
        val scores = for {
          i <- 0 until grammars.length;
          aa = a._2 apply i
          bb = b._2 apply i
          g = grammars(i)
        } yield g.ruleScore(UnaryRule(aa,bb))
        urules(a,ur) = scores.sum

    }

    val grammar = Grammar(myProjections.labels.fineIndex, myProjections.rules.fineIndex, brules, urules)

    val lexicons = parsers.map(_.lexicon)

    val _knownTagWords = collection.mutable.Set[(MyLabel,W)]()
    val knownTags = coarseParser.lexicon.knownTagWords.map(_._1).flatMap(myProjections.labels.refinementsOf _).toSet
    val knownWords = coarseParser.lexicon.knownTagWords.map(_._2).toSet
    for( w <- knownWords; ll1 <- lexicons(0).tagScores(w).keysIterator; ref <- myProjections.labels.refinementsOf(projections(0).labels.project(ll1))) {
      _knownTagWords += (ref->w)
    }

    def scoreWord(label: (L,Seq[L2]), w: W):Double = {
      var score = 0.0
      for( (lex,l) <- lexicons zip label._2) {
        val s = lex.wordScore(l,w)
        if(s == Double.NegativeInfinity)
          return s
        score += s
      }
      score
    }

    val lexicon = new Lexicon[MyLabel,W] {

      def wordScore(label: MyLabel, w: W):Double = {
        scoreWord(label,w)
      }


      override def tagScores(w: W) = {
        val scores = lexicons.map(_.tagScores(w));
        val res = Counter[MyLabel,Double]()
        for( ll1 <- scores(0).keysIterator; ref <- myProjections.labels.refinementsOf(projections(0).labels.project(ll1))) {
          val allScores = for (
            (sc,label) <- scores zip ref._2
          ) yield sc(label)
          res(ref) = allScores.sum
        }
        res
      }

      def tags = knownTags.iterator

      def knownTagWords = _knownTagWords.iterator
    }

    val root = myProjections.labels.refinementsOf(coarseParser.root)(0)
    val builder = new CKYChartBuilder[ParseChart.LogProbabilityParseChart,MyLabel,W](root, lexicon, grammar, ParseChart.logProb)
    new SimpleChartParser(builder, new MaxConstituentDecoder[L,MyLabel,W](myProjections), myProjections)

  }


}

object ExactPipeline extends ParserPipeline {
  type Params = LatentParams[SpecificParams]
  case class SpecificParams(numParsers: Int = 2)

  protected lazy val paramManifest = implicitly[Manifest[Params]]
  type MyLabel = (String,Seq[L2])
  type L2 = (String,Int)

  def split(l: String, numStates: Int, numParsers: Int): IndexedSeq[MyLabel] = if(l == "") IndexedSeq((l,IndexedSeq.fill(numParsers)(l -> 0))) else {
    val options = IndexedSeq[IndexedSeq[L2]](Vector.empty)
    val r = (0 until numParsers).foldLeft(options){ (options,p) =>
      for(l2 <- 0 until numStates; o <- options) yield {
        val r: IndexedSeq[L2] = o :+ (l->l2)
        r
      }
    }
    r.map(l ->  _)
  }

  def unsplit(l: MyLabel) = l._1

  def getFeaturizer(params: Params,
                    initLexicon: Counter2[String, String, Double],
                    initBinaries: Counter2[String, BinaryRule[String], Double],
                    initUnaries: Counter2[String, UnaryRule[String], Double],
                    numStates: Int) = {
    val factory = params.featurizerFactory;
    val featurizer = factory.getFeaturizer(initLexicon, initBinaries, initUnaries);
    val latentFactory = params.latentFactory;
    val latentFeaturizer = latentFactory.getFeaturizer(featurizer, numStates);
    val weightsPath = params.oldWeights

    if(weightsPath == null) {
      new ProductFeaturizer[String,L2,String](IndexedSeq.fill(params.specific.numParsers)(latentFeaturizer))
    } else {
      println("Using awesome weights...")
      val weightSeq = readObject[Array[(DenseVector[Double],Counter[Feature[(String,Int),String],Double])]](weightsPath).map(_._2)
      val splitFactor = params.splitFactor
      def identity(x: Feature[(String,Int),String]) = x
      val proj: Feature[(String,Int),String]=>Feature[(String,Int),String] = FeatureProjectors.split[String,String](_,splitFactor)

      val feats = Array.tabulate(params.specific.numParsers){ m =>
        if(m < weightSeq.length)
          new CachedWeightsFeaturizer(latentFeaturizer, weightSeq(m), proj, randomize=false)
        else latentFeaturizer
      }
      new ProductFeaturizer[String,L2,String](feats)
    }

  }

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees);
    import params._;
    val numParsers = params.specific.numParsers;
    println("NumStates: " + params.numStates);
    println("NumParsers: " + params.specific.numParsers);

    val xbarParser = parser.optParser.getOrElse {
      val grammar = Grammar(Library.logAndNormalizeRows(initBinaries),Library.logAndNormalizeRows(initUnaries));
      val lexicon = new SimpleLexicon(initLexicon);
      new CKYChartBuilder[LogProbabilityParseChart,String,String]("",lexicon,grammar,ParseChart.logProb);
    }


    val indexedProjections = GrammarProjections(xbarParser.grammar, split(_:String,numStates, numParsers), unsplit);

    val latentFeaturizer = getFeaturizer(params, initLexicon, initBinaries, initUnaries, numStates)

    val openTags = Set.empty ++ {
      for(t <- initLexicon.nonzero.keys.map(_._1) if initLexicon(t,::).size > 50; t2 <- split(t, numStates, numParsers).iterator ) yield t2;
    }

    val closedWords = Set.empty ++ {
      val wordCounts = sum(initLexicon)
      wordCounts.nonzero.pairs.iterator.filter(_._2 > 5).map(_._1);
    }

    val obj = new LatentDiscrimObjective[String,MyLabel,String](latentFeaturizer,
      trainTrees,
      indexedProjections,
      xbarParser,
      openTags,
      closedWords) with ConfiguredLogging;


    val init = obj.initialWeightVector + 0.0;

    import scalanlp.optimize.RandomizedGradientCheckingFunction;
    type OptState = FirstOrderMinimizer[DenseVector[Double],BatchDiffFunction[DenseVector[Double]]]#State
    def evalAndCache(pair: (OptState,Int) ) {
      val (state,iter) = pair;
      val weights = state.x;
      if(iter % iterPerValidate == 0) {
        cacheWeights(params, obj,weights, iter);
        val parser = obj.extractParser(weights);
        val results = validate(parser)
        println("Validation : " + results)
      }
    }


    val cachedObj = new CachedBatchDiffFunction[DenseVector[Double]](obj);

    for( (state,iter) <- params.opt.iterations(cachedObj,init).take(maxIterations).zipWithIndex.tee(evalAndCache _);
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      val parser = obj.extractParser(state.x)
      ("LatentDiscrim-" + iter.toString,parser)
    } catch {
      case e => println(e);e.printStackTrace(); throw e;
    }
  }

  def cacheWeights(params: Params, obj: LatentDiscrimObjective[String,MyLabel,String], weights: DenseVector[Double], iter: Int) = {
    val name = if(iter % 20 == 0) {
      new File("weights-a.ser")
    } else {
      new File("weights-b.ser")
    }
    writeObject( name, weights -> obj.indexedFeatures.decode(weights));
  }
}

object ExactRunner extends ParserPipeline {

  case class Params(parser: ParserParams.BaseParser[String],
                    model0: File = null,
                    model1: File = null,
                    model2: File = null,
                    model3: File = null)
  protected val paramManifest = manifest[Params]

  def trainParser(trainTrees: IndexedSeq[TreeInstance[String,String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {
    val parsers = new ArrayBuffer[SimpleChartParser[String,(String,Int),String]]
    var found = true
    var i = 0
    val paths = params.productIterator.buffered
    while(found && paths.hasNext) {
      found = false
      while(paths.hasNext && !paths.head.isInstanceOf[File]) paths.next
      if(paths.hasNext) {
        val path = paths.next.asInstanceOf[File]
        println(path)
        if(path ne null) {
          parsers += readObject(path)
          found = true
        }
        i += 1
      }
    }
    val coarseParser = params.parser.optParser

    val productParser = ExactParserExtractor.extractParser(parsers.map(_.builder.withCharts(ParseChart.logProb)), coarseParser.get, parsers.map(_.projections))
    Iterator.single( "Exact" -> productParser)
  }


}

object SplitExact extends ParserPipeline {

  case class Params(parser: ParserParams.BaseParser[String],
                    featurizerFactory: FeaturizerFactory[String,String] = new PlainFeaturizerFactory[String],
                    weightsPath: File, numStates: Int)

  protected val paramManifest = implicitly[Manifest[Params]]


  def split(x: String, numStates: Int) = {
    if(x.isEmpty) Seq((x,0))
    else for(i <- 0 until numStates) yield (x,i)
  }

  def unsplit(x: (String,Int)) = x._1


  def trainParser(trainTrees: IndexedSeq[TreeInstance[String, String]],
                  validate: Parser[String,String]=>ParseEval.Statistics,
                  params: Params) = {

    import params._

    val features = scalanlp.util.readObject[(Any,Counter[Feature[(String,Seq[(String,Int)]),String],Double])](weightsPath)._2
    val featuresByIndex = Counter2[Int,Feature[(String,Int),String],Double]()
    for( (IndexFeature(SubstateFeature(f,states),i),v) <- features.pairsIterator) {
      featuresByIndex(i,SubstateFeature(f,ArrayBuffer(states:_*))) = v
    }

    val (initLexicon,initBinaries,initUnaries) = GenerativeParser.extractCounts(trainTrees)

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
    val parsers = {for( i <- featuresByIndex.domain._1.iterator) yield {
      val init = obj.indexedFeatures.encodeDense(featuresByIndex(i,::))
      println(i)
      println("Norm: " + norm(init,2))
      println("Norm counter: " + norm(featuresByIndex(i,::),2))
      println("Number zeros: " + init.toArray.count(_ == 0.0))
      println("Number zeros counter: " + featuresByIndex(i,::).valuesIterator.count(_ == 0.0))
      val parser = obj.extractParser(init)
      ("Split-" + i) -> parser

    }}.toIndexedSeq

    val projections = IndexedSeq.fill(parsers.length)(indexedProjections)
    val models = parsers.map(_._2).map(EPModel.fromChartParser(_)).map(_.builder)
    val ep = new EPParser(models, xbarParser, 4)
    val adf = new EPParser(models, xbarParser, 1)
    val product = new ProductParser(parsers.map(_._2.builder.withCharts(ParseChart.logProb)), xbarParser, projections)
    val ep8 = new EPParser(models, xbarParser, 8)
    val exact = ExactParserExtractor.extractParser(parsers.map(_._2.builder.withCharts(ParseChart.logProb)), xbarParser, projections)

    parsers.iterator ++ Iterator("EP"-> ep, "ADF" -> adf, "Product" -> product, "f0" -> ep.f0parser, "Exact" -> exact)


  }
}