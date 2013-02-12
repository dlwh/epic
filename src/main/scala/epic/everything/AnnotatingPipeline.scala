package epic.everything

import java.io.File
import epic.trees._
import epic.parser.{SimpleRefinedGrammar, Lexicon, GenerativeParser, ParserParams}
import epic.parser.ParserParams.XbarGrammar
import breeze.config.{Help, CommandLineParser}
import epic.ontonotes.{NERType, ConllOntoReader}
import breeze.linalg._
import epic.framework._
import epic.sequences.{Gazetteer, SegmentationModelFactory, SemiCRFModel, SemiCRF}
import breeze.optimize._
import breeze.util.{Lens, Index, Encoder}
import epic.parser.projections.ConstraintCoreGrammar
import epic.trees.annotations.StripAnnotations
import epic.trees.annotations.AddMarkovization
import epic.trees.annotations.PipelineAnnotator
import epic.parser.models.{IndexedLexFeaturizer, SimpleWordShapeGen}
import epic.parser.features.TagAwareWordShapeFeaturizer
import epic.trees.ProcessedTreebank
import epic.parser.features.RuleFeature
import scala.Some
import epic.parser.models.StandardFeaturizer
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.ontonotes.Document
import epic.parser.models.LexGrammarBundle
import com.google.common.io.Files
import collection.mutable.ArrayBuffer
import collection.immutable


/**
 * 
 * @author dlwh
 */
object AnnotatingPipeline {
  case class Params(corpus: File,
                    treebank: ProcessedTreebank,
                    cache: IntermediateCache,
                    nfiles: Int = 100000,
                    nthreads: Int = -1,
                    iterPerEval: Int = 20,
                    baseParser: ParserParams.XbarGrammar,
                    weightsCache: File = new File("everything.weights.gz"),
                    trainBaseModels: Boolean = true,
                    checkGradient: Boolean = false,
                    includeSRL: Boolean = false,
                    includeParsing: Boolean = true,
                    includeNER: Boolean = true,
                    lexHashFeatures: Double = 100.0,
                    @Help(text="For optimizing the base models")
                    baseOpt: OptParams,
                    @Help(text="For optimizing the joint model")
                    opt: OptParams)

  def main(args: Array[String]) {

    val params = CommandLineParser.readIn[Params](args)
    println(params)
    require(params.corpus.exists(), params.corpus + " does not exist!")


    val corpus = params.corpus
    val nfiles = params.nfiles
    val traintest = readTrainTestSplit(corpus, nfiles)
    val train = traintest._1
    val test = traintest._2

    val weightsCache = if (params.weightsCache.exists()) {
      loadWeights(params.weightsCache)
    } else {
      Counter[String, Double]()
    }

    val (docProcessor, processedTrain) = params.cache.cached("processorAndTrain", train){
      buildProcessor(train, weightsCache, params)
    }


//    println(docProcessor.parseConstrainer.augmentedGrammar.refined.asInstanceOf[SimpleRefinedGrammar[_,_,_]].refinements)
    val processedTest = params.cache.cached("test", test, docProcessor) {
      test.par.map(docProcessor(_)).seq.flatMap(_.sentences)
//      processedTrain.flatMap(_.sentences).take(10)
    }.toIndexedSeq

    println(s"${processedTrain.length} training documents totalling ${processedTrain.flatMap(_.sentences).length} sentences.")
    println(s"${processedTest.length} test sentences.")

    val beliefsFactory = new SentenceBeliefs.Factory(docProcessor.grammar, docProcessor.nerLabelIndex, docProcessor.srlLabelIndex, docProcessor.outsideSrlLabel)

    // now build the individual models
    type Datum = FeaturizedSentence
    type Augment = SentenceBeliefs
    type CompatibleModel = EvaluableModel[Datum] { type Inference <: ProjectableInference[Datum, Augment] with AnnotatingInference[Datum]}
    val models = ArrayBuffer[CompatibleModel]()
    if (params.includeParsing)
     models += makeLexParserModel(beliefsFactory, docProcessor, processedTrain, weightsCache, params.lexHashFeatures)
    if (params.includeNER)
       models += makeNERModel(beliefsFactory, docProcessor, processedTrain, weightsCache)
    if (params.includeSRL)
      models += makeSRLModel(beliefsFactory, docProcessor, processedTrain, weightsCache)


    if (params.trainBaseModels && params.checkGradient) {
       println("Checking gradients...")
      for(m <- models) {

        println("Checking " + m.getClass.getName)
        val obj = new ModelObjective(m, processedTrain.flatMap(_.sentences).filter(_.words.filter(_(0).isLetterOrDigit).length <= 40), params.nthreads)
        val cachedObj = new CachedBatchDiffFunction(obj)
              val w = obj.initialWeightVector(true)
      val (v, grad) = obj.calculate(w)
      for (i <- (m.featureIndex.size-1 to m.featureIndex.size-200) by -3) {
        w(i) += 1E-8
        val v2 = obj.valueAt(w)
        w(i) -= 1E-8
        val emp = (v2-v)/1E-8
        val rel = ((grad(i) - emp)/math.max(emp.abs, grad(i).abs).max(1E-6)).abs
        println(i + " " + m.featureIndex.get(i) + " " + grad(i) + " " + emp + " " + rel + " " + w(i))

      }
        GradientTester.test(cachedObj, obj.initialWeightVector(randomize = true), randFraction = 1E-4, toString={(x:Int) => m.featureIndex.get(x).toString})
      }
    }

    // initial models
    if(params.trainBaseModels) {
      println("Training base models")
      for(m <- models) {
        println("Training " + m.getClass.getName)
        val obj = new ModelObjective(m, processedTrain.flatMap(_.sentences).filter(_.words.filter(_(0).isLetterOrDigit).length <= 40), params.nthreads)
        val cachedObj = new CachedBatchDiffFunction(obj)
        val weights = params.baseOpt.minimize(cachedObj, obj.initialWeightVector(randomize = false))
        updateWeights(params.weightsCache, weightsCache, Encoder.fromIndex(m.featureIndex).decode(weights))
        println(s"Decoding $m...")
        println(s"Evaluation result for $m: " + m.evaluate(processedTest, weights))
      }
    }

    // propagation

    val allModels = ArrayBuffer[EPModel.CompatibleModel[FeaturizedSentence, SentenceBeliefs]](models:_*)

    if(params.includeNER && params.includeParsing) {
      allModels += PropertyModels.nerSyntaxModel(docProcessor, beliefsFactory)
    }

    if(params.includeSRL && params.includeParsing) {
      allModels += PropertyModels.srlSyntaxModel(docProcessor, beliefsFactory)
    }

    if(params.includeSRL && params.includeNER) {
      allModels += PropertyModels.srlNerModel(docProcessor, beliefsFactory)
    }






    // the big model!
    val epModel = new EPModel[FeaturizedSentence, SentenceBeliefs](4, epInGold = true, initFeatureValue = {f => Some(weightsCache(f.toString)).filter(_ != 0.0)})(
      allModels: _*
    )


    val obj = new ModelObjective(epModel, processedTrain.flatMap(_.sentences).filter(_.words.filter(_(0).isLetterOrDigit).length <= 40), params.nthreads)
    val cachedObj = new CachedBatchDiffFunction(obj)

    if (params.checkGradient) {
      println("Checking gradients...")
//      val w = obj.initialWeightVector(true)
//      val (v, grad) = obj.calculate(w)
//      for (i <- (epModel.featureIndex.size-1 to epModel.featureIndex.size-2000) by -10) {
//        w(i) += 1E-8
//        val v2 = obj.valueAt(w)
//        w(i) -= 1E-8
//        val emp = (v2-v)/1E-8
//        val rel = ((grad(i) - emp)/math.max(emp.abs, grad(i).abs).max(1E-6)).abs
//        println(i + " " + epModel.featureIndex.get(i) + " " + grad(i) + " " + emp + " " + rel)
//
//      }
      GradientTester.test(cachedObj, obj.initialWeightVector(randomize = true), randFraction = 1E-1, toString={(x:Int) => epModel.featureIndex.get(x).toString})
    }

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    val opt = params.opt
    for( s:OptState <- opt.iterations(cachedObj, obj.initialWeightVector(randomize = false))) {
      //      val (unregularized, deriv) = obj.calculate(s.x)
      //      bump(unregularized, deriv, s, 1000)
      //      bump(unregularized, deriv, s, 12000)
      //      bump(unregularized, deriv, s, featureICareAbout)
      //      bump(unregularized, deriv, s, featureICareAbout + 1)

      if( s.iter % params.iterPerEval == 0) {
        updateWeights(params.weightsCache, weightsCache, Encoder.fromIndex(epModel.featureIndex).decode(s.x))
        val inf = epModel.inferenceFromWeights(s.x)
        val results: immutable.Seq[immutable.IndexedSeq[EvaluationResult[_]]] = {for (d <- processedTest.par) yield {
          val epMarg = inf.marginal(d)
          for ( i <- 0 until epMarg.marginals.length) yield {
            epModel.models(i) match {
              case m: EvaluableModel[FeaturizedSentence] =>
                val casted =  inf.inferences(i).asInstanceOf[AnnotatingInference[FeaturizedSentence]]
                val newDoc = casted.annotate(d, epMarg.marginals(i).asInstanceOf[casted.Marginal])
                Some(m.evaluate(newDoc, d, true))
              case _ => None
            }
          }}.flatMap(_.iterator)
        }.seq

        val hacketyHack = results.toIndexedSeq.transpose.map(_.reduce{ (a: Object, b: Object) =>
          val aa = a.asInstanceOf[{def +(other: EvaluationResult[_]):EvaluationResult[_]}]
          (aa + b.asInstanceOf[EvaluationResult[_]]).asInstanceOf[Object]
        })

        println("Joint results:")
        for ( (result, model) <- hacketyHack zip epModel.models.filter(_.isInstanceOf[EvaluableModel[FeaturizedSentence]])) {
          println(model.getClass.getName + ": " + result )
        }
      }

    }

  }


  def readTrainTestSplit(corpus: File, nfiles: Int): (IndexedSeq[Document], IndexedSeq[Document]) = {

    val instances = for {
      file <- corpus.listFiles.sortBy(_.getName) take nfiles
      doc <- ConllOntoReader.readDocuments(file)
    } yield doc
    val train = instances.take(instances.length * 9 / 10)
    val test = instances.drop(instances.length * 9 / 10)
    (train.toIndexedSeq, test.toIndexedSeq)

  }


  def buildProcessor(train: IndexedSeq[Document], weightsCache: Counter[String, Double], params: AnnotatingPipeline.Params): (FeaturizedDocument.Factory, IndexedSeq[FeaturizedDocument]) = {
    val nerSegments = for (d <- train; s <- d.sentences) yield s.nerSegmentation
    val nerPruningModel : SemiCRF.ConstraintGrammar[NERType.Value, String] = if(params.includeNER) params.cache.cached("baseNER", nerSegments.toSet) {
      println("Building basic NER model...")
      val baseNER = {
        val model: SemiCRFModel[NERType.Value, String] = new SegmentationModelFactory(NERType.OutsideSentence, NERType.NotEntity, gazetteer = Gazetteer.ner("en"), weights = {
          (f: Feature) => weightsCache(f.toString)
        }).makeModel(nerSegments)

        val obj = new ModelObjective(model, nerSegments, params.nthreads)
        val cached = new CachedBatchDiffFunction(obj)

        val weights = params.opt.minimize(cached, obj.initialWeightVector(randomize = false))
        val crf = model.extractCRF(weights)
        val decoded: Counter[Feature, Double] = Encoder.fromIndex(model.featureIndex).decode(weights)
        updateWeights(params.weightsCache, weightsCache, decoded)
        crf
      }
      new SemiCRF.BaseModelConstraintGrammar(baseNER)
    } else new SemiCRF.IdentityConstraintGrammar(Index(NERType.values), NERType.OutsideSentence)


    println("Building basic parsing model...")
    var trainTrees = for (d <- train; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, removeUnaries = true)
    }

//    if(params.treebank.path.exists) {
//      trainTrees ++= params.treebank.trainTrees
//    }

    val baseParser = params.cache.cached("baseParser", trainTrees.toSet) {
      val annotator = new PipelineAnnotator[AnnotatedLabel, String](Seq(StripAnnotations()))
      GenerativeParser.annotated(new XbarGrammar(), annotator, trainTrees)
    }


    FeaturizedDocument.makeFactory(params.treebank.process,
      new ConstraintCoreGrammar(baseParser.augmentedGrammar, {(_:AnnotatedLabel).isIntermediate}, -8),
      nerPruningModel, GenerativeParser.extractCounts(trainTrees)._1, null)(train)
  }


  private def makeNERModel(beliefsFactory: SentenceBeliefs.Factory, processor: FeaturizedDocument.Factory, docs: IndexedSeq[FeaturizedDocument], weightsCache: Counter[String, Double]) = {
    new ChainNER.ModelFactory(beliefsFactory, processor, weights={(f: Feature)=>Some(weightsCache(f.toString)).filter(_ != 0)}).makeModel(docs.flatMap(_.sentences))
  }


  private def makeSRLModel(beliefsFactory: SentenceBeliefs.Factory, processor: FeaturizedDocument.Factory, docs: IndexedSeq[FeaturizedDocument], weightsCache: Counter[String, Double]) = {
    new SRL.ModelFactory(beliefsFactory, processor, weights={(f: Feature)=>Some(weightsCache(f.toString)).filter(_ != 0)}).makeModel(docs.flatMap(_.sentences))
  }

  def makeLexParserModel(beliefsFactory: SentenceBeliefs.Factory,
                         docProcessor: FeaturizedDocument.Factory,
                         train: IndexedSeq[FeaturizedDocument],
                         weightsCache: Counter[String, Double],
                         lexHashFeatures: Double): SentLexParser.Model = {
    val trainTrees = train.flatMap(_.sentences).map(_.treeInstance)
    val trees = trainTrees.map(StripAnnotations())
    val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

    val wordIndex: Index[String] = Index(trainTrees.iterator.flatMap(_.words))
    val summedCounts = sum(initLexicon, Axis._0)
    val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
    val tagShapeGen = new TagAwareWordShapeFeaturizer(initLexicon)

    val lexicon: Lexicon[AnnotatedLabel, String] = initLexicon

    def ruleGen(r: Rule[AnnotatedLabel]) = IndexedSeq(RuleFeature(r))
    def validTag(w: String) = lexicon.tagsForWord(w).toArray

    val headFinder = HeadFinder.collins
    val feat = new StandardFeaturizer(wordIndex,
    docProcessor.grammar.labelIndex,
    docProcessor.grammar.index,
    ruleGen,
    shapeGen, {
      (w: Seq[String], pos: Int) => tagShapeGen.featuresFor(w, pos)
    })

    val indexed = IndexedLexFeaturizer.extract[AnnotatedLabel, String](feat,
      headFinder,
      docProcessor.grammar.index,
      docProcessor.grammar.labelIndex,
//    2,
      lexHashFeatures,
      -1,
      trees)

    val bundle = new LexGrammarBundle[AnnotatedLabel, String](docProcessor.grammar,
      docProcessor.lexicon,
      headFinder,
      wordIndex
    )

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    new SentLexParser.Model(beliefsFactory, bundle, reannotate, indexed, {(f: Feature) => Some(weightsCache(f.toString))})
  }


  private def updateWeights(out: File, weightsCache: Counter[String, Double], newWeights: Counter[Feature, Double]) {
    for ( (f,w) <- newWeights.activeIterator) {
      weightsCache(f.toString) = w
    }
    if(out.exists()) {
      Files.copy(out, new File(out.toString +".backup"))
    }
    breeze.util.writeObject(out, weightsCache)
  }

  private def loadWeights(in: File) = {
    val ctr = Counter[String, Double]()
    breeze.util.readObject[AnyRef](in) match {
      case seq: IndexedSeq[(String, Double)] =>
        for ( (k, v) <- seq) {
          ctr(k) = v
        }
      case ctr2: Counter[String, Double] =>
        ctr += ctr2
    }
    ctr
  }
}
