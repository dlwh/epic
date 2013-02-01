package epic.redux

import java.io.File
import epic.trees._
import epic.parser.{Lexicon, GenerativeParser, ParserParams}
import epic.parser.ParserParams.XbarGrammar
import breeze.config.{Help, CommandLineParser}
import epic.ontonotes.{NERType, ConllOntoReader}
import breeze.linalg._
import epic.framework._
import epic.sequences.{Gazetteer, SegmentationModelFactory, SemiCRFModel, SemiCRF}
import breeze.optimize.{BatchDiffFunction, FirstOrderMinimizer, RandomizedGradientCheckingFunction, CachedBatchDiffFunction}
import breeze.util.{Lens, Index, Encoder}
import epic.parser.projections.ConstraintCoreGrammar
import epic.everything.models._
import epic.trees.annotations.StripAnnotations
import epic.trees.annotations.AddMarkovization
import epic.trees.annotations.PipelineAnnotator
import epic.parser.models.{IndexedLexFeaturizer, SimpleWordShapeGen}
import epic.parser.features.TagAwareWordShapeFeaturizer
import collection.immutable
import epic.trees.ProcessedTreebank
import epic.parser.features.RuleFeature
import scala.Some
import epic.parser.models.StandardFeaturizer
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.ontonotes.Document
import epic.everything.models.SpanBeliefs
import epic.parser.models.LexGrammarBundle
import epic.redux.FeaturizedDocument.Factory

/**
 * 
 * @author dlwh
 */
object AnnotatingPipeline {
  case class Params(corpus: File,
                    treebank: ProcessedTreebank,
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    constraints: ParserParams.Constraints[AnnotatedLabel, String],
                    baseParser: ParserParams.XbarGrammar,
                    weightsCache: File = new File("everything.weights.gz"),
                    trainBaseModels: Boolean = true,
                    @Help(text="For optimizing the base models")
                    baseOpt: OptParams,
                    @Help(text="For optimizing the joint model")
                    opt: OptParams)

  def main(args: Array[String]) {

    val params = CommandLineParser.readIn[Params](args)
    require(params.corpus.exists(), params.corpus + " does not exist!")

    val corpus = params.corpus
    val nfiles = params.nfiles
    val (train, test) = readTrainTestSplit(corpus, nfiles)

    val weightsCache = if (params.weightsCache.exists()) {
      loadWeights(params.weightsCache)
    } else {
      Counter[String, Double]()
    }

    val (docProcessor, processedTrain) = buildProcessor(train, weightsCache, params)
    val processedTest = test.par.map(docProcessor(_)).seq.flatMap(_.sentences)

    val beliefsFactory = new SentenceBeliefs.Factory(docProcessor.grammar, docProcessor.nerLabelIndex, Index[String]())

    // now build the individual models
    val nerModel = makeNERModel(beliefsFactory, docProcessor, processedTrain, weightsCache)
    val lexModel = makeLexParserModel(beliefsFactory, docProcessor, processedTrain, weightsCache)

    // initial models
    if(params.trainBaseModels) {
      println("Training base models")
      for(m <- IndexedSeq(nerModel, lexModel)) {
        println("Training " + m.getClass.getName)
        val obj = new ModelObjective(m, processedTrain.flatMap(_.sentences))
        val cachedObj = new CachedBatchDiffFunction(obj)
        val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-2, toString = {
          (i: Int) => m.featureIndex.get(i).toString
        })
        val weights = params.baseOpt.minimize(cachedObj, obj.initialWeightVector(randomize = false))
        updateWeights(params.weightsCache, weightsCache, Encoder.fromIndex(m.featureIndex).decode(weights))
        println(s"Decoding $m...")
        println(s"Evaluation result for $m: " + m.evaluate(processedTrain.flatMap(_.sentences), weights))
      }
    }

    // propagation

    // lenses
    val nerLens: Lens[SpanBeliefs,Beliefs[NERType.Value]] = Lens({_.ner}, {(a,b) => a.copy(ner=b)})
    val symLens: Lens[SpanBeliefs,Beliefs[Option[AnnotatedLabel]]] = Lens({_.label}, {(a,b) => a.copy(label=b)})

    val assocSynNer = PropertyPropagation.simpleModel(beliefsFactory,
      beliefsFactory.nerProp, nerLens,
      beliefsFactory.optionLabelProp, symLens)

    // the big model!
    val epModel = new EPModel[FeaturizedSentence, SentenceBeliefs](5, epInGold = true, initFeatureValue = {f => Some(weightsCache(f.toString)).filter(_ != 0.0)})(
      lexModel,
      nerModel,
      assocSynNer
    )

    val obj = new ModelObjective(epModel, processedTrain.flatMap(_.sentences))
    val cachedObj = new CachedBatchDiffFunction(obj)

    val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-2, toString = {
      (i: Int) => epModel.featureIndex.get(i).toString
    })

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    val opt = params.opt
    for( s:OptState <- opt.iterations(cachedObj, obj.initialWeightVector(randomize = false))) {
      updateWeights(params.weightsCache, weightsCache, Encoder.fromIndex(epModel.featureIndex).decode(s.x))
      //      val (unregularized, deriv) = obj.calculate(s.x)
      //      bump(unregularized, deriv, s, 1000)
      //      bump(unregularized, deriv, s, 12000)
      //      bump(unregularized, deriv, s, featureICareAbout)
      //      bump(unregularized, deriv, s, featureICareAbout + 1)

      if( s.iter % 5 == 0) {
        val inf = epModel.inferenceFromWeights(s.x)
        val results: IndexedSeq[immutable.IndexedSeq[EvaluationResult[_]]] = {for (d <- processedTest.par) yield {
          val epMarg = inf.marginal(d)
          for ( i <- 0 until epMarg.marginals.length) yield {
            val casted =  inf.inferences(i).asInstanceOf[AnnotatingInference[FeaturizedSentence]]
            val newDoc = casted.annotate(d, epMarg.marginals(i).asInstanceOf[casted.Marginal])
            epModel.models(i).asInstanceOf[EvaluableModel[FeaturizedSentence]].evaluate(newDoc, d)
          }}
        }.seq

        val hacketyHack = results.toIndexedSeq.transpose.map(_.reduce{ (a: Object, b: Object) =>
          val aa = a.asInstanceOf[{def +(other: EvaluationResult[_]):EvaluationResult[_]}]
          (aa + b.asInstanceOf[EvaluationResult[_]]).asInstanceOf[Object]
        })

        println(hacketyHack)
      }

    }

  }


  def readTrainTestSplit(corpus: File, nfiles: Int): (Array[Document], Array[Document]) = {

    val instances = for {
      file <- corpus.listFiles take nfiles
      doc <- ConllOntoReader.readDocuments(file)
    } yield doc
    val train = instances.take(instances.length * 9 / 10)
    val test = instances.drop(instances.length * 9 / 10)
    (train, test)

  }


  def buildProcessor(train: Array[Document], weightsCache: Counter[String, Double], params: AnnotatingPipeline.Params): (Factory, IndexedSeq[FeaturizedDocument]) = {
    val nerSegments = for (d <- train; s <- d.sentences) yield s.nerSegmentation
    println("Building basic NER model...")
    val baseNER = {
      val model: SemiCRFModel[NERType.Value, String] = new SegmentationModelFactory(NERType.OutsideSentence, gazetteer = Gazetteer.ner("en"), weights = {
        (f: Feature) => weightsCache(f.toString)
      }).makeModel(nerSegments)

      val obj = new ModelObjective(model, nerSegments)
      val cached = new CachedBatchDiffFunction(obj)

      val weights = params.opt.minimize(cached, obj.initialWeightVector(randomize = false))
      val crf = model.extractCRF(weights)
      val decoded: Counter[Feature, Double] = Encoder.fromIndex(model.featureIndex).decode(weights)
      updateWeights(params.weightsCache, weightsCache, decoded)
      crf
    }

    val nerPruningModel = new SemiCRF.ConstraintGrammar(baseNER)

    var trainTrees = for (d <- train; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, removeUnaries = true)
    }

    if(params.treebank.path.exists) {
      trainTrees ++= params.treebank.trainTrees
    }

    val annotator = new PipelineAnnotator[AnnotatedLabel, String](Seq(StripAnnotations(), AddMarkovization(horizontal = 1, vertical = 2)))
    val baseParser = GenerativeParser.annotated(new XbarGrammar(), annotator, trainTrees)

    FeaturizedDocument.makeFactory(params.treebank.process,
      new ConstraintCoreGrammar(baseParser.augmentedGrammar, -8),
      nerPruningModel, GenerativeParser.extractCounts(trainTrees)._1, null)(train)
  }

  private def makeNERModel(beliefsFactory: SentenceBeliefs.Factory, processor: FeaturizedDocument.Factory, docs: IndexedSeq[FeaturizedDocument], weightsCache: Counter[String, Double]) = {
    new ChainNER.ModelFactory(beliefsFactory, processor, weights={(f: Feature)=>Some(weightsCache(f.toString)).filter(_ != 0)}).makeModel(docs.flatMap(_.sentences))
  }

  def makeLexParserModel(beliefsFactory: SentenceBeliefs.Factory,
                         docProcessor: FeaturizedDocument.Factory,
                         train: IndexedSeq[FeaturizedDocument],
                         weightsCache: Counter[String, Double]): SentLexParser.Model = {
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
      100,
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
    breeze.util.writeObject(out, weightsCache.activeIterator.toIndexedSeq)
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
