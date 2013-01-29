package epic.everything

import java.io.File
import breeze.config.CommandLineParser
import epic.ontonotes.{NERType, ConllOntoReader}
import epic.trees._
import annotations.AddMarkovization
import annotations.AddMarkovization
import annotations.PipelineAnnotator
import annotations.PipelineAnnotator
import annotations.StripAnnotations
import annotations.StripAnnotations
import epic.parser._
import epic.parser.projections.{ConstraintAnchoring, ConstraintCoreGrammar}
import breeze.util.logging.ConsoleLogging
import collection.immutable.BitSet
import epic.sequences._
import collection.mutable.ArrayBuffer
import epic.framework.{EvaluationResult, Feature, EPModel, ModelObjective}
import breeze.collection.mutable.TriangularArray
import breeze.optimize._
import epic.parser.ParserParams.XbarGrammar
import features.RuleFeature
import features.RuleFeature
import features.{RuleFeature, TagAwareWordShapeFeaturizer}
import models._
import breeze.util.{Encoder, Lens, Index}
import epic.parser.models._
import breeze.linalg._
import epic.trees.annotations.StripAnnotations
import epic.trees.annotations.AddMarkovization
import epic.trees.annotations.PipelineAnnotator
import models.SpanBeliefs
import projections.ConstraintAnchoring.RawConstraints
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import epic.parser.models.StandardFeaturizer
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.parser.models.LexGrammarBundle
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import scala.Some
import epic.parser.models.StandardFeaturizer
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.parser.models.LexGrammarBundle
import collection.immutable


object EverythingPipeline {
  case class Params(path: File,
                    treebank: ProcessedTreebank,
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    constraints: ParserParams.Constraints[AnnotatedLabel, String],
                    baseParser: ParserParams.XbarGrammar,
                    baseNERModel: File = new File("ner.model.gz"),
                    weightsCache: File = new File("everything.weights.gz"),
                    opt: OptParams)

  def main(args: Array[String]) {

    val params = CommandLineParser.readIn[Params](args)

    require(params.path.exists(), params.path + " does not exist!")

    val (train, test) = {
      val instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
      } yield doc
      val train = instances.take(instances.length * 9 / 10)
      val test = instances.drop(instances.length * 9 / 10)
      (train,test)
    }

    val weightsCache = if (params.weightsCache.exists()) {
      loadWeights(params.weightsCache)
    } else {
      Counter[String, Double]()
    }



    /////////////////
    // base models///
    /////////////////
    // NER


    val nerSegments = for(d <- train; s <- d.sentences) yield s.nerSegmentation
    val baseNER = if(params.baseNERModel.exists) {
      breeze.util.readObject[SemiCRF[NERType.Value, String]](params.baseNERModel)
    } else {
      println("Building basic NER model...")
      val model: SemiCRFModel[NERType.Value, String] = new SegmentationModelFactory(NERType.OutsideSentence, gazetteer = Gazetteer.ner("en"), weights={(f:Feature) => weightsCache(f.toString)}).makeModel(nerSegments)

      val obj = new ModelObjective(model, nerSegments)
      val cached = new CachedBatchDiffFunction(obj)

      val weights = params.opt.minimize(cached, obj.initialWeightVector(randomize = false))
      val crf = model.extractCRF(weights)
      val decoded: Counter[Feature, Double] = Encoder.fromIndex(model.featureIndex).decode(weights)
      updateWeights(params.weightsCache, weightsCache, decoded)
//      if (params.baseNERModel.getAbsoluteFile.getParentFile.exists()) {
//        breeze.util.writeObject(params.baseNERModel, crf)
//      }
      crf
    }

    val nerPruningModel = new SemiCRF.ConstraintGrammar(baseNER)
    val nerModel = new SegmentationModelFactory(NERType.OutsideSentence, Some(nerPruningModel), Gazetteer.ner("en"), {(f: Feature) => weightsCache(f.toString)}).makeModel(nerSegments)
    // NERProperties
    val nerProp = Property("NER::Type", nerModel.labelIndex)


    var trainTrees = for (d <- train; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, removeUnaries = true)
    }

    if(params.treebank.path.exists) {
      trainTrees ++= params.treebank.trainTrees
    }

    val annotator = new PipelineAnnotator[AnnotatedLabel, String](Seq(StripAnnotations(), AddMarkovization()))
    val baseParser = GenerativeParser.annotated(new XbarGrammar(), annotator, trainTrees)

    println(baseParser.lexicon.tagsForWord("in").toIndexedSeq)
    println(baseParser.lexicon.tagsForWord("follow").toIndexedSeq)

    val docProcessor = new ProcessedDocument.Factory(params.treebank.process,
      new ConstraintCoreGrammar(baseParser.augmentedGrammar, -10),
      nerPruningModel,
      null)

    val beliefsFactory = new DocumentBeliefs.Factory(baseParser.grammar, nerProp)
    /*
    // Coref
    val corefNerProp = nerProp.copy(name = "Coref::NER") // own copy of corefNer, since we're using soft agreement.
    val corefExtractors = Properties.allExtractors :+ Properties.alwaysUnobservedExtractor(corefNerProp)
    val corefProperties = corefExtractors.map(_.property)
    val mentionDetector = CorefInstance.goldMentions
    val corefInstanceFactory = new CorefInstance.Factory(mentionDetector)
    val corefFeaturizer:CorefInstanceFeaturizer = CorefInstanceFeaturizer.fromTrainingSet(new SimplePairwiseFeaturizer, corefExtractors, corefInstanceFactory)(train)._1
      corefFeaturizer)
    val corefModel = new PropCorefModel(corefFeaturizer.propertyFeatures, corefFeaturizer.featureIndex)
    */


    val processedTrain = train.map(docProcessor(_, keepGoldTree=true))

    val lexParseModel = extractLexParserModel(trainTrees, baseParser, beliefsFactory, weightsCache)
    // train the lex parse model
    println("Training base lex parse model...")
    val lexWeights = {
      val obj = new ModelObjective(lexParseModel, processedTrain)
      val cached = new CachedBatchDiffFunction(obj)
      val weights = params.opt.minimize(cached, obj.initialWeightVector(randomize = false))
//      if (params.baseNERModel.getAbsoluteFile.getParentFile.exists()) {
//        breeze.util.writeObject(params.baseNERModel, crf)
//      }
      updateWeights(params.weightsCache, weightsCache, Encoder.fromIndex(lexParseModel.featureIndex).decode(weights))
      weights
    }


    // propagation

    // lenses
    val nerLens: Lens[SpanBeliefs,Beliefs[NERType.Value]] = Lens({_.ner}, {(a,b) => a.copy(ner=b)})
    val symLens: Lens[SpanBeliefs,Beliefs[Option[AnnotatedLabel]]] = Lens({_.label}, {(a,b) => a.copy(label=b)})

    val assocSynNer = PropertyPropagation.simpleModel(beliefsFactory,
      nerProp, nerLens,
      beliefsFactory.optionLabelProp, symLens)

    // joint-aware models
    val adaptedNerModel = new ChainNERModel(beliefsFactory, nerModel)

//    val propBuilder = new PropertyPropagator.Builder(spanIndex, wordIndex)
//    propBuilder.spans.associate(nerProp, corefNerProp, agreement = true)
//
//    val propModel = new PropertyPropagatingModel(propBuilder)

    // the big model!
    val epModel = new EPModel[ProcessedDocument, DocumentBeliefs](30, epInGold = true, initFeatureValue = {f => Some(weightsCache(f.toString)).filter(_ != 0.0)})(
      lexParseModel,
      adaptedNerModel,
      assocSynNer
    )
//    corefModel)
    //propModel)
//    val epModel = lexParseModel

    val obj = new ModelObjective(epModel, processedTrain)
    val cachedObj = new CachedBatchDiffFunction(obj)


    val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-2, toString = {
      (i: Int) => epModel.featureIndex.get(i).toString
    })

    val featureICareAbout = epModel.featureIndex.iterator.toIndexedSeq.indexWhere(_.toString == "ComponentFeature(2,AssociationFeature(NotEntity,Some(JJ)))")
    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State
    def bump(unreg: Double, gradx: DenseVector[Double], s: OptState, featureICareAbout: Int) = {
      val grad = gradx(featureICareAbout)
      val weights = s.x
      weights(featureICareAbout) += 1E-5
      val bump = obj.valueAt(weights)
      val bumpedGrad = (bump - unreg)/1E-5
      weights(featureICareAbout) -= 1E-5
      println(epModel.featureIndex.get(featureICareAbout) + s" grad:$grad bumped:$bumpedGrad rel: ${(grad - bumpedGrad)/math.max(grad, bumpedGrad + 1E-5)}" )
    }


    val myTest = test.take(20).map(docProcessor)

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
        val results: Array[immutable.IndexedSeq[DocumentAnnotatingModel#EvaluationResult]] = for (d <- myTest) yield {
          val epMarg = inf.marginal(d)
          for ( i <- 0 until epMarg.marginals.length) yield {
            val casted =  inf.inferences(i).asInstanceOf[DocumentAnnotatingInference]
            val newDoc = casted.annotate(d, epMarg.marginals(i).asInstanceOf[casted.Marginal])
            epModel.models(i).asInstanceOf[DocumentAnnotatingModel].evaluate(newDoc, d)
          }
        }

        val hacketyHack = results.toIndexedSeq.transpose.map(_.reduce{ (a: Object, b: Object) =>
          val aa = a.asInstanceOf[{def +(other: EvaluationResult[_]):EvaluationResult[_]}]
          (aa + b.asInstanceOf[EvaluationResult[_]]).asInstanceOf[Object]
        })

        println(hacketyHack)
      }

    }

  }

  def extractLexParserModel(trainTrees: Array[TreeInstance[AnnotatedLabel, String]],
                            parser: SimpleChartParser[AnnotatedLabel, String],
                            beliefsFactory: DocumentBeliefs.Factory,
                            weightsCache: Counter[String, Double]): DocLexParser.Model = {


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
    parser.grammar.labelIndex,
    parser.grammar.index,
    ruleGen,
    shapeGen, {
      (w: Seq[String], pos: Int) => tagShapeGen.featuresFor(w, pos)
    })

    val indexed = IndexedLexFeaturizer.extract[AnnotatedLabel, String](feat,
      headFinder,
      parser.grammar.index,
      parser.grammar.labelIndex,
      1, // TODO was 100
      -1,
      trees)

    val bundle = new LexGrammarBundle[AnnotatedLabel, String](parser.grammar,
      parser.lexicon,
      headFinder,
      wordIndex
    )

    def reannotate(tree: BinarizedTree[AnnotatedLabel], words: Seq[String]) = tree.map(_.baseAnnotatedLabel)
    new DocLexParser.Model(beliefsFactory, bundle, reannotate, indexed, {(f: Feature) => Some(weightsCache(f.toString))})

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

