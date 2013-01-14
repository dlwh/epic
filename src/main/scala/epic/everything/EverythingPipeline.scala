package epic.everything

import java.io.File
import breeze.config.CommandLineParser
import epic.ontonotes.ConllOntoReader
import epic.trees._
import epic.parser._
import epic.parser.projections.{ConstraintAnchoring, ConstraintCoreGrammar}
import breeze.util.logging.ConsoleLogging
import collection.immutable.BitSet
import epic.sequences.{SegmentationEval, SegmentationModelFactory}
import collection.mutable.ArrayBuffer
import epic.framework.{EPModel, ModelObjective}
import breeze.collection.mutable.TriangularArray
import breeze.optimize.{RandomizedGradientCheckingFunction, CachedBatchDiffFunction}
import epic.parser.ParserParams.XbarGrammar
import features.{RuleFeature, TagAwareWordShapeFeaturizer}
import models._
import breeze.util.{Lens, Index}
import epic.parser.models._
import breeze.linalg._
import epic.trees.annotations.StripAnnotations
import epic.trees.annotations.KMAnnotator
import epic.trees.annotations.AddMarkovization
import epic.trees.annotations.PipelineAnnotator
import epic.trees.ProcessedTreebank
import epic.trees.TreeInstance
import breeze.optimize.FirstOrderMinimizer.OptParams
import epic.sequences.Segmentation
import epic.parser.models.LexGrammarBundle
import epic.trees.Span
import projections.ConstraintAnchoring.RawConstraints


object EverythingPipeline {
  case class Params(path: File,
                    treebank: ProcessedTreebank,
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    constraints: ParserParams.Constraints[AnnotatedLabel, String],
                    baseParser: ParserParams.XbarGrammar,
                    opt: OptParams)

  def main(args: Array[String]) {

    val params = CommandLineParser.readIn[Params](args)

    val (train, test) = {
      val instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
      } yield doc
      val train = instances.take(instances.length * 9 / 10)
      val test = instances.drop(instances.length * 9 / 10)
      (train,test)
    }



    /////////////////
    // base models///
    /////////////////
    // NER
    val nerModel = new SegmentationModelFactory[NERType.Value](NERType.OutsideSentence).makeModel(train.flatMap(_.sentences).map(_.nerSegmentation))
    // NERProperties
    val nerProp = Property("NER::Type", nerModel.labelIndex)



    var trainTrees = for (d <- train; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, true)
    }

    if(params.treebank.path.exists) {
      trainTrees ++= params.treebank.trainTrees
    }

    val km = new PipelineAnnotator[AnnotatedLabel, String](Seq(StripAnnotations(), AddMarkovization()))
    val parser: SimpleChartParser[AnnotatedLabel, String] = GenerativeParser.annotated(new XbarGrammar(), km, trainTrees)

    val docProcessor = new ProcessedDocument.Factory(params.treebank.process,
      new ConstraintCoreGrammar(parser.augmentedGrammar, -7), null)

    val beliefsFactory = new DocumentBeliefs.Factory(parser.grammar, nerProp)
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

    val processedTrain = train.map(docProcessor)

    val lexParseModel = {

      val trees = trainTrees.map(StripAnnotations())
      val (initLexicon, initBinaries, initUnaries) = GenerativeParser.extractCounts(trees)

      val wordIndex: Index[String] = Index(trainTrees.iterator.flatMap(_.words))
      val summedCounts = sum(initLexicon, Axis._0)
      val shapeGen = new SimpleWordShapeGen(initLexicon, summedCounts)
      val tagShapeGen = new TagAwareWordShapeFeaturizer(initLexicon)

      val lexicon:Lexicon[AnnotatedLabel, String] = initLexicon

      def ruleGen(r: Rule[AnnotatedLabel]) = IndexedSeq(RuleFeature(r))
      def validTag(w: String) = lexicon.tagsForWord(w).toArray

      val headFinder = HeadFinder.collins
      val feat = new StandardFeaturizer(wordIndex,
      parser.grammar.labelIndex,
      parser.grammar.index,
      ruleGen,
      shapeGen,
      { (w:Seq[String], pos: Int) => tagShapeGen.featuresFor(w, pos)})

      val indexed =  IndexedLexFeaturizer.extract[AnnotatedLabel, String](feat,
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
      new DocLexParser.Model(beliefsFactory, bundle, reannotate, indexed)
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
    val epModel = new EPModel[ProcessedDocument, DocumentBeliefs](30, epInGold = false)(

      adaptedNerModel
      , lexParseModel
      , assocSynNer
    )
//    corefModel)
    //propModel)
//    val epModel = lexParseModel

    val obj = new ModelObjective(epModel, processedTrain)
    val cachedObj = new CachedBatchDiffFunction(obj)


    val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-4, toString = {
      (i: Int) => epModel.featureIndex.get(i).toString
    })

    val opt = params.opt
    for( s <- opt.iterations(cachedObj, obj.initialWeightVector(randomize = true))) {
      println(s.value)
    }
  }
}

