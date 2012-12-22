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
import breeze.optimize.CachedBatchDiffFunction
import epic.parser.ParserParams.XbarGrammar
import features.{RuleFeature, TagAwareWordShapeFeaturizer}
import models._
import breeze.util.Index
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
      new ConstraintCoreGrammar(parser.augmentedGrammar, -7, false), null)

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
        100,
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

    val spanIndex = Index[Property[_]](Iterator(nerProp))
    val wordIndex = Index[Property[_]]()

    // lenses
    // joint-aware models
    val adaptedNerModel = new ChainNERModel(beliefsFactory, nerModel)

//    val propBuilder = new PropertyPropagator.Builder(spanIndex, wordIndex)
//    propBuilder.spans.associate(nerProp, corefNerProp, agreement = true)
//
//    val propModel = new PropertyPropagatingModel(propBuilder)

    // the big model!
    val epModel = new EPModel[ProcessedDocument, DocumentBeliefs](5, epInGold = false)( lexParseModel)
//    corefModel)
    //propModel)
//    val epModel = lexParseModel

    val obj = new ModelObjective(epModel, processedTrain)

    val opt = params.opt
    for( s <- opt.iterations(new CachedBatchDiffFunction(obj), obj.initialWeightVector(randomize = true))) {
      println(s.value)
    }
  }
}

object EverythingConstraints extends ConsoleLogging {
  case class Params(path: File,
                    treebank: ProcessedTreebank,
                    nfiles: Int = 100000,
                    iterPerEval: Int = 20,
                    maxParseLength : Int = 200,
                    opt: OptParams,
                    outdir: File = new File("constraints"))


  def main(args: Array[String]) {
    val params = CommandLineParser.readIn[Params](args)
    val (train, dev) = {
      val instances =  for {
        file <- params.path.listFiles take params.nfiles
        doc <- ConllOntoReader.readDocuments(file)
      } yield doc
      val train = instances.take(instances.length * 9 / 10)
      val dev = instances.drop(instances.length * 9 / 10)
      (train,dev)
    }

    println(params)
    params.outdir.exists() || params.outdir.mkdirs() || {throw new RuntimeException("Could not make directory " + params.outdir)}

    var trainTrees = for (d <- train; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, true)
    }

    var devTrees = for (d <- dev; s <- d.sentences) yield {
      params.treebank.makeTreeInstance(s.id, s.tree.map(_.label), s.words, true)
    }

    if(params.treebank.path.exists) {
      trainTrees ++= params.treebank.trainTrees
      devTrees ++= params.treebank.devTrees
    }

    val km = new KMAnnotator()
    val parser = GenerativeParser.annotated(new XbarGrammar(), km, trainTrees)

    val outConstraints = new File(params.outdir, "parseconstraints.ser.gz")
    val anchorings = if(!outConstraints.exists) {
      val map: Map[Seq[String], ConstraintAnchoring.RawConstraints] = extractParseConstraints(parser, trainTrees, devTrees, params.maxParseLength)
      breeze.util.writeObject(outConstraints, map)

       for ( (w, c) <- map) yield w -> c.toAnchoring(parser.grammar, parser.lexicon, w)
    } else {
      val map = breeze.util.readObject[Map[Seq[String], ConstraintAnchoring.RawConstraints]](outConstraints)
      for ( (w, c) <- map) yield w -> c.toAnchoring(parser.grammar, parser.lexicon, w)
    }

    // now we need to extract candidates mentions etc. We'll use a semicrf
    val np_tags = Set("NP","PRP","NN","NNS","NNPS","NNP","PRP$","NX,", "NML")
    val nps = BitSet.empty ++ (0 until parser.grammar.labelIndex.size).filter(i => np_tags(parser.grammar.labelIndex.get(i).label))

    var allMentions = 0
    var caughtByNP = 0
    var caughtByEnt = 0
    var caughtByEither = 0
    var caughtByThresh = 0
    var goldNPsCaught = 0
    var goldNPs = 0

    val xx_dspans = for(d <- train; s <- d.sentences if s.words.length < params.maxParseLength)  {
      val ns = s.tree.allChildren.filter(l => l.label.label == "NP").map(_.span).toSet
      goldNPs += ns.size
      val entities = s.ner.keys.map(_.span).toSet
      val mentions = s.coref.keys.map(_.span).toSet
      allMentions += mentions.size
      caughtByNP += (ns & mentions).size
      caughtByEnt += (mentions & entities).size
      caughtByEither += (mentions & (entities ++ ns)).size
      val anch = anchorings(s.words)
      caughtByThresh += mentions.filter(span => nps.exists(np => anch.scoreSpan(span.start, span.end, np) != Double.NegativeInfinity)).size
      goldNPsCaught += ns.filter(span => nps.exists(np => anch.scoreSpan(span.start, span.end, np) != Double.NegativeInfinity)).size
    }

    println(allMentions + " " + caughtByNP + " " + caughtByEnt + " " + caughtByEither + " " + caughtByThresh + " " + goldNPsCaught + " " + goldNPs)

    val trainSpansOfInterest = makeSpansOfInterest(train)
    val devSpansOfInterest = makeSpansOfInterest(dev)

    val model = new SegmentationModelFactory[Boolean](false).makeModel(trainSpansOfInterest)
    val obj = new ModelObjective(model, trainSpansOfInterest)
    val weights = params.opt.minimize(new CachedBatchDiffFunction(obj), obj.initialWeightVector(randomize = false))
    val seg = model.extractCRF(weights)
    println(SegmentationEval.eval(seg, trainSpansOfInterest, false))
    println("Dev")
    println(SegmentationEval.eval(seg, devSpansOfInterest, false))

    val on = seg.model.labelIndex(true)
    val outSOIs = new File(params.outdir, "spansOfInterest.ser.gz")
    val trainSOIs = for( inst <- (trainSpansOfInterest ++ devSpansOfInterest).par) yield {
      val onSpans = collection.mutable.BitSet()
      val marg = seg.marginal(inst.words)
      var beg = 0
      while ( beg < inst.words.length) {
        var end = beg + 1
        while(end <= inst.words.length) {
          if(marg.spanMarginal(on, beg, end) > math.exp(-6)) {
            onSpans(TriangularArray.index(beg, end)) = true
          }
          end += 1
        }
        beg += 1
      }

      inst.id -> onSpans
    }

    println(trainSOIs)


  }

  def extractParseConstraints(parser: SimpleChartParser[AnnotatedLabel, String],
                              trainTrees: Array[TreeInstance[AnnotatedLabel, String]],
                              devTrees: Array[TreeInstance[AnnotatedLabel, String]], maxParseLength: Int ): Map[Seq[String], ConstraintAnchoring.RawConstraints] = {
    val factory = new ConstraintCoreGrammar[AnnotatedLabel, String](parser.augmentedGrammar, -6, true)

    import epic.parser.projections.ProjectTreebankToConstraints._
    val trainOut = mapTrees(factory, trainTrees, parser.grammar.labelIndex, useTree = true, maxL = maxParseLength)
    //    val test = mapTrees(factory, treebank.testTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val devOut = mapTrees(factory, devTrees, parser.grammar.labelIndex, useTree = false, maxL = 10000)
    val map: Map[Seq[String], RawConstraints] = Map.empty ++ trainOut ++ devOut
    map
  }


  def makeSpansOfInterest(documents: IndexedSeq[Document]): IndexedSeq[Segmentation[Boolean, String]] = {
    for(d <- documents; s <- d.sentences) yield {
//      val mentionsOfInterest = s.coref.keys.map(_.span)
//      val usedStarts = mentionsOfInterest.map(_.start).toSet
//      val usedEnds = mentionsOfInterest.map(_.end).toSet
      val nerOfInterest = s.ner.keys.map(_.span)
      val spansOfInterest = nerOfInterest
      val sorted = spansOfInterest.toIndexedSeq.sortBy((_:Span).start)
      var out = new ArrayBuffer[(Boolean, Span)]()
      var last = 0
      for( span <- sorted ) {
        assert(last <= span.start, sorted + " " + s)
        while(span.start != last) {
          out += (false -> Span(last,last+1))
          last += 1
        }
        out += (true -> Span(span.start, span.end))
        last = span.end
      }
      while(s.words.length != last) {
        out += (false -> Span(last,last+1))
        last += 1
      }
      Segmentation(out, s.words, s.id)
    }

  }

}
