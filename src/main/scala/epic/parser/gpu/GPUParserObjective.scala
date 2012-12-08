package epic.parser.gpu

import breeze.optimize.{FirstOrderMinimizer, RandomizedGradientCheckingFunction, CachedBatchDiffFunction, BatchDiffFunction}
import breeze.linalg.{Counter2, DenseVector}
import epic.trees._
import annotations.FilterAnnotations
import annotations.FilterAnnotations
import epic.trees.annotations.FilterAnnotations
import epic.trees.annotations.FilterAnnotations
import breeze.linalg.NumericOps.Arrays._
import epic.parser.models.{FeaturizedLexicon, FeaturizedGrammar, IndexedFeaturizer}
import epic.framework.StandardExpectedCounts
import epic.parser._
import breeze.config.{Configuration, CommandLineParser, Help}
import epic.trees.annotations.{FilterAnnotations, TreeAnnotator}
import epic.parser.features.{GenFeaturizer, IndicatorFeature, TagAwareWordShapeFeaturizer}
import features.IndicatorFeature
import java.io.File
import epic.parser.ParseEval.Statistics
import epic.trees.annotations.FilterAnnotations
import epic.parser.ParseEval.Statistics
import epic.trees.annotations.FilterAnnotations
import epic.parser.ParseEval.Statistics
import io.Source
import epic.parser.ParseEval.Statistics
import epic.parser.ParseEval.Statistics
import epic.trees.TreeInstance
import epic.parser.ParseEval.Statistics
import epic.trees.TreeInstance
import epic.parser.ParseEval.Statistics
import projections.GrammarRefinements
import breeze.optimize.FirstOrderMinimizer.OptParams
import com.nativelibs4java.opencl.JavaCL

/**
 * 
 * @author dlwh
 */
class GPUParserObjective[C, L, W](parser: GrammarKernel[C, L, W],
                                  reannotate: (BinarizedTree[C], Seq[W]) => BinarizedTree[C],
                                  indexedFeatures: IndexedFeaturizer[C, L, W],
                                  sentences: IndexedSeq[TreeInstance[C, W]]) extends BatchDiffFunction[DenseVector[Double]] {
  def initialWeightVector(randomize: Boolean) = if(randomize) DenseVector.rand(featureIndex.size) else DenseVector.zeros[Double](featureIndex.size)

  def projections = indexedFeatures.projections.labels
  def featureIndex = indexedFeatures.index
  def grammar = indexedFeatures.grammar
  def lexicon = indexedFeatures.lexicon

  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]): (Double, DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(x, indexedFeatures)
    def scoreTag(w: IndexedSeq[W], pos: Int, tag: Int) = lexicon.scoreTag(projections.fineIndex.get(tag), w, pos)
    val weights = RuleScores(x.data.take(parser.structure.numBinaries), x.data.slice(parser.structure.numBinaries, parser.structure.numBinaries + parser.structure.numUnaries) :* parser.structure.nonIdentityMask)
    parser.ruleScores = Array(weights)
    parser.tagScorers = Array(scoreTag _)

    val mySents = batch.map(sentences)
    val parser.ExpectedCounts(ruleCounts, tagCounts, partition) = parser.expectedRuleCounts(mySents.map(_.words))
    val wordCounts = computeFeaturesForLexicon(mySents, tagCounts)
    val gc = goldCounts(mySents, x)
    val finalCounts = new StandardExpectedCounts(partition, DenseVector.vertcat(ruleCounts, wordCounts), featureIndex) -= gc
    finalCounts.loss -> finalCounts.counts
  }

  def fullRange: IndexedSeq[Int] = 0 until sentences.length

  private def computeFeaturesForLexicon(sents: IndexedSeq[TreeInstance[C, W]], counts: IndexedSeq[IndexedSeq[DenseVector[Double]]]) = {
    val feats = DenseVector.zeros[Double](featureIndex.size)
    for( (s,c) <- (sents zip counts)) {
      for(i <- 0 until s.words.length; a <- 0 until projections.fineIndex.size)
        for(f <- indexedFeatures.featuresFor(a, s.words, i)) {
          feats(f) += c(i)(a)
        }
    }

    feats
  }

  private def goldCounts(sents: IndexedSeq[TreeInstance[C, W]], weights: DenseVector[Double]) = {
    val lexicon = new FeaturizedLexicon(weights, indexedFeatures)
    val grammar = FeaturizedGrammar(this.grammar, this.lexicon, indexedFeatures.projections, weights, indexedFeatures, lexicon)
    sents.foldLeft(StandardExpectedCounts.zero(featureIndex)) { (counts, ti) =>
      val reannotated = reannotate(ti.tree, ti.words)
      val product = AugmentedAnchoring.fromRefined(grammar.anchor(ti.words))
      val ecounts = LatentTreeMarginal(product, projections, reannotated).expectedCounts(indexedFeatures)

      counts += ecounts
    }
  }
}

object GPUParserTrainer {

  /**
   * Trains a sequence of parsers and evaluates them.
   */
  def main(args: Array[String]) {
    import ParserParams.JointParams

    val (baseConfig, files) = CommandLineParser.parseArguments(args)
    val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
    val params = try {
      config.readIn[JointParams[Params]]("test")
    } catch {
      case e =>
        e.printStackTrace()
        println(breeze.config.GenerateHelp[JointParams[Params]](config))
        sys.exit(1)
    }

    if(params.help) {
      println(breeze.config.GenerateHelp[JointParams[Params]](config))
      System.exit(1)
    }
    println("Training Parser...")
    println(params)

    val parsers = trainParser(params.treebank.trainTrees, params.trainer)

    import params.treebank._
  }



  case class Params(baseParser: ParserParams.XbarGrammar,
                     @Help(text= """The kind of annotation to do on the refined grammar. Default uses no annotations.
  You can also epic.trees.annotations.KMAnnotator to get more or less Klein and Manning 2003.
                                """)
                    annotator: TreeAnnotator[AnnotatedLabel, String, AnnotatedLabel] = FilterAnnotations(),
                    @Help(text="Path to substates to use for each symbol. Uses numStates for missing states.")
                    substates: File = null,
                    @Help(text="Number of states to use. Overridden by substates file")
                    numStates: Int = 2,
                    @Help(text="Old weights to initialize with. Optional.")
                    oldWeights: File = null,
                    opt: OptParams,
                    @Help(text="How often to run on the dev set.")
                    iterationsPerEval: Int = 100,
                    @Help(text="How many iterations to run.")
                    maxIterations: Int = 1002,
                    @Help(text="How often to look at a small set of the dev set.")
                    iterPerValidate: Int = 10,
                    @Help(text="How many threads to use, default is to use whatever Scala thinks is best.")
                    threads: Int = -1,
                    @Help(text="Should we randomize weights? Some models will force randomization.")
                    randomize: Boolean = false,
                    useGPU: Boolean = true)

  protected val paramManifest: Manifest[Params] = implicitly[Manifest[Params]]

  /**
   * The main point of entry for implementors. Should return a sequence
   * of parsers
   */
  def trainParser(trainTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]],
                  params: Params): Iterator[(String, Parser[AnnotatedLabel, String])] = {
    import params._
    val annTrees: IndexedSeq[TreeInstance[AnnotatedLabel, String]] = trainTrees.map(annotator(_))

    val (annWords, annBinaries, annUnaries) = this.extractBasicCounts(annTrees)

    val (xbarGrammar, xbarLexicon) = baseParser.xbarGrammar(trainTrees)

    val substateMap = if (substates != null && substates.exists) {
      val in = Source.fromFile(substates).getLines()
      val pairs = for (line <- in) yield {
        val split = line.split("\\s+")
        AnnotatedLabel(split(0)) -> split(1).toInt
      }
      pairs.toMap + (xbarGrammar.root -> 1)
    } else {
      Map(xbarGrammar.root -> 1)
    }

    def split(x: AnnotatedLabel): Seq[(AnnotatedLabel, Int)] = {
      for (i <- 0 until substateMap.getOrElse(x, numStates)) yield (x, i)
    }

    val presplit = xbarGrammar.labelIndex.map(l => l -> split(l)).toMap

    def unsplit(x: (AnnotatedLabel, Int)): AnnotatedLabel = x._1

    def splitRule[L, L2](r: Rule[L], split: L=>Seq[L2]):Seq[Rule[L2]] = r match {
      case BinaryRule(a, b, c) => for(aa <- split(a); bb <- split(b); cc <- split(c)) yield BinaryRule(aa, bb, cc)
        // don't allow non-identity rule refinements for identity rewrites
      case UnaryRule(a, b, chain) if a == b => for(aa <- split(a)) yield UnaryRule(aa, aa, chain)
      case UnaryRule(a, b, chain) => for(aa <- split(a); bb <- split(b)) yield UnaryRule(aa, bb, chain)
    }

    val gen = new TagAwareWordShapeFeaturizer(annWords)
    def labelFlattener(l: (AnnotatedLabel, Int)) = {
      val basic = Seq(l)
      basic map (IndicatorFeature)
    }
    val feat = new GenFeaturizer[(AnnotatedLabel, Int), String](gen, labelFlattener _)

    val annGrammar: BaseGrammar[AnnotatedLabel] = BaseGrammar(annTrees.head.tree.label, annBinaries, annUnaries)
    val firstLevelRefinements = GrammarRefinements(xbarGrammar, annGrammar, {(_: AnnotatedLabel).baseAnnotatedLabel})
    val secondLevel = GrammarRefinements(annGrammar, split _, {splitRule(_ :Rule[AnnotatedLabel], presplit)}, unsplit)
    val finalRefinements = firstLevelRefinements compose secondLevel

    val refinements = finalRefinements
    val refinedGrammar = BaseGrammar(refinements.labels.refinementsOf(xbarGrammar.root)(0),
      refinements.labels.fineIndex,
      refinements.rules.fineIndex)

    implicit val context = if(useGPU) {
      JavaCL.createBestContext()
    } else {
      val cpuPlatform = JavaCL.listPlatforms().filter(_.listCPUDevices(true).nonEmpty).head
      cpuPlatform.createContext(new java.util.HashMap(), cpuPlatform.listCPUDevices(true):_*)
    }
    println(context)

    val gpuParser = new GrammarKernel(xbarGrammar, finalRefinements, refinedGrammar,  xbarLexicon, Array(RuleScores(DenseVector.zeros(), DenseVector.zeros())), Array(null))

    println(finalRefinements.labels)

    val indexedFeaturizer = IndexedFeaturizer(xbarGrammar, xbarLexicon, trainTrees, feat, finalRefinements)

    val obj = new GPUParserObjective(gpuParser, annotator, indexedFeaturizer, trainTrees)
    val cachedObj = new CachedBatchDiffFunction(obj)
    val checking = new RandomizedGradientCheckingFunction(cachedObj, 1E-4, toString = {
      (i: Int) => obj.featureIndex.get(i).toString
    })
    val init = obj.initialWeightVector(randomize)

    type OptState = FirstOrderMinimizer[DenseVector[Double], BatchDiffFunction[DenseVector[Double]]]#State


    for ((state, iter) <- params.opt.iterations(cachedObj, init).take(maxIterations).zipWithIndex
         if iter != 0 && iter % iterationsPerEval == 0) yield try {
      gpuParser
    } catch {
      case e => println(e); e.printStackTrace(); throw e
    }
  }


  protected def extractBasicCounts[L, W](trees: IndexedSeq[TreeInstance[L, W]]): (Counter2[L, W, Double], Counter2[L, BinaryRule[L], Double], Counter2[L, UnaryRule[L], Double]) = {
    GenerativeParser.extractCounts(trees)
  }
}
