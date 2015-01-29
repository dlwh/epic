package epic.corefdense

import edu.berkeley.nlp.entity.coref.DocumentInferencerBasic
import edu.berkeley.nlp.entity.coref.MentionPropertyComputer
import edu.berkeley.nlp.entity.sem.QueryCountsBundle
import edu.berkeley.nlp.entity.coref.NumberGenderComputer
import edu.berkeley.nlp.entity.sem.BasicWordNetSemClasser
import edu.berkeley.nlp.entity.coref.PairwiseScorer
import edu.berkeley.nlp.entity.coref.CorefEvaluator
import edu.berkeley.nlp.entity.coref.DocumentGraph
import edu.berkeley.nlp.entity.coref.CorefDoc
import edu.berkeley.nlp.entity.lang.Language
import edu.berkeley.nlp.entity.sem.SemClasser
import edu.berkeley.nlp.entity.ConllDocReader
import edu.berkeley.nlp.entity.coref.CorefPruner
import edu.berkeley.nlp.entity.coref.CorefDocAssembler
import edu.berkeley.nlp.entity.coref.LexicalCountsBundle
import edu.berkeley.nlp.entity.WordNetInterfacer
import edu.berkeley.nlp.entity.coref.CorefFeaturizerTrainer
import edu.berkeley.nlp.entity.coref.OrderedClustering
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint
import epic.dense.TanhTransform
import epic.dense.IdentityTransform
import epic.dense.Word2VecIndexed
import edu.berkeley.nlp.futile.LightRunner
import epic.dense.AffineTransform
import breeze.linalg.DenseVector
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import edu.berkeley.nlp.entity.coref.PairwiseLossFunctions
import edu.berkeley.nlp.entity.coref.ConjScheme
import edu.berkeley.nlp.entity.coref.ConjFeatures
import edu.berkeley.nlp.entity.coref.FeatureSetSpecification
import epic.features.SegmentedIndex
import epic.framework.Feature
import scala.collection.mutable.HashSet

object CorefDriver {
  
  // ARGUMENTS ///////
  
  val numberGenderDataPath = "data/gender.data"
  val docSuffix = "auto_conll"
  val wordNetPath = ""
  val pruningStrategy = "distance:10000:5000" // TODO: FIGURE OUT PRUNING
  
  val hiddenSize = 100
  
  val numItrs = 5
  val eta = 1.0F
  val reg = 0.001F
  val initWeightsScale = 0.01
  val batchSize = 1
    
  val featsToUse = ""
  
  val lossFcn = "customLoss-0.1-3-1";
  val conllEvalScriptPath = "scorer/v7/scorer.pl"
  val word2vecPath = ""
  
  val nonLinear = true;
  
  
  val trainPath = "";
  val trainSize = -1;
  val testPath = "";
  val testSize = -1;
  
  val checkEmpiricalGradient = false
  
  val oldStyleCorefNN = false;
  
  ////////////////////

  def loadCorefDocs(path: String, size: Int, suffix: String, maybeNumberGenderComputer: Option[NumberGenderComputer]): Seq[CorefDoc] = {
    val docs = ConllDocReader.loadRawConllDocsWithSuffix(path, size, suffix);
    val assembler = CorefDocAssembler(Language.ENGLISH, false);
    val mentionPropertyComputer = new MentionPropertyComputer(maybeNumberGenderComputer);
    val corefDocs = docs.map(doc => assembler.createCorefDoc(doc, mentionPropertyComputer));
    CorefDocAssembler.checkGoldMentionRecall(corefDocs);
    corefDocs;
  }
  
  def preprocessDocsCacheResources(allDocGraphs: Seq[DocumentGraph]) {
    if (wordNetPath != "") {
      val wni = new WordNetInterfacer(wordNetPath);
      allDocGraphs.foreach(_.cacheWordNetInterfacer(wni));
    }
  }
  
  def prepareTestDocuments(devPath: String, devSize: Int): Seq[DocumentGraph] = {
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(numberGenderDataPath);
    val devDocs = loadCorefDocs(devPath, devSize, docSuffix, Some(numberGenderComputer));
    val devDocGraphs = devDocs.map(new DocumentGraph(_, false));
    preprocessDocsCacheResources(devDocGraphs);
    CorefPruner.buildPruner(pruningStrategy).pruneAll(devDocGraphs);
    devDocGraphs;
  }
  
  def main(args: Array[String]) {
    LightRunner.initializeOutput(CorefDriver.getClass())
    LightRunner.populateScala(CorefDriver.getClass(), args)
    val numberGenderComputer = NumberGenderComputer.readBergsmaLinData(numberGenderDataPath);
    val queryCounts: Option[QueryCountsBundle] = None;
    val trainDocs = loadCorefDocs(trainPath, trainSize, docSuffix, Some(numberGenderComputer));
    // Randomize
    val trainDocsReordered = new scala.util.Random(0).shuffle(trainDocs);
    val lexicalCounts = LexicalCountsBundle.countLexicalItems(trainDocs, 20);
    val semClasser: Option[SemClasser] = Some(new BasicWordNetSemClasser);
    val trainDocGraphs = trainDocsReordered.map(new DocumentGraph(_, true));
    preprocessDocsCacheResources(trainDocGraphs);
    CorefPruner.buildPruner(pruningStrategy).pruneAll(trainDocGraphs);
    
    // N.B. does not need to be updated to CorefNeuralModel2 because we're just getting the vocab
    val trainVoc = trainDocGraphs.map(docGraph => docGraph.getMentions.map(ment => CorefNeuralModel.extractRelevantMentionWords(ment).toSet).reduce(_ ++ _)).reduce(_ ++ _)
    
    val featureIndexer = new Indexer[String]();
    val word2vecRaw = Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), trainVoc.map(str => Word2Vec.convertWord(str)), Int.MaxValue, true)
    val word2vecRawDoubleVect = word2vecRaw.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
    val word2vecIndexed = Word2VecIndexed(word2vecRawDoubleVect, (str: String) => Word2Vec.convertWord(str))
    
//    val corefNeuralModel 
    
    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
//    val featurizer = new SimplePairwiseIndexingFeaturizerJoint(featureIndexer, featsToUse.split(",").toSet)
    val featureSetSpec = FeatureSetSpecification("FINAL", ConjScheme.COARSE_BOTH, ConjFeatures.TYPE_OR_CANONICAL_PRON, "", "");
    val featurizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, None, None)
    val lossFcnObj = PairwiseLossFunctions(lossFcn)
    
//    val (transform, corefNeuralModel): (AffineTransform[DenseVector[Double],DenseVector[Double]],CorefPredictor]) = if (oldStyleCorefNN) {
    val (transform: AffineTransform[DenseVector[Double],DenseVector[Double]], corefNeuralModel: CorefPredictor) = if (oldStyleCorefNN) {
      val vecSize = word2vecIndexed.wordRepSize * CorefNeuralModel.extractRelevantMentionWords(trainDocGraphs.head.getMention(0)).size
      Logger.logss("Net size: " + vecSize + " x " + hiddenSize + " x " + vecSize)
      val transform = if (nonLinear) {
        new AffineTransform(vecSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, vecSize, new IdentityTransform[DenseVector[Double]]())))
      } else {
        new AffineTransform(vecSize, vecSize, new IdentityTransform[DenseVector[Double]]())
      }
      Logger.logss("Transform index size: " + transform.index.size)
      (transform, new CorefNeuralModel(featurizer, transform, word2vecIndexed, lossFcnObj))
    } else {
      val vecSize = word2vecIndexed.wordRepSize * CorefNeuralModel2.extractRelevantMentionWords(trainDocGraphs.head.getMention(0), trainDocGraphs.head.getMention(0)).size
      Logger.logss("Net size: " + vecSize + " x " + hiddenSize + " x " + vecSize)
      val transform = if (nonLinear) {
        new AffineTransform(1, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, vecSize, new IdentityTransform[DenseVector[Double]]())))
      } else {
        new AffineTransform(1, vecSize, new IdentityTransform[DenseVector[Double]]())
      }
      Logger.logss("Transform index size: " + transform.index.size)
      (transform, new CorefNeuralModel2(featurizer, transform, word2vecIndexed, lossFcnObj))
    }
    new CorefFeaturizerTrainer().featurizeBasic(trainDocGraphs, featurizer)
    
    
    val initialWeights = corefNeuralModel.getInitialWeights(initWeightsScale);
//    val numItrs = 100
    
    if (checkEmpiricalGradient) {
      val indices = HashSet[Int]();
      indices ++= 0 until 5
//      indices ++= ((vecSize + 1) * hiddenSize) until ((vecSize + 1) * hiddenSize + 5)
      indices ++= transform.index.size until transform.index.size + 5
      GeneralTrainer.checkGradientFromPoint(trainDocGraphs, corefNeuralModel, initialWeights, Array.tabulate(initialWeights.size)(i => 0.0), indices.toSet, verbose = true)
    }
    val weights = new GeneralTrainer().trainAdagrad(trainDocGraphs, corefNeuralModel, eta, reg, batchSize, numItrs, initialWeights, verbose = true);
    
    
    
//    val basicFeaturizer = new PairwiseIndexingFeaturizerJoint(featureIndexer, featureSetSpec, lexicalCounts, queryCounts, semClasser);
//    val featurizerTrainer = 
//    featurizerTrainer.featurizeBasic(trainDocGraphs, basicFeaturizer);
//    PairwiseIndexingFeaturizer.printFeatureTemplateCounts(featureIndexer)
//
//    val basicInferencer = new DocumentInferencerBasic()
//    val lossFcnObjFirstPass = PairwiseLossFunctions(Driver.lossFcn);
//    val firstPassWeights = featurizerTrainer.train(trainDocGraphs,
//                                                   basicFeaturizer,
//                                                   Driver.eta.toFloat,
//                                                   Driver.reg.toFloat,
//                                                   Driver.batchSize,
//                                                   lossFcnObjFirstPass,
//                                                   Driver.numItrs,
//                                                   basicInferencer);
//    new PairwiseScorer(basicFeaturizer, firstPassWeights).pack;
    
    
    val devDocGraphs = prepareTestDocuments(testPath, testSize);
//    new CorefFeaturizerTrainer().featurizeBasic(devDocGraphs, scorer.featurizer);  // dev docs already know they are dev docs so they don't add features
//    val basicInferencer = new DocumentInferencerBasic();
//    val (allPredBackptrs, allPredClusterings) = basicInferencer.viterbiDecodeAllFormClusterings(devDocGraphs, scorer);
    Logger.startTrack("Decoding dev");
    val (allPredBackptrs, allPredClusterings) = corefNeuralModel.predictAllFormClusterings(devDocGraphs, weights)
    Logger.logss(CorefEvaluator.evaluateAndRender(devDocGraphs, allPredBackptrs, allPredClusterings, conllEvalScriptPath, "DEV: ", ""));
    Logger.endTrack();
    LightRunner.finalizeOutput()
  }
  
//  def getInterestingIndexSet(transform: AffineTransform[DenseVector[Double],DenseVector[Double]]) {
//    if (transform.index.isInstanceOf[SegmentedIndex[Feature,_]]) {
//      transform.index.asInstanceOf[SegmentedIndex[Feature]].
//    }
//    transform.get
//  }
}
