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
import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.Indexer
import edu.berkeley.nlp.entity.coref.PairwiseIndexingFeaturizerJoint
import epic.dense.TanhTransform
import epic.dense.IdentityTransform
import edu.berkeley.nlp.futile.LightRunner
import epic.dense.AffineTransform
import breeze.linalg.DenseVector
import epic.dense.Word2VecSurfaceFeaturizerIndexed
import edu.berkeley.nlp.entity.coref.PairwiseLossFunctions

object CorefDriver {
  
  val numberGenderDataPath = "data/gender.data"
  val docSuffix = "auto_conll"
  val wordNetPath = ""
  val pruningStrategy = "" // TODO: FIGURE OUT PRUNING
    
  val hiddenSize = 100
    
  val featsToUse = ""
  
  
  val lossFcn = "customLoss-0.1-3-1";
  val conllEvalScriptPath = "scorer/v7/scorer.pl"
  val word2vecPath = ""
  
  
  val trainPath = "";
  val trainSize = -1;
  val testPath = "";
  val testSize = -1;

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
    
    val trainVoc = trainDocGraphs.map(docGraph => docGraph.getMentions.map(ment => CorefNeuralModel.extractRelevantMentionWords(ment).toSet).reduce(_ ++ _)).reduce(_ ++ _)
    
    val featureIndexer = new Indexer[String]();
    val word2vecRaw = Word2Vec.smartLoadVectorsForVocabulary(word2vecPath.split(":"), trainVoc.map(str => Word2Vec.convertWord(str)), true)
    val word2vecRawDoubleVect = word2vecRaw.map(keyValue => (keyValue._1 -> keyValue._2.map(_.toDouble)))
    val word2vec = Word2VecSurfaceFeaturizerIndexed(word2vecRawDoubleVect, (str: String) => Word2Vec.convertWord(str))
    
//    val corefNeuralModel 
    val vecSize = word2vec.vectorSize * CorefNeuralModel.extractRelevantMentionWords(trainDocGraphs.head.getMention(0)).size
    
    featureIndexer.getIndex(PairwiseIndexingFeaturizerJoint.UnkFeatName);
    val sparseFeaturizer = new SimplePairwiseIndexingFeaturizerJoint(featureIndexer, featsToUse.split(",").toSet)
    val transform = new AffineTransform(vecSize, hiddenSize, new TanhTransform(new AffineTransform(hiddenSize, vecSize, new IdentityTransform[DenseVector[Double]]())))
    val lossFcnObj = PairwiseLossFunctions(lossFcn)
    val corefNeuralModel = new CorefNeuralModel(sparseFeaturizer, transform, word2vec, lossFcnObj)
    new CorefFeaturizerTrainer().featurizeBasic(trainDocGraphs, sparseFeaturizer)
    
    
    val initialWeights = corefNeuralModel.getInitialWeights;
//    val numItrs = 100
    val numItrs = 50
    val eta = 1.0F
    val reg = 0.0000001F
//    val eta = 1.0F
//    val reg = 1.0F
//    val weights = new GeneralTrainer().train(trainExs, corefNN, eta, reg, 10, numItrs, initialWeights, verbose = false);
    
    
    
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
//    Logger.startTrack("Decoding dev");
//    val basicInferencer = new DocumentInferencerBasic();
//    val (allPredBackptrs, allPredClusterings) = basicInferencer.viterbiDecodeAllFormClusterings(devDocGraphs, scorer);
    val (allPredBackptrs, allPredClusterings) = (null, null)
    Logger.logss(CorefEvaluator.evaluateAndRender(devDocGraphs, allPredBackptrs, allPredClusterings, conllEvalScriptPath, "DEV: ", ""));
    Logger.endTrack();
    LightRunner.finalizeOutput()
  }
}