//package epic.corefdense
//
//import scala.util.Random
//import edu.berkeley.nlp.futile.classify.ClassifyUtils
//import edu.berkeley.nlp.futile.util.Logger
//
//case class CorefEx(val antecedents: Array[Array[Double]],
//                   val anaphor: Array[Double],
//                   val correctIdx: Int);
//
//class CorefNN(val inputSize: Int,
//              val embeddingSize: Int,
//              val exSize: Int) extends LikelihoodAndGradientComputer[CorefEx] {
//  
//  val mat = new GMatrix(embeddingSize, inputSize)
//  
//  private val anaphorEmbedding = new Array[Float](embeddingSize)
//  private val antecedentEmbeddings = Array.tabulate(exSize)(i => new Array[Float](embeddingSize))
//  private val scoresArr = new Array[Float](exSize)
//  private val logProbArr = new Array[Float](exSize)
//  private val probArr = new Array[Float](exSize)
//  
//  private def computeNormalizedLogProbs(ex: CorefEx, weights: Array[Float]) {
//    // Feedforward everyone
//    mat.mmul(weights, ex.anaphor, anaphorEmbedding)
//    for (i <- 0 until ex.antecedents.size) {
//      mat.mmul(weights, ex.antecedents(i), antecedentEmbeddings(i))
//      scoresArr(i) = 0.0F
//      var j = 0;
//      while (j < embeddingSize) {
//        scoresArr(i) += antecedentEmbeddings(i)(j) * anaphorEmbedding(j)
//      }
//    }
//    ClassifyUtils.softmaxFloat(scoresArr, logProbArr)
//    for (i <- 0 until logProbArr.size) {
//      logProbArr(i) = 0.0F;
//    }
//    (0 until logProbArr.size).foreach(i => probArr(i) = Math.exp(logProbArr(i)).toFloat)
//  }
//  
//  def getInitialWeights: Array[Float] = {
//    mat.getQuasiIdentityWeights;
//  }
//  
//  def predict(ex: CorefEx, weights: Array[Float]): Int = {
//    computeNormalizedLogProbs(ex, weights)
//    ClassifyUtils.argMaxIdxFloat(logProbArr)
//  }
//  
//  def computeObjective(ex: CorefEx, weights: Array[Float]) = {
//    computeNormalizedLogProbs(ex, weights)
//    logProbArr(ex.correctIdx)
//  }
//
//  def accumulateGradientAndComputeObjective(ex: CorefEx, weights: Array[Float], gradient: Array[Float]) = {
//    computeNormalizedLogProbs(ex, weights)
//    // Accumulate the gradient
//    
//    logProbArr(ex.correctIdx)
//  }
//}
//
//object CorefNN {
//
//  def randVect(numDim: Int, rng: Random): Array[Double] = {
//    val vector = Array.tabulate(numDim)(i => rng.nextGaussian())
//    val norm = Math.sqrt(vector.map(x => x * x).reduce(_ + _))
//    (0 until vector.size).foreach(i => vector(i) = vector(i)/norm);
//    vector
//  }
//  
//  /**
//   * Constructs numExamples examples with numAntecedents antecedents each, with each having
//   * numDim dimensions. The first numDim/4 coordinates are fixed to be the same between
//   * the anaphor and antecedent (so that hopefully the projection will focus on these)
//   */
//  def randExamples(numExamples: Int, numAntecedents: Int, numDim: Int): Array[CorefEx] = {
//    val rng = new Random(0)
//    Array.tabulate(numExamples)(i => {
//      val anaphorVect = randVect(numDim, rng)
//      val firstAntecedentVect = randVect(numDim, rng)
//      (0 until numDim/4).foreach(j => firstAntecedentVect(i) = anaphorVect(i))
//      new CorefEx(Array.tabulate(numAntecedents)(j => if (j == 0) firstAntecedentVect else randVect(numDim, rng)), anaphorVect, 0)
//    })
//  }
//  
//  def main(args: Array[String]) {
//    val trainExs = randExamples(1000, 5, 20)
//    val testExs = randExamples(1000, 5, 20)
//    val corefNN = new CorefNN(trainExs.head.anaphor.size, 5, trainExs.head.antecedents.size)
////    val nnBlas = new SimpleNNBest(3, 20, 2)
//    Logger.logss(trainExs(0))
////    GeneralTrainer.checkGradient(trainSamples.slice(0, 1), nn, nn.numFeats, verbose = true)
////    System.exit(0);
//    val initialWeights = corefNN.getInitialWeights;
//    val weights = new GeneralTrainer().train(trainExs, corefNN, 1.0, 0.0000001, 10, 100, initialWeights, verbose = false);
//    
//    var correct = 0;
//    for (i <- 0 until testExs.size) {
//      if (corefNN.predict(testExs(i), weights) == testExs(i).correctIdx) {
//        correct += 1;
//      }
//    }
//    Logger.logss(correct + " / " + testExs.size);
//  }
//}