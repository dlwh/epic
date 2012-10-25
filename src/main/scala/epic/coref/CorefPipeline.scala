package epic.coref

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.everything.{DSpan, Document}
import epic.ontonotes.ConllOntoReader
import breeze.linalg.DenseVector
import epic.framework.{ModelObjective, Feature}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.CachedBatchDiffFunction
import breeze.util.Encoder
import pairwise._
import collection.mutable.ArrayBuffer

/**
 *
 * @author dlwh
 */

object CorefPipeline extends App {
  case class Params(path: File, name: String = "eval/coref", nfiles: Int = 100000)
  val (baseConfig, files) = CommandLineParser.parseArguments(args)
  val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
  val params = config.readIn[Params]("")

  val instanceFactory = new CorefInstance.Factory(CorefInstance.goldMentions)

  val instances = for {
    file <- params.path.listFiles take params.nfiles
    doc <- ConllOntoReader.readDocuments(file)
  } yield instanceFactory(doc)


  val feat = new SimplePairwiseFeaturizer
  val extractors = Properties.allExtractors
  val model = new PropCorefModelFactory(feat, extractors).makeModel(instances)

  val obj = new ModelObjective(model, instances)
  val cached = new CachedBatchDiffFunction(obj)

  println(model.featureIndex)
  val init = OptParams(useStochastic=true, batchSize=25, tolerance=1E-3, maxIterations = 100).minimize(cached, obj.initialWeightVector(randomize = true))
  val optimum =  OptParams(useStochastic = false, tolerance = 1E-3).minimize(cached, init)
  val inference = model.inferenceFromWeights(optimum)


  var allResults = Stats(0, 0, 0)


  var output = new ArrayBuffer[(Document,Seq[Set[DSpan]])]
  for(i <- instances) {
    println("=================")
    println("Text: ")
    i.words foreach { s => println(s.mkString(" "))}
    println("Gold: ")
    for(cluster <- sortClusters(i.unindexedClusters)) {
      println(formatCluster(cluster))
    }
    println("Guess: ")
    val guess = inference.decode(i, inference.baseAugment(i))
    for(cluster <- sortClusters(guess)) {
      println(formatCluster(cluster))
    }

    output += (i.doc -> guess)


    val results = eval(i.unindexedClusters.toIndexedSeq, guess)
    println(results)
    allResults += results
  }
  println("=================")

  println("Weights: ")
  for( (f,v) <- Encoder.fromIndex(model.featureIndex).decode(optimum).iterator.toIndexedSeq[(Feature, Double)].sortBy(_._2)) {
    println(v + " " + f)
  }
  println("=================")
  println(allResults)

  println("Copying output results to " + params.name + " for use in official evaluation")
  ConllEval.evaluate(params.name, output)

  case class Stats(numInter: Int, numGold: Int, numGuess: Int) {
    def +(stats: Stats) = Stats(numInter + stats.numInter, numGold + stats.numGold, numGuess + stats.numGuess)

    def precision = if(numInter == 0.0) 0.0 else numInter.toDouble / numGuess
    def recall = if(numInter == 0.0) 0.0 else numInter.toDouble / numGold
    def f1 = 2.0 / (1.0/precision + 1.0/recall)

    override def toString() = {
      "Stats( f1 = " + f1 + ", precision = " + precision + ", recall = "  + recall +")"
    }
  }

  def eval(gold: IndexedSeq[Set[DSpan]], guess: IndexedSeq[Set[DSpan]]) = {
    val pairsGold = getCoreferentPairs(gold)
    val pairsGuess = getCoreferentPairs(guess)

    val missedPairs = pairsGold.toSet -- pairsGuess.toSet
    println("Missed Pairs:")
    missedPairs foreach println

    val wrongPairs = pairsGuess.toSet -- pairsGold.toSet
    println("Wrong Pairs:")
    wrongPairs foreach println

    Stats(pairsGold.toSet & pairsGuess.toSet size, pairsGold.size, pairsGuess.size)
  }

  def sortClusters(clusters: Iterable[Set[DSpan]]):IndexedSeq[Set[DSpan]] = {
    clusters.toIndexedSeq[Set[DSpan]].sortBy(_.min)
  }

  def formatCluster(set: Set[DSpan]) = {
    set.toIndexedSeq[DSpan].sorted.mkString(", ")
  }

  def getCoreferentPairs(clusters: IndexedSeq[Set[DSpan]]): IndexedSeq[(DSpan, DSpan)] = {
    for {
      cluster <- clusters
      cc = cluster.toIndexedSeq
      i <- 0 until cc.length
      a = cc(i)
      j <- 0 until i
      b = cc(j)
    } yield (a -> b)
  }
}
