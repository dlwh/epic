package epic.coref

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import breeze.linalg.{DenseVector, Counter}
import epic.framework.{ModelObjective, Feature}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.RandomizedGradientCheckingFunction
import breeze.util.Encoder
import collection.immutable.IndexedSeq
import breeze.stats.ContingencyStats
import pairwise._

/**
 *
 * @author dlwh
 */

object CorefPipeline extends App {
  case class Params(path: File)
  val (baseConfig, files) = CommandLineParser.parseArguments(args)
  val config = baseConfig backoff Configuration.fromPropertiesFiles(files.map(new File(_)))
  val params = config.readIn[Params]("")
  val instances = for {
    file <- params.path.listFiles take 100
    doc <- ConllOntoReader.readDocuments(file)
  } yield CorefInstance.fromDocument(doc)



  val feat = new SimplePairwiseFeaturizer
  val extractors = Properties.allExtractors
  val indexed = PropIndexer(feat, extractors, instances)

  /*
  val model = new PairwiseModel(feat, indexed.index)
  val obj = new ModelObjective(model, indexed.instances)



  println(indexed.index)
  val optimum =  OptParams(useStochastic = false).minimize(obj, DenseVector.zeros[Double](indexed.index.size))
  val inference = model.inferenceFromWeights(optimum)


  var allResults = Stats(0, 0, 0)


  for(i <- indexed.instances) {
    println("=================")
    println("Text: ")
    i.unindexed.words foreach { s => println(s.mkString(" "))}
    println("Gold: ")
    for(cluster <- sortClusters(i.unindexed.clusters)) {
      println(formatCluster(cluster))
    }
    println("Guess: ")
    val guess = inference.decode(i, forceMergeSingletons = false)
    for(cluster <- sortClusters(guess)) {
      println(formatCluster(cluster))
    }

    val results = eval(i.unindexed.clusters.toIndexedSeq, guess)
    println(results)
    allResults += results
  }
  println("=================")

  println("Weights: ")
  for( (f,v) <- Encoder.fromIndex(indexed.index).decode(optimum).iterator.toIndexedSeq[(Feature, Double)].sortBy(_._2)) {
    println(v + " " + f)
  }
  println("=================")
  println(allResults)

  case class Stats(numInter: Int, numGold: Int, numGuess: Int) {
    def +(stats: Stats) = Stats(numInter + stats.numInter, numGold + stats.numGold, numGuess + stats.numGuess)

    def precision = if(numInter == 0.0) 0.0 else numInter.toDouble / numGuess
    def recall = if(numInter == 0.0) 0.0 else numInter.toDouble / numGold
    def f1 = 2.0 / (1.0/precision + 1.0/recall)

    override def toString() = {
      "Stats( f1 = " + f1 + ", precision = " + precision + ", recall = "  + recall +")"
    }
  }

  def eval(gold: IndexedSeq[Set[MentionCandidate]], guess: IndexedSeq[Set[MentionCandidate]]) = {
    val pairsGold = getCoreferentPairs(gold)
    val pairsGuess = getCoreferentPairs(guess)

    Stats(pairsGold.toSet & pairsGuess.toSet size, pairsGold.size, pairsGuess.size)
  }

  def sortClusters(clusters: Iterable[Set[MentionCandidate]]):IndexedSeq[Set[MentionCandidate]] = {
    clusters.toIndexedSeq[Set[MentionCandidate]].sortBy(_.min)
  }

  def formatCluster(set: Set[MentionCandidate]) = {
    set.toIndexedSeq[MentionCandidate].sorted.mkString(", ")
  }

  def getCoreferentPairs(clusters: IndexedSeq[Set[MentionCandidate]]): IndexedSeq[(MentionCandidate, MentionCandidate)] = {
    for {
      cluster <- clusters
      cc = cluster.toIndexedSeq
      i <- 0 until cc.length
      a = cc(i)
      j <- 0 until i
      b = cc(j)
    } yield (a -> b)
  }
  */
}
