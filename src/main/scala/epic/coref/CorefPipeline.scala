package epic.coref

import java.io.File
import breeze.config.{Configuration, CommandLineParser}
import epic.ontonotes.ConllOntoReader
import breeze.linalg.{DenseVector, Counter}
import epic.framework.{ModelObjective, Feature}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.optimize.RandomizedGradientCheckingFunction

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
  val indexed = PairwiseIndexer(feat, instances)

  val model = new PairwiseModel(feat, indexed.index)
  val obj = new ModelObjective(model, indexed.instances)



  println(indexed.index)
  val optimum =  OptParams(useStochastic = false).minimize(obj, DenseVector.zeros[Double](indexed.index.size))
  val inference = model.inferenceFromWeights(optimum)
  for(i <- indexed.instances) {
    println("=================")
    println("Text: ")
    i.unindexed.words foreach { s => println(s.mkString(" "))}
    println("Gold: ")
    for(cluster <- i.unindexed.clusters) {
      println(cluster.mkString(", "))
    }
    println("Guess: ")
    for(cluster <- inference.decode(i)) {
      println(cluster.mkString(", "))
    }
  }


}
