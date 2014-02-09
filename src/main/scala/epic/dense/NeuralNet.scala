package epic.dense
import epic.framework.{Feature, StandardExpectedCounts}
import breeze.util.Index
import breeze.linalg._
import breeze.stats.distributions.RandBasis
import breeze.numerics.{exp, log}
import java.io.{FileInputStream, File}
import breeze.optimize.FirstOrderMinimizer.OptParams
import breeze.config.{Configuration, CommandLineParser}
import com.typesafe.scalalogging.slf4j.Logging
import scala.io.Source
import java.util.zip.GZIPInputStream
import epic.dense.NeuralNet.ClassificationInstance
import breeze.features.FeatureVector

/**
 * TODO
 *
 * @author dlwh
 **/
object NeuralNet {
  case class ClassificationInstance[Features](label: Int, inputs: Features)
  case class Model[Label, Features](labelIndex: Index[Label], transform: Transform[Features, DenseVector[Double]])(implicit basis: RandBasis = RandBasis.mt0)  extends StandardExpectedCounts.Model[ClassificationInstance[Features]] {
    def featureIndex: Index[Feature] = transform.index

    type Marginal = NeuralNet.Marginal
    type Inference = NeuralNet.Inference[Label, Features]
    type Scorer = NeuralNet.Scorer[Features]

    def initialValueForFeature(f: Feature): Double = basis.uniform.draw() * 1E-1 - 0.05

    def inferenceFromWeights(weights: DenseVector[Double]): Inference =  {
      val layer = transform.extractLayer(weights)

      new Inference(labelIndex, layer)
    }

    def accumulateCounts(s: Scorer, d: ClassificationInstance[Features], m: Marginal, accum: ExpectedCounts, scale: Double): Unit = {
      s.layer.tallyDerivative(accum.counts, (m.counts * scale).toDenseVector, d.inputs)
      accum.loss += scale * m.logPartition
    }
  }

  case class Inference[Label, Features](labelIndex: Index[Label], layer: Transform[Features, DenseVector[Double]]#Layer) extends epic.framework.Inference[ClassificationInstance[Features]] {
    type Marginal = NeuralNet.Marginal
    type Scorer = NeuralNet.Scorer[Features]


    def scorer(v: ClassificationInstance[Features]): Scorer = {
      new Scorer(layer, layer.activations(v.inputs))
    }


    def goldMarginal(scorer: Scorer, v: ClassificationInstance[Features]): Marginal = {
      Marginal(SparseVector(labelIndex.size)(v.label -> 1.0), scorer.scores(v.label))
    }

    def marginal(scorer: Scorer, v: ClassificationInstance[Features]): Marginal = {
      val logNormalizer = softmax(scorer.scores)
      val normalized = scorer.scores - logNormalizer
      exp.inPlace(normalized)
      Marginal(normalized, logNormalizer)
    }
  }

  case class Marginal(counts: Vector[Double], logPartition: Double) extends epic.framework.Marginal

  class Scorer[Features](val layer: Transform[Features, DenseVector[Double]]#Layer, _scores: => DenseVector[Double]) {
    lazy val scores = _scores
  }

}


object DevlinLM extends Logging {
  case class Options(path: File, numTrain: Int = Int.MaxValue, opt: OptParams, numHidden: Int = 200, embedDim: Int = 100)

  def main(args: Array[String]):Unit = {
    val params = CommandLineParser.readIn[Options](args)
    logger.info("Command line arguments for recovery:\n" + Configuration.fromObject(params).toCommandLineString)
    import params._

    val inputIndex = Index[String](Source.fromFile(new File(path, "input_vocab.txt")).getLines().map(_.trim))
    val labelIndex = Index[String](Source.fromFile(new File(path, "label_vocab.txt")).getLines().map(_.trim))
    logger.info(s"There are ${inputIndex.size} input types and ${labelIndex.size} output types")

    val dev = {
      val source = new FileInputStream(new File(path, "sample_ids.val.txt"))
      readInstances(Source.fromInputStream(source, "UTF-8")).toIndexedSeq
    }

    val numInputs = dev.head.inputs.data.length
    logger.info(s"There are $numInputs inputs to each instance.")

    val transform = new AffineTransform(labelIndex.size, numHidden, new SigmoidTransform(new AffineTransform(numHidden, numInputs * embedDim, new SigmoidTransform(new DevlinTransform(inputIndex, embedDim)))))

    val train = {
      val input = new GZIPInputStream(new FileInputStream(new File(path, "sample_ids.train.txt.gz")))
      readInstances(Source.fromInputStream(input, "UTF-8")).take(numTrain).toIndexedSeq
    }

    val test = {
      val test = new FileInputStream(new File(path, "sample_ids.test.txt"))
      readInstances(Source.fromInputStream(test, "UTF-8")).toIndexedSeq
    }



  }

  def readInstances(source: Source): Iterator[ClassificationInstance[FeatureVector]] = {
    for(Seq(inputs, label) <- source.getLines().grouped(2)) yield {
      val x = inputs.split("\\s+").map(_.toInt)
      NeuralNet.ClassificationInstance(label.toInt, new FeatureVector(x))
    }
  }
}