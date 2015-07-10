package epic.framework

import collection.GenTraversable
import breeze.optimize.BatchDiffFunction
import breeze.linalg.DenseVector
import breeze.util.Encoder
import java.util.concurrent.atomic.AtomicInteger
import collection.parallel.ForkJoinTaskSupport
import concurrent.forkjoin.ForkJoinPool
import com.typesafe.scalalogging.slf4j.LazyLogging
import epic.util.{SafeLogging, CacheBroker}
import epic.trees.AnnotatedLabel
import epic.trees.TreeInstance

/**
 * The objective function for training a [[epic.framework.Model]]. Selects
 * a batch, creates an [[epic.framework.Inference]] object using the model,
 * computes expected counts using the inference, and then turns them into
 * the objective value.
 * @author dlwh
 */
class ModelObjective[Datum](val model: Model[Datum],
                            batchSelector: IndexedSeq[Int]=>GenTraversable[Datum],
                            val fullRange: IndexedSeq[Int]) extends BatchDiffFunction[DenseVector[Double]] with SafeLogging {
  def this(model: Model[Datum], data: IndexedSeq[Datum], numThreads: Int = -1) = this(model,ModelObjective.makePar(data, numThreads)(_), 0 until data.length)

  import model.{ExpectedCounts => _, _}

  type Builder = model.Inference

  // Selects a set of data to use
  protected def select(batch: IndexedSeq[Int]):GenTraversable[Datum] = batchSelector(batch)
  
  def initialWeightVector(randomize: Boolean): DenseVector[Double] = {
    initialWeightVector(randomize, 1E-3)
  }

  def initialWeightVector(randomize: Boolean, scale: Double): DenseVector[Double] = {
   val v = model.readCachedFeatureWeights() match {
     case Some(vector) => vector
     case None => Encoder.fromIndex(featureIndex).tabulateDenseVector(f => model.initialValueForFeature(f))
   }
    if(randomize) {
      // Control the seed of the RNG for the weights
      val rng = new scala.util.Random(0)
      v += DenseVector(Array.tabulate(numFeatures)(i => rng.nextDouble * 2.0 * scale - scale))
    }
    v
  }

  var timeSinceLastWrite = 0L
  var nextSave = 5L * 20 * 1000
  def calculate(x: DenseVector[Double], batch: IndexedSeq[Int]) = {
    if(timeSinceLastWrite > nextSave) {
      logger.info("Saving feature weights...")
      val timeIn = System.currentTimeMillis()
      model.cacheFeatureWeights(x)
      val writeLength = System.currentTimeMillis() - timeIn
      nextSave = math.max(writeLength * 100, 5L * 20 * 1000)// don't spend more than 1% of our time caching weights
      logger.info(f"Saving took ${writeLength/1000.0}%.2fs. Will write again in ${nextSave/1000.0}%.0fs")
      timeSinceLastWrite = 0
    }
    val inference = inferenceFromWeights(x)
    val timeIn = System.currentTimeMillis()
    val success = new AtomicInteger(0)
    val minibatch = select(batch)
    val finalCounts = minibatch.aggregate(null:model.ExpectedCounts)({ ( _countsSoFar,datum) =>
      try {
        val countsSoFar:model.ExpectedCounts = if (_countsSoFar ne null) _countsSoFar else emptyCounts
        model.accumulateCounts(inference, datum, countsSoFar, 1.0)
        success.incrementAndGet()
        countsSoFar
      } catch {
        case e: Exception =>
          e.printStackTrace()
//          new Exception("While processing " + datum, e).printStackTrace()
          _countsSoFar
      }
    },{ (a,b) => if(a eq null) b else if (b eq null) a else b += a})
    val timeOut = System.currentTimeMillis()
    timeSinceLastWrite += timeOut - timeIn
    logger.info(f"Inference took: ${(timeOut - timeIn) * 1.0/1000}%.3fs" )
    val (loss,grad) = expectedCountsToObjective(finalCounts)
    (loss/success.intValue() * fullRange.size,  grad * (fullRange.size * 1.0 / success.intValue))
  }
}

object ModelObjective {
  private def makePar[Datum](data: IndexedSeq[Datum], nThreads:Int)(indices: IndexedSeq[Int]) = {
    val xx =  indices.par.map(data)
    if (nThreads > 0)
      xx.tasksupport = new ForkJoinTaskSupport(new ForkJoinPool(nThreads))
    xx
  }
}
