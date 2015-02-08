package epic.corefdense

import edu.berkeley.nlp.futile.util.Logger
import edu.berkeley.nlp.futile.fig.basic.SysInfoUtils
import java.util.Arrays
import scala.util.Random
import edu.berkeley.nlp.futile.math.CachingDifferentiableFunction
import edu.berkeley.nlp.futile.math.LBFGSMinimizer

/**
 * N.B. REPURPOSED TO USE DOUBLES
 */
trait LikelihoodAndGradientComputer[T] {
  
  def getInitialWeights(initialWeightsScale: Double): Array[Double]
  
  /**
   * Accumulates the gradient on this example into gradient and returns the log likelihood
   * of this example
   */
  def accumulateGradientAndComputeObjective(ex: T, weights: Array[Double], gradient: Array[Double]): Double;
  
  /**
   * Just computes the objective; lighter-weight method that clients may want to implement
   * more efficiently
   */
  def computeObjective(ex: T, weights: Array[Double]): Double;
  
  /**
   * Allows for modification of the weights to do things like clipping or printing
   */
  def weightsUpdateCallback(weights: Array[Double]) = {}
  
  /**
   * Allows for modification of the weights to do things like clipping or printing
   */
  def iterationEndCallback(weights: Array[Double]) = {}
}

class GeneralTrainer[T](val parallel: Boolean = false) {
  
  var inferenceNanos = 0L;
  var adagradNanos = 0L;
  
//  def train(trainExs: Seq[T],
//            computer: LikelihoodAndGradientComputer[T],
//            eta: Double,
//            reg: Double,
//            batchSize: Int,
//            numItrs: Int,
//            numFeats: Int,
//            verbose: Boolean): Array[Double] = {
//    trainAdagrad(trainExs, computer, eta, reg, batchSize, numItrs, Array.fill(numFeats)(0.0), verbose);
//  }
//  
//  def train(trainExs: Seq[T],
//            computer: LikelihoodAndGradientComputer[T],
//            eta: Double,
//            reg: Double,
//            batchSize: Int,
//            numItrs: Int,
//            initialWeights: Array[Double],
//            verbose: Boolean): Array[Double] = {
//    trainAdagrad(trainExs, computer, eta, reg, batchSize, numItrs, initialWeights, verbose);
//  }

  def trainAdagrad(trainExs: Seq[T],
                   computer: LikelihoodAndGradientComputer[T],
                   eta: Double,
                   lambda: Double,
                   batchSize: Int,
                   numItrs: Int,
                   initialWeights: Array[Double],
                   verbose: Boolean = true): Array[Double] = {
//    val weights = Array.fill(pairwiseIndexingFeaturizer.featureIndexer.size)(0.0);
    val weights = initialWeights;
    val reusableGradientArray = Array.fill(initialWeights.size)(0.0);
    val diagGt = Array.fill(initialWeights.size)(0.0);
    for (i <- 0 until numItrs) {
      Logger.logss("ITERATION " + i);
      val startTime = System.nanoTime();
      inferenceNanos = 0;
      adagradNanos = 0;
      if (verbose) Logger.startTrack("Computing gradient");
      var cumulativeObjective = 0.0
      var currIdx = 0;
      var currBatchIdx = 0;
      val printFreq = (trainExs.size / batchSize) / 10 // Print progress 10 times per pass through the data
      while (currIdx < trainExs.size) {
        if (verbose && (printFreq == 0 || currBatchIdx % printFreq == 0)) {
          Logger.logs("Computing gradient on " + currIdx + " (batch " + currBatchIdx + " / " + (trainExs.size / batchSize) + ")");
        }
        cumulativeObjective += takeAdagradStepL1R(trainExs.slice(currIdx, Math.min(trainExs.size, currIdx + batchSize)),
                                                  computer,
                                                  weights,
                                                  reusableGradientArray,
                                                  diagGt,
                                                  eta,
                                                  lambda);
        computer.weightsUpdateCallback(weights)
        currIdx += batchSize;
        currBatchIdx += 1;
      }
      for (weight <- weights) {
        cumulativeObjective -= lambda * Math.abs(weight);
      }
      Logger.logss("APPROXIMATE OBJECTIVE: " + cumulativeObjective + " (avg = " + cumulativeObjective/trainExs.size + ")")
      if (verbose) {
        Logger.endTrack();
        displayWeightsAndTime(i, weights, startTime, inferenceNanos, adagradNanos)
      }
      computer.iterationEndCallback(weights)
    }
    if (verbose) {
      Logger.logss("FINAL TRAIN OBJECTIVE: " + computeObjectiveL1R(trainExs, computer, weights, lambda));
    }
    weights
  }
  
  def displayWeightsAndTime(iter: Int, weights: Array[Double], startTime: Long, inferenceNanos: Long, adagradNanos: Long) {
    Logger.logss("NONZERO WEIGHTS: " + weights.foldRight(0)((weight, count) => if (Math.abs(weight) > 1e-15) count + 1 else count));
    Logger.logss("WEIGHT VECTOR NORM: " + weights.foldRight(0.0)((weight, norm) => norm + weight * weight));
    Logger.logss("MILLIS FOR ITER " + iter + ": " + (System.nanoTime() - startTime) / 1000000.0 +
              " (" + inferenceNanos / 1000000.0 + " for inference and " + adagradNanos / 1000000.0 + " for Adagrad)");
    Logger.logss("MEMORY AFTER ITER " + iter + ": " + SysInfoUtils.getUsedMemoryStr());
  }
  
  def computeObjectiveL1R(trainExs: Seq[T],
                          computer: LikelihoodAndGradientComputer[T],
                          weights: Array[Double],
                          lambda: Double): Double = {
//    var objective = trainExs.map(computer.computeObjective(_, weights)).reduce(_ + _);
    var objective = (if (parallel) trainExs.par else trainExs).aggregate(0.0)((currLL, ex) => currLL + computer.computeObjective(ex, weights), _ + _)
    objective + computeRegularizationTermL1R(weights, lambda)
  }
  
  def computeRegularizationTermL1R(weights: Array[Double], lambda: Double): Double = {
    var regTerm = 0.0
    for (weight <- weights) {
      regTerm -= lambda * Math.abs(weight);
    }
    regTerm;
  }
  
  def getMinibatchObjectiveAndGradient(exs: Seq[T], computer: LikelihoodAndGradientComputer[T], weights: Array[Double], gradientArray: Array[Double]) = {
    var nanoTime = System.nanoTime();
    val objective = if (parallel) {
      parallelGetMinibatchObjectiveAndGradient(exs, computer, weights, gradientArray)
    } else {
      serialGetMinibatchObjectiveAndGradient(exs, computer, weights, gradientArray)
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    objective
  }
  
  private def serialGetMinibatchObjectiveAndGradient(exs: Seq[T], computer: LikelihoodAndGradientComputer[T], weights: Array[Double], gradientArray: Array[Double]) = {
    var objective = 0.0
    for (ex <- exs) {
      // Don't need to rescale objective here since it's not used in the update
      objective += computer.accumulateGradientAndComputeObjective(ex, weights, gradientArray);
    }
    objective
  }
  
//  case class SuffStats(val ll: Double, val gradient: Array[Double]) {
//    def +(other: SuffStats) = new SuffStats(ll + other.ll, Array.tabulate(gradient.size)(i => gradient(i) + other.gradient(i)))
//  }
  
  case class SuffStats(var ll: Double, val gradient: Array[Double]) {
    
    def incrementLL(increment: Double) { ll += increment }
    
    def +=(other: SuffStats) = {
      ll += other.ll
      var i = 0
      while (i < gradient.size) {
        gradient(i) += other.gradient(i)
        i += 1
      }
      this
    }
  }
  
  def parallelGetMinibatchObjectiveAndGradient(exs: Seq[T], computer: LikelihoodAndGradientComputer[T], weights: Array[Double], gradientArray: Array[Double]) = {
    val emptySS = new SuffStats(0.0, Array.tabulate(gradientArray.size)(i => 0.0))
//    val finalSS = exs.aggregate(null: SuffStats)((currSS, ex) => {
    val finalSS = exs.par.aggregate(null: SuffStats)((currSS, ex) => {
      val ss = if (currSS ne null) currSS else new SuffStats(0.0, Array.tabulate(gradientArray.size)(i => 0.0))
//      val ss = if (currSS ne null) currSS else new SuffStats(0.0, Array.tabulate(gradientArray.size)(i => 0.0))
      val ll = computer.accumulateGradientAndComputeObjective(ex, weights, ss.gradient);
      ss.incrementLL(ll)
      ss
    }, { (a, b) => if (a eq null) b else if (b eq null) a else b += a })
    System.arraycopy(finalSS.gradient, 0, gradientArray, 0, gradientArray.size)
    finalSS.ll
  }

  def takeAdagradStepL1R(exs: Seq[T],
                         computer: LikelihoodAndGradientComputer[T],
                         weights: Array[Double],
                         reusableGradientArray: Array[Double],
                         diagGt: Array[Double],
                         eta: Double,
                         lambda: Double): Double = {
    Arrays.fill(reusableGradientArray, 0.0);
    val objective = getMinibatchObjectiveAndGradient(exs, computer, weights, reusableGradientArray)
    val nanoTime = System.nanoTime();
//    var nanoTime = System.nanoTime();
//    var objective = 0.0
//    for (ex <- exs) {
//      objective += computer.accumulateGradientAndComputeObjective(ex, weights, reusableGradientArray);
//    }
//    inferenceNanos += (System.nanoTime() - nanoTime);
//    nanoTime = System.nanoTime();
    
    // Precompute this so dividing by batch size is a multiply and not a divide
    val batchSizeMultiplier = 1.0/exs.size;
    var i = 0;
    while (i < reusableGradientArray.size) {
      val xti = weights(i);
      // N.B. We negate the gradient here because the Adagrad formulas are all for minimizing
      // and we're trying to maximize, so think of it as minimizing the negative of the objective
      // which has the opposite gradient
      // Equation (25) in http://www.cs.berkeley.edu/~jduchi/projects/DuchiHaSi10.pdf
      // eta is the step size, lambda is the regularization
      val gti = -reusableGradientArray(i) * batchSizeMultiplier;
      // Update diagGt
      diagGt(i) += gti * gti;
      val Htii = 1 + Math.sqrt(diagGt(i)).toDouble;
      // Avoid divisions at all costs...
      val etaOverHtii = eta / Htii;
      val newXti = xti - etaOverHtii * gti;
      weights(i) = Math.signum(newXti) * Math.max(0, Math.abs(newXti) - lambda * etaOverHtii);
      i += 1;
    }
    adagradNanos += (System.nanoTime() - nanoTime);
    objective
  }
  
  
  
  
  def trainAdadelta(trainExs: Seq[T],
                    computer: LikelihoodAndGradientComputer[T],
                    rho: Double,
                    batchSize: Int,
                    numItrs: Int,
                    initialWeights: Array[Double],
                    verbose: Boolean = true): Array[Double] = {
//    val weights = Array.fill(pairwiseIndexingFeaturizer.featureIndexer.size)(0.0);
    val weights = initialWeights;
    val reusableGradientArray = Array.fill(initialWeights.size)(0.0);
    val gradsSquared = Array.fill(initialWeights.size)(0.0);
    val updatesSquared = Array.fill(initialWeights.size)(0.0);
    for (i <- 0 until numItrs) {
      Logger.logss("ITERATION " + i);
      val startTime = System.nanoTime();
      inferenceNanos = 0;
      adagradNanos = 0;
      if (verbose) Logger.startTrack("Computing gradient");
      var cumulativeObjective = 0.0
      var currIdx = 0;
      var currBatchIdx = 0;
      val printFreq = (trainExs.size / batchSize) / 10 // Print progress 10 times per pass through the data
      while (currIdx < trainExs.size) {
        if (verbose && (printFreq == 0 || currBatchIdx % printFreq == 0)) {
          Logger.logs("Computing gradient on " + currIdx + " (batch " + currBatchIdx + " / " + (trainExs.size / batchSize) + ")");
        }
        cumulativeObjective += takeAdadeltaStep(trainExs.slice(currIdx, Math.min(trainExs.size, currIdx + batchSize)),
                                                computer,
                                                weights,
                                                reusableGradientArray,
                                                gradsSquared,
                                                updatesSquared,
                                                rho);
        computer.weightsUpdateCallback(weights)
        currIdx += batchSize;
        currBatchIdx += 1;
      }
      Logger.logss("APPROXIMATE OBJECTIVE: " + cumulativeObjective + " (avg = " + cumulativeObjective/trainExs.size + ")")
      if (verbose) {
        Logger.endTrack();
        displayWeightsAndTime(i, weights, startTime, inferenceNanos, adagradNanos)
      }
      computer.iterationEndCallback(weights)
    }
    if (verbose) {
      Logger.logss("FINAL TRAIN OBJECTIVE: " + computeObjectiveL1R(trainExs, computer, weights, 0.0));
    }
    weights
  }
  
  
  def takeAdadeltaStep(exs: Seq[T],
                       computer: LikelihoodAndGradientComputer[T],
                       weights: Array[Double],
                       reusableGradientArray: Array[Double],
                       gradsSquared: Array[Double],
                       updatesSquared: Array[Double],
                       rho: Double): Double = {
    Arrays.fill(reusableGradientArray, 0.0);
    val objective = getMinibatchObjectiveAndGradient(exs, computer, weights, reusableGradientArray)
    val nanoTime = System.nanoTime();
//    var nanoTime = System.nanoTime();
//    var objective = 0.0
//    for (ex <- exs) {
//      // Don't need to rescale objective here since it's not used in the update
//      objective += computer.accumulateGradientAndComputeObjective(ex, weights, reusableGradientArray);
//    }
//    inferenceNanos += (System.nanoTime() - nanoTime);
//    nanoTime = System.nanoTime();
    val epsilon = 1e-6
    val inverseBatchSize = 1.0/exs.size
    var i = 0
    while (i < reusableGradientArray.size) {
      // Rescale the gradient by the batch size
      reusableGradientArray(i) *= inverseBatchSize
      gradsSquared(i) = rho * gradsSquared(i) + (1 - rho) * reusableGradientArray(i) * reusableGradientArray(i)
      val step = Math.sqrt(updatesSquared(i) + epsilon)/Math.sqrt(gradsSquared(i) + epsilon) * reusableGradientArray(i)
      updatesSquared(i) = rho * updatesSquared(i) + (1 - rho) * step * step
      weights(i) += step
      i += 1
    }
    adagradNanos += (System.nanoTime() - nanoTime);
    objective
  }

  def takeSGDStep(exs: Seq[T],
                  computer: LikelihoodAndGradientComputer[T],
                  weights: Array[Double],
                  reusableGradientArray: Array[Double],
                  eta: Double): Double = {
    Arrays.fill(reusableGradientArray, 0.0);
    var nanoTime = System.nanoTime();
    var objective = 0.0
    for (ex <- exs) {
      objective += computer.accumulateGradientAndComputeObjective(ex, weights, reusableGradientArray);
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    nanoTime = System.nanoTime();
    // Precompute this so dividing by batch size is a multiply and not a divide
    val coeff = eta / exs.size;
    var i = 0;
    while (i < reusableGradientArray.size) {
      weights(i) += coeff * (reusableGradientArray(i) - 2 * weights(i))
    }
    adagradNanos += (System.nanoTime() - nanoTime);
    objective
  }
  
  def trainLBFGS(trainExs: Seq[T],
                 computer: LikelihoodAndGradientComputer[T],
                 lambda: Double,
                 epsilon: Double,
                 numItrs: Int,
                 initialWeights: Array[Double],
                 verbose: Boolean = true): Array[Double] = {
    val diffFunc = new CachingDifferentiableFunction() {
      val reusableGradientArr = Array.tabulate(initialWeights.size)(i => 0.0)
      
      def dimension(): Int = initialWeights.size
      
//      protected[edu.berkeley.nlp.futile.math] calculate(currWeights: Array[Double]): edu.berkeley.nlp.futile.fig.basic.Pair[Double,Array[Double]] = {
      import java.lang.{Double => JDouble}
      def calculate(currWeights: Array[Double]): edu.berkeley.nlp.futile.fig.basic.Pair[JDouble,Array[Double]] = {
//        val sweights = Array.tabulate(currWeights.size)(i => currWeights(i).asInstanceOf[Double])
        val nanoTime = System.nanoTime();
        var objective = 0.0;
        Arrays.fill(reusableGradientArr, 0.0);
        for (ex <- trainExs) {
          objective += computer.accumulateGradientAndComputeObjective(ex, currWeights, reusableGradientArr)
//          objective += computer.accumulateGradientAndComputeObjective(ex, sweights, reusableGradientArr)
        }
        for (i <- 0 until reusableGradientArr.size) {
          objective -= lambda * currWeights(i) * currWeights(i);
          reusableGradientArr(i) -= 2 * lambda * currWeights(i)
          reusableGradientArr(i) = -reusableGradientArr(i)
        }
        val negObjective = -objective;
        var norm = 0.0
        for (j <- 0 until currWeights.size) {
          norm += currWeights(j) * currWeights(j);
        }
        Logger.logss("NORM OF WEIGHTS: " + norm);
        Logger.logss("OBJECTIVE: " + objective + " (avg = " + objective/trainExs.size + ")")
        Logger.logss("TRAIN MILLIS: "+  (System.nanoTime() - nanoTime)/1000000);
        new edu.berkeley.nlp.futile.fig.basic.Pair[JDouble,Array[Double]](new JDouble(negObjective), reusableGradientArr);
      }
    }
    new LBFGSMinimizer(numItrs).minimize(diffFunc, initialWeights, epsilon, true);
  }
}

object GeneralTrainer {
  
  def main(args: Array[String]) {
    
  }
  
  
//def sgd_updates_adadelta(params,cost,rho=0.95,epsilon=1e-6,norm_lim=9):
//    """
//    adadelta update rule, mostly from
//    https://groups.google.com/forum/#!topic/pylearn-dev/3QbKtCumAW4 (for Adadelta)
//    """
//    updates = OrderedDict({})
//    exp_sqr_grads = OrderedDict({})
//    exp_sqr_ups = OrderedDict({})
//    gparams = []
//    for param in params:
//        empty = numpy.zeros_like(param.get_value())
//        exp_sqr_grads[param] = theano.shared(value=as_floatX(empty),name="exp_grad_%s" % param.name)
//        gp = T.grad(cost, param)
//        exp_sqr_ups[param] = theano.shared(value=as_floatX(empty), name="exp_grad_%s" % param.name)
//        gparams.append(gp)
//    for param, gp in zip(params, gparams):
//        exp_sg = exp_sqr_grads[param]
//        exp_su = exp_sqr_ups[param]
//        up_exp_sg = rho * exp_sg + (1 - rho) * T.sqr(gp)
//        updates[exp_sg] = up_exp_sg
//        step =  -(T.sqrt(exp_su + epsilon) / T.sqrt(up_exp_sg + epsilon)) * gp
//        updates[exp_su] = rho * exp_su + (1 - rho) * T.sqr(step)
//        stepped_param = param + step
//        if (param.get_value(borrow=True).ndim == 2) and (param.name!='Words'):
//            col_norms = T.sqrt(T.sum(T.sqr(stepped_param), axis=0))
//            desired_norms = T.clip(col_norms, 0, T.sqrt(norm_lim))
//            scale = desired_norms / (1e-7 + col_norms)
//            updates[param] = stepped_param * scale
//        else:
//            updates[param] = stepped_param      
//    return updates
  
  
  def checkGradient[T](trainExs: Seq[T],
                       computer: LikelihoodAndGradientComputer[T],
                       numFeats: Int,
                       indexSet: Set[Int] = Set(),
                       verbose: Boolean = false) {
    val rng = new Random(0);
    val randWeights = Array.tabulate(numFeats)(i => (rng.nextDouble * 0.1).toDouble)
    if (numFeats < 100) {
      Logger.logss("Weights: " + randWeights.toSeq);
    }
    checkGradientFromPoint(trainExs, computer, randWeights, Array.tabulate(numFeats)(i => 0.0), indexSet, verbose);
  }
  
  def checkGradientFromPoint[T](trainExs: Seq[T],
                                computer: LikelihoodAndGradientComputer[T],
                                weights: Array[Double],
                                gradient: Array[Double],
                                indexSet: Set[Int] = Set(),
                                verbose: Boolean = false) {
    var currLL = trainExs.map(ex => computer.accumulateGradientAndComputeObjective(ex, weights, gradient)).reduce(_ + _);
    Logger.logss("Base LL: " + currLL)
    val stepSize = 1e-3F;
    // If we've restricted to an index set, only check those, otherwise check all indices
    val indicesToCheck = if (indexSet.isEmpty) 0 until gradient.size else indexSet.toSeq.sorted;
    for (i <- indicesToCheck) {
      if (i % 1000 == 0) {
        Logger.logss("Checking empirical gradient on weight " + i);
      }
      weights(i) += stepSize;
      var newLL: Double = trainExs.map(computer.computeObjective(_, weights)).reduce(_ + _)
      val empGradient = (newLL - currLL)/stepSize;
      if (Math.abs(empGradient - gradient(i)) > 1e-3) {
        Logger.logss("Difference on feature " + i + ": gradient: " + gradient(i) + ", emp gradient: " + empGradient);
      } else if (verbose) {
        Logger.logss("On feature " + i + ": gradient: " + gradient(i) + ", emp gradient: " + empGradient);
      }
      weights(i) -= stepSize;
    }
  }
}
