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
}

class GeneralTrainer[T] {
  
  var inferenceNanos = 0L;
  var adagradNanos = 0L;
  
  def train(trainExs: Seq[T],
            computer: LikelihoodAndGradientComputer[T],
            eta: Double,
            reg: Double,
            batchSize: Int,
            numItrs: Int,
            numFeats: Int,
            verbose: Boolean): Array[Double] = {
    trainAdagrad(trainExs, computer, eta, reg, batchSize, numItrs, Array.fill(numFeats)(0.0), verbose);
  }
  
  def train(trainExs: Seq[T],
            computer: LikelihoodAndGradientComputer[T],
            eta: Double,
            reg: Double,
            batchSize: Int,
            numItrs: Int,
            initialWeights: Array[Double],
            verbose: Boolean): Array[Double] = {
    trainAdagrad(trainExs, computer, eta, reg, batchSize, numItrs, initialWeights, verbose);
  }

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
        if (verbose && currBatchIdx % printFreq == 0) {
          Logger.logs("Computing gradient on " + currIdx + " (batch " + currBatchIdx + " / " + (trainExs.size / batchSize) + ")");
        }
        cumulativeObjective += takeAdagradStepL1R(trainExs.slice(currIdx, Math.min(trainExs.size, currIdx + batchSize)),
                                                  computer,
                                                  weights,
                                                  reusableGradientArray,
                                                  diagGt,
                                                  eta,
                                                  lambda);
        currIdx += batchSize;
        currBatchIdx += 1;
      }
      for (weight <- weights) {
        cumulativeObjective -= lambda * Math.abs(weight);
      }
      Logger.logss("APPROXIMATE OBJECTIVE: " + cumulativeObjective + " (avg = " + cumulativeObjective/trainExs.size + ")")
      if (verbose) {
        Logger.endTrack();
        Logger.logss("NONZERO WEIGHTS: " + weights.foldRight(0)((weight, count) => if (Math.abs(weight) > 1e-15) count + 1 else count));
        Logger.logss("WEIGHT VECTOR NORM: " + weights.foldRight(0.0)((weight, norm) => norm + weight * weight));
        Logger.logss("MILLIS FOR ITER " + i + ": " + (System.nanoTime() - startTime) / 1000000.0 +
                     " (" + inferenceNanos / 1000000.0 + " for inference and " + adagradNanos / 1000000.0 + " for Adagrad)");
        Logger.logss("MEMORY AFTER ITER " + i + ": " + SysInfoUtils.getUsedMemoryStr());
      }
    }
    if (verbose) {
      Logger.logss("FINAL TRAIN OBJECTIVE: " + computeObjectiveL1R(trainExs, computer, weights, lambda));
    }
    weights
  }
  
  def computeObjectiveL1R(trainExs: Seq[T],
                          computer: LikelihoodAndGradientComputer[T],
                          weights: Array[Double],
                          lambda: Double): Double = {
    var objective = trainExs.map(computer.computeObjective(_, weights)).reduce(_ + _);
    objective + computeRegularizationTermL1R(weights, lambda)
  }
  
  def computeRegularizationTermL1R(weights: Array[Double], lambda: Double): Double = {
    var regTerm = 0.0
    for (weight <- weights) {
      regTerm -= lambda * Math.abs(weight);
    }
    regTerm;
  }

  def takeAdagradStepL1R(exs: Seq[T],
                         computer: LikelihoodAndGradientComputer[T],
                         weights: Array[Double],
                         reusableGradientArray: Array[Double],
                         diagGt: Array[Double],
                         eta: Double,
                         lambda: Double): Double = {
    Arrays.fill(reusableGradientArray, 0.0);
    var nanoTime = System.nanoTime();
    var objective = 0.0
    for (ex <- exs) {
      objective += computer.accumulateGradientAndComputeObjective(ex, weights, reusableGradientArray);
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    nanoTime = System.nanoTime();
    // Precompute this so dividing by batch size is a multiply and not a divide
    val batchSizeMultiplier = 1.0F/exs.size;
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
      val Htii = 1F + Math.sqrt(diagGt(i)).toDouble;
      // Avoid divisions at all costs...
      val etaOverHtii = eta / Htii;
      val newXti = xti - etaOverHtii * gti;
      weights(i) = Math.signum(newXti) * Math.max(0, Math.abs(newXti) - lambda * etaOverHtii);
      i += 1;
    }
    adagradNanos += (System.nanoTime() - nanoTime);
    objective
  }
  
  
  def takeAdadeltaStepL2R(exs: Seq[T],
                          computer: LikelihoodAndGradientComputer[T],
                          weights: Array[Double],
                          reusableGradientArray: Array[Double],
                          diagGt: Array[Double],
                          eta: Double,
                          lambda: Double): Double = {
    Arrays.fill(reusableGradientArray, 0.0);
    var nanoTime = System.nanoTime();
    var objective = 0.0
    for (ex <- exs) {
      objective += computer.accumulateGradientAndComputeObjective(ex, weights, reusableGradientArray);
    }
    inferenceNanos += (System.nanoTime() - nanoTime);
    nanoTime = System.nanoTime();
    // Precompute this so dividing by batch size is a multiply and not a divide
    val batchSizeMultiplier = 1.0F/exs.size;
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
      val Htii = 1F + Math.sqrt(diagGt(i)).toDouble;
      // Avoid divisions at all costs...
      val etaOverHtii = eta / Htii;
      val newXti = xti - etaOverHtii * gti;
      weights(i) = Math.signum(newXti) * Math.max(0, Math.abs(newXti) - lambda * etaOverHtii);
      i += 1;
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
  
  
//  public void trainWeightsLbfgsL2R(final List<? extends Example> exs,
//                                   final double reg,
//                                   final double epsilon,
//                                   final int numItrs,
//                                   final double[] weights) {
//    CachingDifferentiableFunction diffFunc = new CachingDifferentiableFunction() {
//      
//      private final double[] reusableGradientArr = new double[weights.length];
//      
//      public int dimension() {
//        return weights.length;
//      }
//      
//      public Pair<Double, double[]> calculate(double[] currWeights) {
//        long nanoTime = System.nanoTime();
//        double objective = 0.0;
////        double[] gradient = new double[currWeights.length];
//        double[] gradient = reusableGradientArr;
//        Arrays.fill(gradient, 0.0);
//        for (Example ex : exs) {
//          objective += ex.computeLogLikelihood(currWeights);
//          ex.addUnregularizedStochasticGradient(currWeights, gradient);
//        }
//        if (shouldRunEmpiricalGradientCheck) {
//          checkGradient(exs, currWeights, gradient);
//        }
//        for (int i = 0; i < gradient.length; i++) {
//          objective -= reg * currWeights[i] * currWeights[i];
//          gradient[i] -= 2 * reg * currWeights[i];
//        }
//
//        double negObjective = -objective;
//        double[] negGradient = new double[currWeights.length];
//        for (int i = 0; i < gradient.length; i++) {
//          negGradient[i] = -gradient[i];
//        }
//        // Print some diagnostics
//        double norm = 0, nonzeroWeights = 0;
//        for (int j = 0; j < currWeights.length; j++) {
//          if (currWeights[j] != 0) {
//            nonzeroWeights++;
//          }
//          norm += currWeights[j] * currWeights[j];
//        }
//        Logger.logss("NONZERO WEIGHTS: " + nonzeroWeights);
//        Logger.logss("NORM OF WEIGHTS: " + norm);
//        Logger.logss("TRAIN OBJECTIVE: " + objective);
//        Logger.logss("TRAIN MILLIS: "+  (System.nanoTime() - nanoTime)/1000000);
//        return new Pair<Double, double[]>(negObjective, negGradient);
//      }
//    };
//    double[] finalWeights = new LBFGSMinimizer(numItrs).minimize(diffFunc, weights, epsilon, true);
//    for (int i = 0; i < finalWeights.length; i++) {
//      weights[i] = finalWeights[i];
//    }
//  }  
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
