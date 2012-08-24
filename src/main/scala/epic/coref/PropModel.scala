package epic.coref
import epic.framework._
import breeze.util.Index
import breeze.linalg._
import breeze.numerics._
import java.util
import breeze.inference.bp.{BeliefPropagation, Factor, Variable}
import breeze.collection.mutable.TriangularArray
import collection.mutable.ArrayBuffer

class PropModel(propIndexer: PropIndexer, val featureIndex: Index[Feature]) extends Model[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts
  type Inference = PropInference

  def initialValueForFeature(f: Feature) = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = new PropInference(propIndexer, weights)

  def emptyCounts = StandardExpectedCounts.zero(featureIndex)

  def expectedCountsToObjective(ecounts: ExpectedCounts) =  {
    ecounts.loss -> ecounts.counts
  }
}


class PropInference(propIndexer: PropIndexer, weights: DenseVector[Double]) extends GoldGuessInference[IndexedCorefInstance] {
  type ExpectedCounts = StandardExpectedCounts

  def goldCounts(inst: IndexedCorefInstance) = {
    var ll = Double.NegativeInfinity
    var ecounts:DenseVector[Double] = null
    var converged = false
    var iter = 0
    val maxIterations = 5


    while (!converged && iter < maxIterations) {
      val lastLL = ll
      ll = 0.0
      ecounts = DenseVector.zeros[Double](weights.size)

      for (cluster <- inst.goldClusters) {
        var isFirst = true
        for (i <- cluster) {
          if (isFirst) {
//            ll += computeScore(None, Some(props(i)), inst.featuresFor(0, i))
            ecounts += inst.featuresFor(0, i)
            isFirst = false
          } else {
            val scores = DenseVector.zeros[Double](i)
            java.util.Arrays.fill(scores.data, Double.NegativeInfinity)
            for (j <- cluster if j < i) {
//              scores(j) = computeScore(Some(props(i)), Some(props(j)), inst.featuresFor(i, j))
            }
            val sm = softmax(scores)
            ll += sm

            for (j <- cluster if j < i) {
              addIntoScale(ecounts, inst.featuresFor(j, i), math.exp(scores(j) - sm))
            }
          }
        }


      }

      converged = closeTo(lastLL, ll)
      iter += 1
    }

    new StandardExpectedCounts(ll, ecounts)
  }

  def guessCounts(inst: IndexedCorefInstance) = {
    val assignmentVariables: Array[Variable[Int]] = Array.tabulate(inst.numMentions + 1)(i => Variable(0 until i))
    val propertyVariables: Array[Array[Variable[Int]]] = Array.tabulate(inst.numMentions + 1, propIndexer.featuresForProperties.length){ (i, pi) =>
      if(i ==0) null
      else {
        val assignment =  inst.properties(i)(pi)
        if(assignment == -1)
          Variable(0 until propIndexer.featuresForProperties(pi).arity)
        else
          Variable(assignment to assignment)
      }
    }

    // skip factor for 0, since it doesn't get assigned.
    val assignmentFactors = Array.tabulate[Factor](inst.numMentions) { i_minus_1 =>
      assignmentFactor(assignmentVariables, i_minus_1 + 1, inst)
    }

    // this ends up being a little inefficient. We can actually
    // have 1 big factor with all properties of a given type,
    // and only visit certain assignments, but the bp framework
    // doesn't support that yet.
    // TODO: we can set contribution to exp(0) a prior and not need to sum over them... how to
    // do this?
    val agreementFactors = new ArrayBuffer[Factor]()
    for(i <- 1 to inst.numMentions; j <- 0 until i; p <- 0 until propIndexer.featuresForProperties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }

    val flattenedProp = propertyVariables.flatten

    val model = new breeze.inference.bp.Model(assignmentVariables ++ flattenedProp, assignmentFactors ++ agreementFactors)
    val bp = BeliefPropagation.infer(model)

    val ll = bp.logPartition

    // actually get the expected counts!
    val expCounts = DenseVector.zeros[Double](propIndexer.index.size)

    for(i_minus_1 <- 0 until assignmentFactors.length) {
      val i = i_minus_1 + 1
      val arr = Array(0)
      for(j <- 0 until i) {
        arr(0) = j
        addIntoScale(expCounts, inst.featuresFor(i, j), bp.beliefs(i)(j))
      }
    }

    // yuck
    val afIter = agreementFactors.iterator
    val ass = Array(0, 0, 0)
    for(i <- 1 to inst.numMentions; j <- 0 until i; p <- 0 until propIndexer.featuresForProperties.length) {
      val factor = afIter.next()
      ass(2) = j
      val marg = bp.factorMarginalFor(factor)
      for (p_j_ass <- 0 until propIndexer.featuresForProperties(p).arity;
           p_i_ass <- 0 until propIndexer.featuresForProperties(p).arity) {
        ass(0) = p_j_ass
        ass(1) = p_i_ass
        val prob = marg(ass)
        if(p_j_ass == p_i_ass)
          weights(propIndexer.featuresForProperties(p).agree) += prob
        else
          weights(propIndexer.featuresForProperties(p).mismatch) += prob

        weights(propIndexer.featuresForProperties(p).pairs(p_j_ass)(p_i_ass)) += prob
      }

    }


    new StandardExpectedCounts(ll, expCounts)
  }


  def assignmentFactor(assignmentVariables: Array[Variable[Int]], i: Int, inst: IndexedCorefInstance): Factor = {
    val variable = assignmentVariables(i)
    new Factor {
      val variables = IndexedSeq(variable)

      def logApply(assignments: Array[Int]) = {
        val j = assignments(0)
        weights dot inst.featuresFor(j, i)
      }
    }
  }

  def agreementFactor(propertyVariables: Array[Array[Variable[Int]]], assignmentVariables: Array[Variable[Int]], j: Int, i: Int, p: Int): Factor = {
    val p_j = propertyVariables(j)(p)
    val p_i = propertyVariables(i)(p)
    val a = assignmentVariables(i)
    val factor: Factor = new Factor {
      val variables = IndexedSeq(p_j, p_i, a)
      val w_agree = weights(propIndexer.featuresForProperties(p).agree)
      val w_disagree = weights(propIndexer.featuresForProperties(p).mismatch)

      def logApply(assignments: Array[Int]) = {
        if (assignments(2) != j) {
          0
        } else {
          val p_j_ass = assignments(0)
          val p_i_ass = assignments(1)
          val base = if (p_j_ass == p_i_ass) w_agree else w_disagree
          base + weights(propIndexer.featuresForProperties(p).pairs(p_j_ass)(p_i_ass))
        }
      }
    }
    factor
  }

  private def addIntoScale(v: DenseVector[Double], sv: SparseVector[Double], scale: Double) {
    if (scale != 0) {
      var i = 0
      val bi = sv.index
      val bd = sv.data
      val ad = v.data
      val aoff = v.offset
      val astride = v.stride
      while (i < sv.used) {
        ad(aoff + astride * bi(i)) += bd(i) * scale
        i += 1
      }
    }
  }

}

