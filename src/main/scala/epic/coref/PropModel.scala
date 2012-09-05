package epic.coref
import epic.framework._
import breeze.util.Index
import breeze.linalg._
import breeze.numerics._
import java.util
import breeze.inference.bp.{BeliefPropagation, Factor, Variable}
import breeze.collection.mutable.TriangularArray
import collection.mutable.ArrayBuffer
import collection.immutable.BitSet

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
    val (assignmentVariables: Array[Variable[Int]], propertyVariables: Array[Array[Variable[Int]]]) = makeVariables(inst)

    val clusterFor = new Array[BitSet](inst.numMentions + 1)
    for(c <- inst.goldClusters; i <- c) {
      clusterFor(i) = c
    }

    // skip factor for 0, since it doesn't get assigned.
    val assignmentFactors = Array.tabulate[Factor](inst.numMentions) { i_minus_1 =>
      goldAssignmentFactor(assignmentVariables, i_minus_1 + 1, inst, clusterFor(i_minus_1 + 1))
    }

    // this ends up being a little inefficient. We can actually
    // have 1 big factor with all properties of a given type,
    // and only visit certain assignments, but the bp framework
    // doesn't support that yet.
    // TODO: we can set contribution to exp(0) a prior and not need to sum over them... how to
    // do this?
    val agreementFactors = new ArrayBuffer[AgreementFactor]()
    /*
    for(c <- inst.goldClusters; i <- c; j <- c if j < i && j != 0; p <- 0 until propIndexer.featuresForProperties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }
    */

    val flattenedProp = propertyVariables.flatten.filter(_ ne null)

    val model = new breeze.inference.bp.Model(assignmentVariables ++ flattenedProp, assignmentFactors ++ agreementFactors)
    val bp = BeliefPropagation.infer(model)

    val ll = bp.logPartition

    // actually get the expected counts!
    val expCounts = DenseVector.zeros[Double](propIndexer.index.size)

    val clusterMins = BitSet.empty ++ inst.goldClusters.map(_.min)

    for(i_minus_1 <- 0 until assignmentFactors.length) {
      val i = i_minus_1 + 1
      val arr = Array(0)
      if(clusterMins.contains(i))
        addIntoScale(expCounts, inst.featuresFor(0, i), bp.beliefs(i)(0))
      else for(j <- clusterFor(i) if j < i) {
        arr(0) = j
        addIntoScale(expCounts, inst.featuresFor(j, i), bp.beliefs(i)(j))
      }
    }

    /*
    for(f <- agreementFactors) {
      f.tallyAgreeExpectedCounts(bp, expCounts)
    }
    */


    new StandardExpectedCounts(ll, expCounts)
  }

  def guessCounts(inst: IndexedCorefInstance) = {
    val (assignmentVariables: Array[Variable[Int]], propertyVariables: Array[Array[Variable[Int]]]) = makeVariables(inst)

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

    val agreementFactors = new ArrayBuffer[AgreementFactor]()
    /*
    for(i <- 1 to inst.numMentions; j <- 1 until i; p <- 0 until propIndexer.featuresForProperties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }
    */

    val flattenedProp = propertyVariables.flatten.filter( _ ne null)

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
        addIntoScale(expCounts, inst.featuresFor(j, i), bp.beliefs(i)(j))
      }
    }

    /*
    for(f <- agreementFactors) {
      f.tallyAgreeExpectedCounts(bp, expCounts)
    }
    */

    new StandardExpectedCounts(ll, expCounts)
  }


  private def makeVariables(inst: IndexedCorefInstance): (Array[Variable[Int]], Array[Array[Variable[Int]]]) = {
    // assignmentVariables
    val aV: Array[Variable[Int]] = Array.tabulate(inst.numMentions + 1)(i => if (i == 0) Variable(0 to 0) else Variable(0 until i))
    // propertyVariables
    val pV: Array[Array[Variable[Int]]] = Array.tabulate(inst.numMentions + 1, propIndexer.featuresForProperties.length) {
      (i, pi) =>
        if (i == 0) null
        else {
          val assignment = inst.properties(i)(pi)
          if (assignment == -1)
            Variable(0 until propIndexer.featuresForProperties(pi).arity)
          else
            Variable(assignment to assignment)
        }
    }

    (aV, pV)
  }

  def decode(inst: IndexedCorefInstance) : IndexedSeq[Set[MentionCandidate]] = {
    val (assignmentVariables: Array[Variable[Int]], propertyVariables: Array[Array[Variable[Int]]]) = makeVariables(inst)

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
    for(i <- 1 to inst.numMentions; j <- 1 until i; p <- 0 until propIndexer.featuresForProperties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }

    val flattenedProp = propertyVariables.flatten.filter(_ ne null)

    val model = new breeze.inference.bp.Model(assignmentVariables ++ flattenedProp, assignmentFactors ++ agreementFactors)
    val bp = BeliefPropagation.infer(model)

    val links = Array.fill(inst.numMentions + 1)(null: collection.mutable.BitSet)
    val trueClusters = ArrayBuffer[collection.mutable.BitSet]()
    for (i <- 1 to inst.numMentions) {
      val link = bp.beliefs(i).argmax
      if (link == 0) {
        links(i) = collection.mutable.BitSet(i)
        trueClusters += links(i)
      } else {
        links(i) = links(link)
        links(i) += i
      }
    }

    {
      for (cluster <- trueClusters) yield {
        for (i <- cluster) yield inst.unindexed.mentions(i - 1)
      }.toSet
    }.toIndexedSeq

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


  def goldAssignmentFactor(assignmentVariables: Array[Variable[Int]], i: Int,
                           inst: IndexedCorefInstance, cluster: BitSet): Factor = {
    val variable = assignmentVariables(i)
    val i_is_min = cluster.min == i
    new Factor {
      val variables = IndexedSeq(variable)

      def logApply(assignments: Array[Int]) = {
        val j = assignments(0)
        weights dot inst.featuresFor(j, i)
      }

      override def foreachAssignment(f: (Array[Int]) => Any) {
        val arr = new Array[Int](1)
        if(i_is_min)
          f(arr)
        else for(j <- cluster if j < i) {
          arr(0) = j
          f(arr)
        }
      }
    }
  }

  case class AgreementFactor(p: Int, p_j: Variable[Int], p_i: Variable[Int], a_i: Variable[Int], j: Int) extends Factor {
    val w_agree = weights(propIndexer.featuresForProperties(p).agree)
    val w_disagree = weights(propIndexer.featuresForProperties(p).mismatch)

    val variables = IndexedSeq(p_j, p_i, a_i)

    def logApply(assignments: Array[Int]) = {
      if (assignments(2) != j) {
        0
      } else {
        val p_j_ass = p_j.domain.get(assignments(0))
        val p_i_ass = p_i.domain.get(assignments(1))
        val base = if (p_j_ass == p_i_ass) w_agree else w_disagree
        base + weights(propIndexer.featuresForProperties(p).pairs(p_j_ass)(p_i_ass))
      }
    }


    def tallyAgreeExpectedCounts(bp: BeliefPropagation.Beliefs, expCounts: DenseVector[Double]) {
      val marg = bp.factorMarginalFor(this)
      marg.foreachAssignment { assignments =>
        if(assignments(2) == j) {
          val p_j_ass = p_j.domain.get(assignments(0))
          val p_i_ass = p_i.domain.get(assignments(1))

          val prob = marg(assignments)
          if (p_j_ass == p_i_ass)
            expCounts(propIndexer.featuresForProperties(p).agree) += prob
          else
            expCounts(propIndexer.featuresForProperties(p).mismatch) += prob
          expCounts(propIndexer.featuresForProperties(p).pairs(p_j_ass)(p_i_ass)) += prob
        }
      }
    }
  }

  def agreementFactor(propertyVariables: Array[Array[Variable[Int]]], assignmentVariables: Array[Variable[Int]], j: Int, i: Int, p: Int) = {
    val p_j = propertyVariables(j)(p)
    val p_i = propertyVariables(i)(p)
    val a = assignmentVariables(i)

    AgreementFactor(p, p_j, p_i, a, j)
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

