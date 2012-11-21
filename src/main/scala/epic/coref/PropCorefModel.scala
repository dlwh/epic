package epic.coref
import epic.framework._
import breeze.util.Index
import breeze.linalg._
import breeze.inference.bp.{BeliefPropagation, Factor, Variable}
import collection.mutable.ArrayBuffer
import collection.immutable.BitSet
import epic.everything.DSpan
import epic.everything.models.{DocumentBeliefs, Property}

class PropCorefModel(properties: IndexedSeq[PropertyFeatures[_]], val featureIndex: Index[Feature]) extends Model[FeaturizedCorefInstance] with StandardExpectedCounts.Model {
  type Inference = PropCorefInference

  def initialValueForFeature(f: Feature) = 0.0

  def inferenceFromWeights(weights: DenseVector[Double]) = new PropCorefInference(featureIndex, properties, weights)
}

case class PropertyFeatures[T](prop: Property[T],
                               agree: Int,
                               mismatch: Int,
                               pairs: Array[Array[Int]]) {
  def arity = prop.arity
}

class PropCorefInference(index: Index[Feature],
                         properties: IndexedSeq[PropertyFeatures[_]],
                         weights: DenseVector[Double]) extends FullProjectableInference[FeaturizedCorefInstance, DocumentBeliefs] {
  type ExpectedCounts = StandardExpectedCounts[Feature]

  def emptyCounts = StandardExpectedCounts.zero(index)



  case class Marginal(marginals: BeliefPropagation.Beliefs,
                      agreementFactors: IndexedSeq[AgreementFactor],
                      assignmentFactors: IndexedSeq[Factor])


  def countsFromMarginal(inst: FeaturizedCorefInstance, marg: Marginal, aug: DocumentBeliefs): ExpectedCounts = {
    val expCounts = emptyCounts
    import marg._
    expCounts.loss = marginals.logPartition

    val clusterMins = BitSet.empty ++ inst.clusters.map(_.min)

    for(i <- 0 until assignmentFactors.length) {
      val arr = Array(0)
      if(clusterMins.contains(i))
        addIntoScale(expCounts.counts, inst.featuresFor(i, i), marginals.beliefs(i)(i))
      else for(j <- inst.clusterFor(i) if j < i) {
        arr(0) = j
        addIntoScale(expCounts.counts, inst.featuresFor(j, i), marginals.beliefs(i)(j))
      }
    }

    for(f <- agreementFactors) {
      f.tallyAgreeExpectedCounts(marginals, expCounts.counts)
    }

    expCounts
  }

  def goldMarginal(inst: FeaturizedCorefInstance, aug: DocumentBeliefs) = {
    val (assignmentVariables: Array[Variable[Int]], propertyVariables: Array[Array[Variable[Int]]]) = makeVariables(inst)

    // skip factor for 0, since it doesn't get assigned.
    val assignmentFactors = Array.tabulate[Factor](inst.numMentions) { i =>
      goldAssignmentFactor(assignmentVariables, i, inst, inst.clusterFor(i))
    }

    // this ends up being a little inefficient. We can actually
    // have 1 big factor with all properties of a given type,
    // and only visit certain assignments, but the bp framework
    // doesn't support that yet.
    // TODO: we can set contribution to exp(0) a prior and not need to sum over them... how to
    // do this?
    val agreementFactors = new ArrayBuffer[AgreementFactor]()
    for(c <- inst.clusters; i <- c; j <- c if j < i && j != 0; p <- 0 until properties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }

    val flattenedProp = propertyVariables.flatten.filter(_ ne null)

    val model = new breeze.inference.bp.Model(assignmentVariables ++ flattenedProp, assignmentFactors ++ agreementFactors)
    val bp = BeliefPropagation.infer(model)

    new Marginal(bp, agreementFactors, assignmentFactors) -> bp.logPartition
  }


  def project(v: FeaturizedCorefInstance, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = error("TODO")

  def baseAugment(v: FeaturizedCorefInstance): DocumentBeliefs = null

  def marginal(inst: FeaturizedCorefInstance, aug: DocumentBeliefs): (Marginal, Double) = {
    val (assignmentVariables: Array[Variable[Int]], propertyVariables: Array[Array[Variable[Int]]]) = makeVariables(inst)

    // skip factor for 0, since it doesn't get assigned.
    val assignmentFactors = Array.tabulate[Factor](inst.numMentions) { i =>
      assignmentFactor(assignmentVariables, i, inst)
    }

    // this ends up being a little inefficient. We can actually
    // have 1 big factor with all properties of a given type,
    // and only visit certain assignments, but the bp framework
    // doesn't support that yet.
    // TODO: we can set contribution to exp(0) a prior and not need to sum over them... how to
    // do this?

    val agreementFactors = new ArrayBuffer[AgreementFactor]()
    for(i <- 0 until inst.numMentions; j <- 0 to i; p <- 0 until properties.length) {
      agreementFactors += agreementFactor(propertyVariables, assignmentVariables, j, i, p)
    }

    val flattenedProp = propertyVariables.flatten.filter( _ ne null)

    val model = new breeze.inference.bp.Model(assignmentVariables ++ flattenedProp, assignmentFactors ++ agreementFactors)
    val bp = BeliefPropagation.infer(model)

    new Marginal(bp, agreementFactors, assignmentFactors) -> bp.logPartition
  }


  private def makeVariables(inst: FeaturizedCorefInstance): (Array[Variable[Int]], Array[Array[Variable[Int]]]) = {
    // assignmentVariables
    val aV: Array[Variable[Int]] = Array.tabulate(inst.numMentions + 1)(i => Variable(0 to i))
    // propertyVariables
    val pV: Array[Array[Variable[Int]]] = Array.tabulate(inst.numMentions, properties.length) { (i, pi) =>
      val assignment = inst.propertyValueFor(i, pi)
      if (assignment == -1)
        Variable(0 until properties(pi).arity)
      else
        Variable(assignment to assignment)
    }

    (aV, pV)
  }

  def decode(inst: FeaturizedCorefInstance, aug: DocumentBeliefs) : IndexedSeq[Set[DSpan]] = {
    val marg: Marginal = marginal(inst, aug)._1
    import marg._

    val links = Array.fill(inst.numMentions)(null: collection.mutable.BitSet)
    val trueClusters = ArrayBuffer[collection.mutable.BitSet]()
    for (i <- 0 until inst.numMentions) {
      val link = marginals.beliefs(i).argmax
      if (link == i) {
        links(i) = collection.mutable.BitSet(i)
        trueClusters += links(i)
      } else {
        links(i) = links(link)
        links(i) += i
      }
    }

    {
      for (cluster <- trueClusters) yield {
        for (i <- cluster) yield inst.mentions(i)
      }.toSet
    }.toIndexedSeq

  }

  def assignmentFactor(assignmentVariables: Array[Variable[Int]], i: Int, inst: FeaturizedCorefInstance): Factor = {
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
                           inst: FeaturizedCorefInstance,
                           cluster: BitSet): Factor = {
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


  def projectGold(v: FeaturizedCorefInstance, m: Marginal, oldAugment: DocumentBeliefs): DocumentBeliefs = {
    project(v, m, oldAugment)
  }

  case class AgreementFactor(p: Int, p_j: Variable[Int], p_i: Variable[Int], a_i: Variable[Int], j: Int) extends Factor {
    val w_agree = weights(properties(p).agree)
    val w_disagree = weights(properties(p).mismatch)

    val variables = IndexedSeq(p_j, p_i, a_i)

    def logApply(assignments: Array[Int]) = {
      if (assignments(2) != j) {
        0
      } else {
        val p_j_ass = p_j.domain.get(assignments(0))
        val p_i_ass = p_i.domain.get(assignments(1))
        val base = if (p_j_ass == p_i_ass) w_agree else w_disagree
        base + weights(properties(p).pairs(p_j_ass)(p_i_ass))
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
            expCounts(properties(p).agree) += prob
          else
            expCounts(properties(p).mismatch) += prob
          expCounts(properties(p).pairs(p_j_ass)(p_i_ass)) += prob
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
      while (i < sv.activeSize) {
        ad(aoff + astride * bi(i)) += bd(i) * scale
        i += 1
      }
    }
  }


}

