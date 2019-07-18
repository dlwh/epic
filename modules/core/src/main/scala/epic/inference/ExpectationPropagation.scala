package epic.inference

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Dirichlet, Bernoulli, Gaussian}

/**
 *
 * @author dlwh
 */
class ExpectationPropagation[F,Q <: AnyRef](project: (Q,F)=>(Q,Double), criterion: Double = 1E-4)(implicit qFactor: Q <:<Factor[Q]) {
  case class State(f_~ : IndexedSeq[Q], q: Q, prior: Q, partitions: IndexedSeq[Double]) {
    lazy val logPartition = f_~.foldLeft(prior)(_*_).logPartition + partitions.sum
  }

  def inference(prior: Q, f: IndexedSeq[F], initialF_~ : IndexedSeq[Q]):Iterator[State] = {
    val lastQ: Q = initialF_~.foldLeft(prior)(_ * _)

    val initPartitions = IndexedSeq.fill(f.length)(Double.NegativeInfinity)

    // pass through the data
    val it:Iterator[State] = new Iterator[State] {
      var cur = State(initialF_~, lastQ, prior, initPartitions)
      var consumed = true

      def hasNext = !consumed || {
        val next =  f.indices.iterator.foldLeft(cur) { (state,i) =>
          val State(f_~, q, _, partitions) = state
          val fi = f(i)
          val fi_~ = f_~(i)
          val q_\  = q / fi_~
          val (new_q, new_partition) = project(q_\ , fi)
          val newF_~ = f_~.updated(i,new_q / q_\)
          State(newF_~, new_q, prior, partitions.updated(i, new_partition))
        }
        val hasNext = (cur.q eq lastQ) || !next.q.isConvergedTo(cur.q, criterion)
        consumed = !hasNext
        cur = next
        hasNext
      }

      def next() = {
        if (consumed) hasNext
        consumed = true
        cur
      }
    }

    it
  }

}

object ExpectationPropagation extends App {
  val prop = 0.9
  val mean = 2
  val gen = for {
    a <- new Bernoulli(prop)
    x <- Gaussian(I(a) * mean,3)
  } yield x

  val data = gen.sample(5000)

  case class ApproxTerm(s: Double = 0.0, b: DenseVector[Double] = DenseVector.zeros(2)) extends Factor[ApproxTerm] { f1 =>
    def logPartition = s + breeze.numerics.lbeta(b)

    def *(f: Double) = copy(s = s + f)
    def *(f2: ApproxTerm) = {
      ApproxTerm(f1.s + f2.s, f1.b + f2.b)
    }
    def /(f2: ApproxTerm) = {
      ApproxTerm(f1.s - f2.s, f1.b - f2.b)
    }

    def apply(a: Double)  = {
      0.0 // TODO
    }

    def isConvergedTo(f: ApproxTerm, diff: Double) = {
      norm(b - f.b, 2.0) <= diff
    }
  }

  def likelihood(x: Double):DenseVector[Double] = {
    DenseVector(Gaussian(0,3).pdf(x), Gaussian(mean,3).pdf(x))
  }

  def solve(old: DenseVector[Double], target: DenseVector[Double]) = {
    val guess = copy(old)
    for(i <- 0 until 20) {
      val t2 = target + digamma(guess.sum)
      for(i <- 0 until 5) {
        guess -= ((digamma(guess) - t2) :/ (( digamma(guess + 1E-4) - digamma(guess))/1E-4))
      }
    }
    guess
  }


  def project(q: ApproxTerm, x: Double): (ApproxTerm, Double) = {
    val like = likelihood(x)
    val target = digamma(q.b) - digamma(q.b.sum) +  (like / (like dot q.b)) - 1/q.b.sum
    val normalizer:Double = likelihood(x) dot normalize(q.b, 1)
    val mle = solve(q.b, target)
    assert(!normalizer.isNaN,(mle,q.b,like,normalize(q.b,1)))

    ApproxTerm(-lbeta(mle+1.0), mle) -> math.log(normalizer)
  }

  val ep = new ExpectationPropagation({project _}, 1E-8)
  for( state <- ep.inference(ApproxTerm(0.0,DenseVector.ones(2)), data, Array.fill(data.length)(ApproxTerm())) take 20) {
    println(state.logPartition, state.q)
    assert(!state.logPartition.isNaN, state.q.s + " " + state.q.b)
  }

}

