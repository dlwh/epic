package epic.slab

import org.apache.spark.SparkContext._
import org.scalatest.FunSpec
import epic.trees.Span
import shapeless._
import ops.hlist._

class SparkIntegration extends FunSpec with SparkSuite {
  val sentences = Vector(Sentence(Span(0,0)))
  val tokens = Vector(Token(Span(0,0)))
  val slab = Slab("", sentences :: HNil)
  describe("Slab") {
    it("should transport around in spark") {
      val rdd = SparkSuite.sc.parallelize(List(slab))
      assert(rdd.collect()(0).content == "")
    }
    it("should work with select") {
      val rdd = SparkSuite.sc.parallelize(List(slab)).map(_.select[Sentence])
      assert(rdd.collect()(0) == sentences)
    }
    it("should work with selectMany") {
      val rdd = SparkSuite.sc.parallelize(List(slab)).map(_.selectMany[Vector[Sentence] :: HNil])
      assert(rdd.collect()(0).select[Vector[Sentence]] == sentences)
    }
    it("should work with adder") {
      // Don't reference stuff from outer scope in functions which are
      // sent over the wire.
      val t = tokens
      val rdd = SparkSuite.sc.parallelize(List(slab)).map(_.add(t))
      assert(rdd.collect()(0).select[Token] == tokens)
    }
  }
}
