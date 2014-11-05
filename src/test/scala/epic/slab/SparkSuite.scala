package epic.slab

// Source stolen from tresata/spark-columnar

import scala.reflect.ClassTag

import org.apache.spark.{ SparkConf, SparkContext }
import org.apache.spark.rdd.RDD

object SparkSuite {
  lazy val sc = {
    val conf = new SparkConf(false)
      .setMaster("local")
      .setAppName("test")
      .set("spark.serializer", "org.apache.spark.serializer.KryoSerializer")
    new SparkContext(conf)
  }
}

trait SparkSuite {
  import SparkSuite._

  implicit def seqToRDD[X: ClassTag](seq: Seq[X]) = new {
    def toRDD: RDD[X] = sc.parallelize(seq)
  }

  implicit def rddToEqualizer[X](rdd: RDD[X]) = new {
    def sameElements(seq: Seq[X]): Option[String] = {
      val x = rdd.collect.toSeq
      if (x.groupBy(identity).mapValues(_.size) == seq.groupBy(identity).mapValues(_.size))
        None
      else
        Some("%s does not have same elements as %s".format(x, seq))
    }

    def sameElements(other: RDD[X]): Option[String] = sameElements(other.collect.toSeq)
  }
}
