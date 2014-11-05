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
}
