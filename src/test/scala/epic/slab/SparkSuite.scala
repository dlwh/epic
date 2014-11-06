package epic.slab

// Source stolen from tresata/spark-columnar

import org.apache.spark.{ SparkConf, SparkContext }

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
