package epic.parser.models

import scala.collection.mutable.HashMap
import epic.dense.TanhTransform
import epic.dense.IdentityTransform
import epic.dense.AffineTransform
import breeze.linalg.DenseVector

object RandomTests {

  def main(args: Array[String]) {
    for (end <- 2 until 10) {
      for (split <- 1 until end) {
        for (begin <- 0 until split) {
          println(tetra(begin, split, end))
        }
      }
    }
    
//    val weightsTest = new DenseVector[Double](Array.tabulate(100000)(i => 0.01))
//    import breeze.linalg._
//    val mat = weightsTest(0 until 100000).asDenseMatrix.reshape(100, 1000, view = View.Require)
//    System.exit(0)
    
    
    
//    testNNSpeedDouble();
//    testNNSpeedFloat();
//  1806 x 100 x 87
  }
  
  
  private def tetra(begin: Int, split: Int, end: Int) = {
    (end.toLong * (end + 1) * (end + 2))/6 + ((split + 1) * split / 2 + begin)
  }
  
  def testNNSpeedDouble() {
    val neuralAffineTransform = new AffineTransform(87, 100, new TanhTransform(new AffineTransform(100, 1807, new IdentityTransform[DenseVector[Double]]())))
    val numParams = neuralAffineTransform.index.size
    val weights = new DenseVector[Double](Array.tabulate(numParams)(i => 0.01))
    val inputVector = new DenseVector[Double](Array.tabulate(1807)(i => 1.0))
    
    println(numParams + " feats")
    val time = System.nanoTime()
    for (i <- 0 until 5000) {
      weights(0) += 0.01
      val layer = neuralAffineTransform.extractLayer(weights)
      layer.activations(inputVector)
    }
    println((System.nanoTime() - time) / 1000000 + " millis")
  }
  
  
  
//  def testNNSpeedFloat() {
//    val neuralAffineTransform = new AffineTransformFloat(87, 100, new TanhTransformFloat(new AffineTransformFloat(100, 1807, new IdentityTransformFloat[DenseVector[Float]]())))
//    val numParams = neuralAffineTransform.index.size
//    val weights = new DenseVector[Float](Array.tabulate(numParams)(i => 0.01F))
//    val inputVector = new DenseVector[Float](Array.tabulate(1807)(i => 1.0F))
//    
//    println(numParams + " feats")
//    val time = System.nanoTime()
////    val layer = neuralAffineTransform.extractLayer(weights)
//    for (i <- 0 until 5000) {
//      weights(0) += 0.01F
//      val layer = neuralAffineTransform.extractLayer(weights)
//      layer.activations(inputVector)
//    }
//    println((System.nanoTime() - time) / 1000000 + " millis")
//  }
}