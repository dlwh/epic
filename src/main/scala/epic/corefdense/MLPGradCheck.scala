package epic.corefdense

import breeze.linalg._
import edu.berkeley.nlp.futile.fig.basic.Indexer

object MLPGradCheck {

  def main(args: Array[String]) {
    
    // NON-TRANSPOSED IS CORRECT (which makes sense, because it's a datum per
    // row in the python code)
    val train = Array(Array(0.8, -0.6), Array(0.5, 0.9))
    val trainLabels = Array[Byte](0.toByte, 1.toByte)
    val nnExs = (0 until train.size).map(i => new NNExampleImpl(train(i), trainLabels(i)))
    
    val transform = SimpleNNEpic.makeDeepTransform(2, 2, 1, 2, "tanh")
//    val transform = SimpleNNEpic.makeDeepTransform(2, 2, 1, 2, "relu")
    
    val vacuousIndexer = new Indexer[Byte];
    (0 to 1).map(idx => vacuousIndexer.add(idx.toByte))
    val nn: SimpleNNEpic[Byte] = new SimpleNNEpic(transform, vacuousIndexer)
    
    
    
//    W1_values = np.array([[-2,-1],[1,2]])
//    b1_values = np.array([1.2,1.4])
//    W2_values = np.array([[-1.5,-0.5],[0.5,1.5]])
//    b2_values = np.array([1.6,1.8])
    
    // TRANSPOSED FROM PYTHON
//    val w1 = DenseVector(Array(-2.0, 1.0, -1.0, 2.0))
//    val w2 = DenseVector(Array(-1.5, 0.5, -0.5, 1.5))
    // NON-TRANSPOSED
    val w1 = DenseVector(Array(-2.0, -1.0, 1.0, 2.0))
    val w2 = DenseVector(Array(-1.5, -0.5, 0.5, 1.5))
    val b1 = DenseVector(Array(1.2, 1.4))
    val b2 = DenseVector(Array(1.6, 1.8))
    
    val initialWeights = DenseVector.vertcat(w2, b2, w1, b1)
    println(initialWeights.data.size)
    
    val gradient = Array.tabulate(initialWeights.size)(i => 0.0)
    for (nnEx <- nnExs) {
      nn.accumulateGradientAndComputeObjective(nnEx, initialWeights.data, gradient)
    }
    // Negate and rescale by batch size to match python
    (0 until gradient.size).foreach(i => gradient(i) *= -0.5)
    println(gradient.toSeq)
  }
}