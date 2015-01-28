package epic.corefdense

object DeepUtils {
  
  def addToGradient(feats: Array[Int], scale: Double, gradient: Array[Double], offset: Int) {
    var i = 0;
    while (i < feats.size) {
      gradient(feats(i) + offset) += scale;
      i += 1;
    }
  }
  
  def dotProductOffset(feats: Array[Int], weights: Array[Double], offset: Int) = {
    var score = 0.0
    var i = 0;
    while (i < feats.size) {
      score += weights(feats(i) + offset)
      i += 1
    }
    score
  }
}